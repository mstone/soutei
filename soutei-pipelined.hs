-- Soutei Policy Decision Service
-- It is intended to communice with a pep-auth-filter, an authorization
-- filter for a web server.
--
-- The interface is straightforward, via stdin/stdout. The filter 
-- writes all attributes as name-value-pairs: 
--    name1 \n value1 \n name2 \n value2 \n ...
-- Two \n in a row signify the end of input. 
-- All attribute names and values are opaque to us, except one.
-- If there is an attribute named 'resource', the corresponding value
-- must be a URL, such as http://host/a/b/c.d
-- The http://host part is optional. We add a sequence of attributes:
--    resource-below = http://host/a
--    resource-below = http://host/a/b
--
-- We reply with either T\n or F\n. We also have an option to close
-- the connection and exit, or just die.
-- We accept SIGUSR1 so to reload the policy database. See the usage
-- message below.

module Main (main) where

import Soutei.Assertions (Assertions, fromDataDirWithActions,loadSysCtx, query)
import Soutei.Syntax
import Soutei.Soutei as Soutei

import Control.Monad.Trans
import Data.Char                     ( toLower, isSpace, isAlphaNum, ord )
import Data.List (scanl, init)
import Numeric (showHex)

import Data.IORef
import System.IO (hSetBuffering,stderr,BufferMode(..), Handle, hGetLine,
		  hPutStr, hPutChar, stdin, stdout, hPutStrLn, hFlush)
import System.Posix.Signals              as Signals

import System.Time                   ( getClockTime )
import System.Environment (getArgs)
import System.Directory as Dir
import System.Exit
import System.IO.Error as IO


version = "$Id: soutei-pipelined.hs 1940 2008-02-14 05:26:56Z oleg.kiselyov $"

usage = unlines (
 ("Soutei-pipelined " ++ version) :
 "Usage: soutei-pipelined DATA-DIR":
 "Start the Soutei server.":
 "  DATA-DIR          a directory with policy":
 " The directory should be owned by a dedicated user (e.g., root)":
 " not being writable to this process, contain a read-only file":
 " named system (the root of the policy) and no file named":
 " application.":
 "Send SIGUSR1 to reload the data.":
 [])


useError msg = do
  note [msg,"\n",usage]
  exitWith (ExitFailure 64)

startError e = note ["Exception: ", show e] >> exitWith (ExitFailure 66)

main = do
  -- setTimeZoneGMT
  hSetBuffering stderr LineBuffering
  noteDate ["===== Soutei Authorizer server: ", version, "\n\n"]
  -- Signals.installHandler Signals.sigPIPE Signals.Ignore Nothing
  getArgs >>= main'

data Policy = Policy{policy_data        :: Assertions,
		     policy_reload      :: IORef Bool,
		     policy_load_action :: IO Assertions}

main' [dataDir] = do
 let load_action = load_policy dataDir `catch` startError
 policy <- load_action
 reloadFlag <- newIORef False
 Signals.installHandler Signals.userDefinedSignal1 
    (Signals.Catch $ writeIORef reloadFlag True) Nothing
 loop (Policy policy reloadFlag load_action)
main' _ = useError "Exactly one arg is required."

-- The main processing loop
loop :: Policy -> IO ()
loop policy = do
   note ["Listening"]
   attrs <- getAttrs stdin
   logRequest attrs
   policy <- check_reload policy
   res <- authorizer (policy_data policy) attrs
   note ["Decision: ", show res]
   hPutStrLn stdout (if res then "T" else "F")
   hFlush stdout
   loop policy
   note ["\nSuccessful exit"]
 where
  check_reload policy = do
   reload <- readIORef (policy_reload policy)
   if reload then do
		  p <- writeIORef (policy_reload policy) False >>
	               policy_load_action policy
		  return policy{policy_data = p}
      else return policy

type RequestInfo = [(String,String)]

-- Read the sequence of name-value pairs
getAttrs :: Handle -> IO [(String,String)]
getAttrs h = get []
 where
 get acc = do
	   name <- hGetLine h
	   if name == "" then return acc
	      else do
		   value <- hGetLine h
		   get ((name,value):acc)

-- Log the received request. We do that early before starting the parsing,
-- so we can display the (potentially erroneous) request data before
-- we begin reporting errors about them.
-- Note that the type of this function says EMonadIO m -- rather than
-- any MonadCGI. That is, this function assuredly creates no CGI
-- output!
logRequest :: MonadIO m => RequestInfo -> m ()
logRequest req = 
  noteDate ["---> New AuthRequest request\n", show req]


-- The main authorizer module
authorizer :: Assertions -> RequestInfo -> IO Bool
authorizer policies req = do
 let auth_req = read_analyze_req req
 soutei_query policies auth_req

data AuthRequest = AuthRequest {areq_verb :: String,
				areq_uri  :: String,
				areq_atts :: [(String,String)]}

-- Build the request for Soutei
read_analyze_req :: RequestInfo -> AuthRequest
read_analyze_req req = AuthRequest {areq_verb = "Access",
				    areq_uri  = "service",
				    areq_atts = ext_req}
 where
 ext_req = maybe req add_to_req $ lookup "resource" req
 add_to_req uri = map (\v -> ("resource-below",v)) (split_uri uri) ++ req

-- Split the URI at resource boundaries (as described in the commenst above)
split_uri :: String -> [String]
split_uri uri = check $ splitAll '/' uri
 where
 check (schema:"":host:components@(_:_)) = 
     map ((schema++'/':'/':host)++) $ build components
 check (schema:"":host:[]) = []
 check ("":components@(_:_)) = build components
 check components@(_:_) = build components
 check _ = []
 build = tail . scanl (\z a -> z ++ '/':a) "" . init

t_split_uri1 = map split_uri [
  "", "http://localhost", "http://localhost/",
  "http://localhost/a", "http://localhost/a/b", "http://localhost/a/b/", 
  "/", "/a/b/c", "a/b/c", "a", "a/", "a/b/c/"]
{-			      
 [[],[],[],
  [],["http://localhost/a"],["http://localhost/a","http://localhost/a/b"],
  [],["/a","/a/b"],["/a","/a/b"],[],["/a"],["/a","/a/b","/a/b/c"]]
-}


-- | Split a list in many on a particular element:
--
-- > splitAll 'm' "simply marvelous"    == ["si", "ply ", "arvelous"]
--
splitAll :: Eq a => a -> [a] -> [[a]]
splitAll c s = case break (==c) s of
  ([], [])  -> []
  (x,  [])  -> [x]
  (x,  [_]) -> [x, []]
  (x,  _:y) -> x : splitAll c y


soutei_query :: Assertions -> AuthRequest -> IO Bool
soutei_query policy areq = do
  let goal = Soutei.goal "may" [SString $ areq_verb areq,
				SString $ areq_uri areq]
  facts <- mapM attr_to_fact $ areq_atts areq
  note ["Beginning the query; goal: ", show goal, "\n",
	"facts: ", show (areq_atts areq)]
  query runLimit policy facts goal

runLimit = Just 10000

attr_to_fact (key,val) = atomToFact $ Soutei.fact key [SString val]

-- Loading the policies and checking them
load_policy :: FilePath -> IO Assertions
load_policy data_dir = do
 note ["Loading policies from directory ",data_dir]
 ifnotA (doesDirectoryExist data_dir) 
	(fail "Data directory does not exist")
 ifnotA (getPermissions data_dir >>= \p -> return $
	 Dir.readable p && Dir.searchable p && (not $ Dir.writable p))
        (fail "Data directory has wrong permissions")
 let sys_ctx_file = ctxFilename data_dir sysCtx
 ifnotA (getPermissions sys_ctx_file >>= \p -> return $
	 Dir.readable p && (not $ Dir.writable p))
        (fail "System policy is writable: wrong")
 ifnotA (fmap not $ doesFileExist (ctxFilename data_dir appCtx))
        (fail "application file should not exist")

 policies <- fromDataDirWithActions logErr readA writeA
 loadSysCtx sys_ctx_file policies
 return policies
 where
 writeA ctx content = return ()
 readA ctx = let ctxFile = ctxFilename data_dir ctx in do
  b <- doesFileExist ctxFile
  if b
    then (return . Just) =<< readFile ctxFile
    else return Nothing
 logErr err = note [if isUserError err then ioeGetErrorString err
                                       else show err]

ifnotA :: Monad m => m Bool -> m () -> m ()
ifnotA testA ac = do
  f <- testA
  if f then return () else ac

ctxFilename :: FilePath -> Const -> FilePath
ctxFilename dataDir ctx = dataDir ++ "/" ++ encode (show ctx)

encode :: String -> String
encode = concatMap encode' where
  encode' ch | isAlphaNum ch || ch `elem` "!#$&'()+,-.;=@_" = [ch]
             | otherwise = '%' : showHex (ord ch) ";"


-- | Convenience function for logging, into stderr
note          :: MonadIO m => [String] -> m ()
note msgs     = liftIO (mapM_ (hPutStr stderr) msgs >> hPutChar stderr '\n')

-- The same but prints the date first
noteDate      :: MonadIO m => [String] -> m ()
noteDate msgs = do 
  t <- liftIO getClockTime
  note $ show t : ": " : msgs
