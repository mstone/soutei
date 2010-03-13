-- The regression test suite for Soutei along the lines of a real
-- demo (given in June 2005).
-- The main entry point runs the tests using the linked-in
-- Soutei engine.
-- The alternative entry point runs the tests against the Metcast server
-- configured to communicate with a Soutei server for
-- authorization decisions. The alternative entry point emulates the
-- demonstration on June 2005.

module Main where

import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Network.Socket
import System.Environment
import System.IO
import System.IO.Error as IO

import Soutei.Assertions (Assertions, emptyF, loadSysCtxF, putCtxF, queryF)
import Soutei.Soutei

-- The main entry point
-- The two arguments are the repetition counts (to test the performamce)
-- For regression tests, pass 1 as the two arguments
main = getArgs >>= main'
 where main' [reps, queryReps] =
	  do
	   putStrLn $ "Soutei Regression tests"
           runLocalN (read reps) (read queryReps) souteiDemo
       main' _ = putStrLn $ 
        "Two arguments are required, "++
        "the repetition counts `rep' and `queryReps'\n"++
	"If not doing load testing, invoke this code as\n"++
	"\t./soutei-metcast-tests 1 1\n\n"
		 

-- The alternative entry point: run the metcast demo
main_metcast = do
	       putStrLn "Ruinning Metcast Channels demo"
	       metcastDemoPrep
	       metcastDemo

-- Testing Soutei running as a separate server on a specified port
maio = getArgs >>= main'
 where main' [port, reps] =
          replicateM_ (read reps)
                      (runRemote (fromIntegral (read port :: Int)) souteiDemo)
       main' _ = putStrLn $ 
        "Two arguments are required\n" ++
        "port to communicate with Soutei at localhost\n" ++
        "repetition count for load testing, for example, 1\n\n"


samSysadminAssertion = unlines [
        "may-admin(create) :-                                                 ",
        "        application says this-user(cam.create@HOST).               ",
        "may-admin(create) :-                                                 ",
        "        application says this-user(cari.create@HOST).              ",
        ""]

camCreateAssertion = unlines [
        "; grants himself every access right                                  ",
        "may(channel, ?access) :-                                             ",
        "        system says access-right(?access),                           ",
        "        application says this-user(cam.create@HOST).               ",
        "; Delegate the conditional read permission to don.delegate           ",
        "may(channel, read) :-                                                ",
        "        application says this-user(?U),                              ",
        "        application says user-citizenship(US),                       ",
        "        don.delegate@HOST says may(channel, read).                 ",
        ""]

donDelegateAssertion = unlines [
        "; permits everything to everybody                                    ",
        "may(channel, read).                                                  ",
        ""]

edEmergencyAssertion = unlines [
        "may-admin(create).                                                   ",
        "may(channel, read).                                                  ",
        ""]

-- This follows the demo scenario, with some additional checks along the
-- way.

souteiDemo :: Soutei -> IO ()
souteiDemo (Soutei query newAssertion) = do
  -- We only have the initial assertion

  -- cam.create can't create channels
  let goal = "may-admin channel create"
      facts = ["this-user cam.create@HOST"]
  assertNot (query "req-init-1" goal facts)

  -- only sam.sysadmin can read logs
  let goal = "may channel read"
      facts = ["this-channel-owner ADMH sys",
               "this-user sam.sysadmin@HOST"]
  assert (query "req-init-2" goal facts)

  let goal = "may channel read"
      facts = ["this-channel-owner ADMH sys",
               "this-user *"]
  assertNot (query "req-init-3" goal facts)

  -- some standard channels are public
  let goal = "may channel write"
      facts = ["this-channel-owner TESTIMG sys",
               "this-user *"]
  assert (query "req-init-4" goal facts)

  let goal = "may channel read"
      facts = ["this-channel-owner TESTIMG sys",
               "this-user *"]
  assert (query "req-init-5" goal facts)

  -- sam.sysadmin sends his policy

  newAssertion "req-sam-sysadmin-0" "sam.sysadmin@HOST" samSysadminAssertion

  -- cam.create creates DustSensorData, but sets no policy.

  -- cam.create is allowed to create new channels
  let goal = "may-admin channel create"
      facts = ["this-user cam.create@HOST"]
  assert (query "req-dust-init-1" goal facts)

  -- nobody can access his channel
  let goal = "may channel write"
      facts = ["this-channel-owner DustSensorData cam.create@HOST",
               "this-user sam.sysadmin@HOST"]
  assertNot (query "req-dust-init-2" goal facts)

  -- even himself
  let goal = "may channel write"
      facts = ["this-channel-owner DustSensorData cam.create@HOST",
               "this-user cam.create@HOST"]
  assertNot (query "req-dust-init-3" goal facts)

  -- cam.create sends his policy

  newAssertion "req-push-cam-create" "cam.create@HOST" camCreateAssertion

  -- cam.create can see his channel
  let goal = "may channel write"
      facts = ["this-channel-owner DustSensorData cam.create@HOST",
               "this-user cam.create@HOST"]
  assert (query "req-dust-cam-create-1" goal facts)

  let goal = "may channel read"
      facts = ["this-channel-owner DustSensorData cam.create@HOST",
               "this-user cam.create@HOST"]
  assert (query "req-dust-cam-create-2" goal facts)

  -- don.delegate can't see
  let goal = "may channel read"
      facts = ["this-channel-owner DustSensorData cam.create@HOST",
               "this-user don.delegate@HOST"]
  assertNot (query "req-dust-cam-create-3" goal facts)

  -- don.delegate sends his policy

  newAssertion "req-push-don-delegate" "don.delegate@HOST" donDelegateAssertion

  -- don.delegate can read but not write
  let goal = "may channel read"
      facts = ["this-channel-owner DustSensorData cam.create@HOST",
               "this-user don.delegate@HOST",
               "user-citizenship US"]
  assert (query "req-dust-don-delegate-1" goal facts)

  let goal = "may channel write"
      facts = ["this-channel-owner DustSensorData cam.create@HOST",
               "this-user don.delegate@HOST",
               "user-citizenship US"]
  assertNot (query "req-dust-don-delegate-2" goal facts)

  -- joe.canuck is a Canadian, so can't read
  let goal = "may channel read"
      facts = ["this-channel-owner DustSensorData cam.create@HOST",
               "this-user joe.canuck@HOST",
               "user-citizenship CA"]
  assertNot (query "req-dust-don-delegate-3" goal facts)

  -- Emergency policy

  newAssertion "req-push-ed-emergency" "ed.emergency@HOST" edEmergencyAssertion

  -- Anyone can create a channel
  let goal = "may-admin channel create"
      facts = ["this-user joe.canuck@HOST",
               "user-citizenship US"]
  assert (query "req-ed-emergency-1" goal facts)

  -- joe.canuck can now see cam.create's channels
  let goal = "may channel read"
      facts = ["this-channel-owner DustSensorData cam.create@HOST",
               "this-user joe.canuck@HOST",
               "user-citizenship CA"]
  assert (query "req-dust-ed-emergency-2" goal facts)

  -- Emergency over

  newAssertion "req-push-ed-emergency" "ed.emergency@HOST" ""

  -- joe.canuck can no longer read cam.create's channel
  let goal = "may channel read"
      facts = ["this-channel-owner DustSensorData cam.create@HOST",
               "this-user joe.canuck@HOST",
               "user-citizenship CA"]
  assertNot (query "req-dust-don-delegate-3" goal facts)

  putStrLn "Regression tests done\n"

 `finally` do
  newAssertion "req-drop-sam-sysadmin" "sam.sysadmin@HOST" ""
  newAssertion "req-drop-cam-create" "cam.create@HOST" ""
  newAssertion "req-drop-don-delegate" "don.delegate@HOST" ""
  newAssertion "req-drop-ed-emergency" "ed.emergency@HOST" ""

-- The same demo, except talking to Metcast (configured to use Soutei)
-- instead of Soutei.  The users must be configured in Apache.

metcastDemoPrep = do
  deleteChannel samSysadmin "TESTIMG"
  assertEq 201 (createChannel samSysadmin "TESTIMG")

metcastDemo = do
  -- We only have the initial assertion

  -- nobody can admin channels
  assertEq 403 (createChannel camCreate "DustSensorData")

  -- only sam.sysadmin can read logs
  assertEq 200 (retrieve "req-init-2" samSysadmin "ADMH")
  assertEq 304 (retrieve "req-init-3" anon "ADMH")

  -- some standard channels are public
  assertEq 201 (shove anon "TESTIMG" "text/plain" "hello world")
  assertEq 200 (retrieve "req-init-5" anon "TESTIMG")

  -- sam.sysadmin sends his policy

  shoveAssertion samSysadmin samSysadminAssertion

  -- cam.create creates DustSensorData, but sets no policy.

  -- cam.create is allowed to create new channels
  assertEq 201 (createChannel camCreate "DustSensorData")

  -- nobody can access his channel
  assertEq 400 (shove samSysadmin "DustSensorData" "text/plain" "hello world")

  -- even himself
  assertEq 400 (shove camCreate "DustSensorData" "text/plain" "hello world")

  -- cam.create sends his policy

  shoveAssertion camCreate camCreateAssertion

  -- cam.create can see his channel
  assertEq 201 (shove camCreate "DustSensorData" "text/plain" "hello world")
  assertEq 200 (retrieve "req-dust-cam-create-1" camCreate "DustSensorData")

  -- don.delegate can't see it
  assertEq 304 (retrieve "req-dust-cam-create-3" donDelegate "DustSensorData")

  -- don.delegate sends his policy

  shoveAssertion donDelegate donDelegateAssertion

  -- don.delegate can read but not write
  assertEq 200 (retrieve "req-dust-don-delegate-1" donDelegate "DustSensorData")
  assertEq 400 (shove donDelegate "DustSensorData" "text/plain" "hello world")

  -- joe.canuck is a Canadian, so can't read
  assertEq 304 (retrieve "req-dust-don-delegate-3" joeCanuck "DustSensorData")

  -- Emergency policy

  shoveAssertion edEmergency edEmergencyAssertion

  -- Anyone can create a channel
  assertEq 201 (createChannel joeCanuck "HockeyScores")

  -- joe.canuck can now see cam.create's channel
  assertEq 200 (retrieve "req-dust-ed-emergency-2" joeCanuck "DustSensorData")

  -- Emergency over

  shoveAssertion edEmergency ""

  -- joe.canuck can no longer read cam.create's channel
  assertEq 304 (retrieve "req-dust-don-delegate-3" joeCanuck "DustSensorData")

 `finally` do
  deleteChannel samSysadmin "DustSensorData"
  deleteChannel samSysadmin "HockeyScores"
  shoveAssertion samSysadmin ""
  shoveAssertion camCreate ""
  shoveAssertion donDelegate ""
  shoveAssertion edEmergency ""

-- Soutei helpers

data Soutei = Soutei {
                query :: String -> String -> [String] -> IO Bool,
                newAssertion :: String -> String -> String -> IO ()
              }

runLocal = runLocalN 1 1

runLocalN reps queryReps m = do
  r <- newIORef emptyF
  modifyIORefIO r (loadSysCtxF "soutei-metcast-demo-init.txt")
  replicateM_ reps (m (Soutei (queryLocal queryReps r) (newAssertionLocal r)))

queryLocal reps r id goal facts = readIORef r >>= \idx -> do
  let goal' = mkAtom (words goal)
      facts' = map (mkAtom . words) facts
  ret <- queryF t idx facts' goal'
  replicateM_ (reps-1) (assert (liftM (ret ==) (queryF t idx facts' goal')))
  return ret
 where
  mkAtom (pred:args) =  let pred' = Pred pred (length args)
                            args' = map (Val . SString) args
                        in  Atom NoCtx pred' args'
  t = Just 1000

newAssertionLocal r id ctx s =
  let s' = case s of
            "" -> Nothing;
            _  -> Just s
  in  modifyIORefIO r (putCtxF "test" (SString ctx) s')

-- Soutei should be already running with just the initial assertions
-- soutei-metcast-demo-init.txt
runRemote port m = m (Soutei (queryRemote port) (newAssertionRemote port))

soutei port = sockReq (liftM (SockAddrInet port) (inet_addr "127.0.0.1"))

queryRemote :: PortNumber -> String -> String -> [String] -> IO Bool
queryRemote port id goal facts = do
    let req = ("(" ++ id ++ " query (" ++ goal ++ ")" ++
                  concatMap (\f -> " (" ++ f ++ ")") facts ++ ")\n")
    r <- soutei port req
    fromMaybe (fail "Unexpected reply from Soutei")
              (lookup r [("(" ++ id ++ " #t)\n", return True),
                         ("(" ++ id ++ " #f)\n", return False)])

newAssertionRemote :: PortNumber -> String -> String -> String -> IO ()
newAssertionRemote port id ctx assertion = do
    let req = ("(" ++ id ++ " assertion " ++ q ctx ++ " " ++
                  q assertion ++ ")\n")
    r <- soutei port req
    fromMaybe (fail "Unexpected reply from Soutei")
              (lookup r [("(" ++ id ++ " #t)\n", return ())])

q :: String -> String
q s = "\"" ++ concatMap q' s ++ "\"" where
  q' '"'  = "\\\""
  q' '\\' = "\\\\"
  q' c    = [c]

-- metcast helpers

metcastAddr = liftM (SockAddrInet 80) (inet_addr "127.0.0.1")
metcastURL  = "/metcast"
serverURL   = metcastURL ++ "/metcast.cgi"
takerURL    = metcastURL ++ "/metcast.cgi"

-- (base64 of "username:", citizenship)
type User = (String, Maybe String)
samSysadmin   = ("c2FtLnN5c2FkbWluQEhPU1Q6", Just "US")
edEmergency   = ("ZWQuZW1lcmdlbmN5QEhPU1Q6", Just "US")
camCreate     = ("Y2FtLmNyZWF0ZUBIT1NUOg==", Just "US")
cariCreate    = ("Y2FyaS5jcmVhdGVASE9TVDo=", Just "US")
donDelegate   = ("ZG9uLmRlbGVnYXRlQEhPU1Q6", Just "US")
joeCanuck     = ("am9lLmNhbnVja0BIT1NUOg==", Just "CA")
anon          = ("Kjo=", Nothing)

retrieve :: String -> User -> String -> IO Int
retrieve id user channel = do
  let body = "(" ++ id ++ " (products (Channel " ++ channel ++ ")))"
  http "POST" serverURL user "text/x-mbl" body

shoveAssertion :: User -> String -> IO ()
shoveAssertion user assertion = do
  let body = "<authorize><![CDATA[" ++ assertion ++ "]]></authorize>"
  s <- shove user "ADM" "text/xml" body
  if (s == 201) then return ()
                else fail ("Failed to shove assertion: " ++ show s)

createChannel :: User -> String -> IO Int
createChannel user channel = do
  let body = unlines [
        "<create>",
        "<channel Name='" ++ channel ++ "'",
        "         Title='" ++ channel ++ "'",
        "         MaxLen='1'>",
        "  <attr Name='=mime-type'/>",
        "</channel></create>"]
  shove user "ADM" "text/xml" body

deleteChannel :: User -> String -> IO Int
deleteChannel user channel = do
  let body = "<drop channel='" ++ channel ++ "'/>"
  shove user "ADM" "text/xml" body

shove :: User -> String -> String -> String -> IO Int
shove user channel contentType body = do
  http "PUT" takerURL user (contentType ++ "; cid=" ++ channel) body

http :: String -> String -> User -> String -> String -> IO Int
http method url (auth, cit) contentType body = do
  r <- sockReq metcastAddr (unlinesCRLF [
        method ++ " " ++ url ++ " HTTP/1.0",
        "User-Agent: SouteiMetcastTests",
        "Authorization: Basic " ++ auth,
        "SAML-Assertion: " ++ (case cit of
          Just c  -> "<Citizenship>" ++ c ++ "</Citizenship>"
          Nothing -> ""),
        "Content-type: " ++ contentType,
        "Content-length: " ++ show (length body),
        ""]
        ++ body)
  let (http, ' ':r')  = span (/= ' ') r
      (status', _)    = span (/= ' ') r'
      status          = read status'
--  putStrLn r
  when (http /= "HTTP/1.1") (fail ("invalid HTTP reply: " ++ http))
  when (status >= 500)      (fail ("HTTP server error: " ++ status'))
  return status

unlinesCRLF = concatMap (++ "\r\n")

-- common helpers

sockReq :: IO SockAddr -> String -> IO String
sockReq addr msg  = do  s <- socket AF_INET Stream 0
                        a <- addr
                        connect s a
                        send' s msg
                        shutdown s ShutdownSend
                        ret <- recv' s
                        sClose s
                        return ret
 where
  send' s ""  = return ()
  send' s msg = do  c <- send s msg
                    send' s (drop c msg)
  recv' s     = do  r <- IO.try (recv s (1024 * 8))
                    case r of
                      Left err  -> if isEOFError err then return ""
                                                     else ioError err
                      Right msg -> do msg' <- recv' s
                                      return (msg ++ msg')

assert :: IO Bool -> IO ()
assert m = m >>= \r -> case r of
  True -> do  --hPutStrLn stderr "assertion passed"
              return ()
  False -> fail "assertion failed"

assertNot :: IO Bool -> IO ()
assertNot = assert . liftM not

assertEq :: (Eq a) => a -> IO a -> IO ()
assertEq x = assert . liftM (== x)

finally :: IO a -> IO b -> IO a
finally m cleanup = do  r <- m `IO.catch` \e -> cleanup >> IO.ioError e
                        cleanup
                        return r

modifyIORefIO :: IORef a -> (a -> IO a) -> IO ()
modifyIORefIO r f = readIORef r >>= f >>= writeIORef r
