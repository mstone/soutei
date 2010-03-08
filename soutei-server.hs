-- Soutei server

module Main where

import Prelude hiding (log)
import Control.Exception (bracket)
import Control.Monad (liftM)
import Network
import System.Environment (getArgs)
import System.Exit
import System.IO
import System.IO.Error as IO
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Locale (defaultTimeLocale)
import System.Time (getClockTime, toCalendarTime, formatCalendarTime)

import Soutei.Assertions (Assertions, fromDataDir, loadSysCtx, putCtx,
                          query, queryResults)
import Soutei.Parsec (parseM)
import Soutei.Sexpr (Sexpr)
import Soutei.Sexpr as Sexpr
import Soutei.Soutei as Soutei
import Soutei.Syntax

usage = unlines (
 "Soutei $Id: soutei-server.hs 2155 2009-05-19 03:40:47Z oleg.kiselyov $":
 "Usage: server SYSTEM-ASSERTION DATA-DIR PORT":
 "Start the Soutei server.":
 "  SYSTEM-ASSERTION  a file containing the system assertion":
 "  DATA-DIR          a directory for storing persistent data":
 "  PORT              the TCP port on which to listen (number or service name)":
 "Requests take one of the forms:":
 "  (REQ-ID query (REQ-PRED ARG1 ...) (FACT1-PRED ARG1 ...) ...)":
 "    Try to derive the request from the policy, using the facts as the":
 "    application assertion.  Returns #t (allow) or #f (deny).":
 "  (REQ-ID query-results (REQ-PRED ARG1 ...) (FACT1-PRED ARG1 ...) ...)":
 "    Like query, but returns the list of derived facts.":
 "  (REQ-ID assertion CONTEXT \"ASSERTION-TEXT\")":
 "    Add or replace the assertion for the given context.  Returns #f if there":
 "    was a problem with the assertion, #t otherwise.":
 "Response has the form":
 "  (REQ-ID RETURN)":
 "Log messages go to stderr, which can be redirected to a file.":
 [])

useError msg = do hPutStrLn stderr msg
                  hPutStr stderr usage
                  exitWith (ExitFailure 64)

startError err =  let msg = if isUserError err then ioeGetErrorString err
                                               else show err
                  in  do  hPutStrLn stderr msg
                          exitWith (ExitFailure 66)

main = do getArgs >>= \args -> case args of
            [initFile, dataDir, port] -> do
                let portID = case reads port of
                              ((n, ""):_) -> PortNumber (fromIntegral n)
                              _           -> Service port
                as <- fromDataDir logErr dataDir `IO.catch` startError
                loadSysCtx initFile as `IO.catch` startError
                bracket (listenOn portID `IO.catch` startError) sClose $
                  \s -> do  logDate "Soutei $Id: soutei-server.hs 2155 2009-05-19 03:40:47Z oleg.kiselyov $ started"
                            loop as s
            _ -> useError "Exactly three arguments required."

loop :: Assertions -> Socket -> IO ()
loop as s = do
  logDate "Listening ..."
  bracket (accept s)
          (\(h,_,_) -> hClose h `IO.catch` logErr) $ \(h,_,_) -> do
    logDate "Connected"
    req <- hGetContents h
    se <- parseM (Sexpr.whiteSpace >> Sexpr.cons term) "request" req
    log ("--> " ++ show se)
    Sexpr.toList se >>= \l -> case l of
      Sexpr.Atom (Val reqId) : Sexpr.Atom (Val (SString cmd)) : rest -> do
        r <- serveRequest as cmd rest
              `IO.catch` \e -> logErr e >> return (boolToSexpr False)
        let rsp = show (Sexpr.Atom (Const' reqId) `Sexpr.Cons` r)
        log ("<-- " ++ rsp)
        hPutStrLn h rsp
      _ -> fail "malformed request"
   `IO.catch` logErr
  loop as s

log msg = log' msg
logDate msg = do  t <- getClockTime >>= toCalendarTime
                  log' ("[" ++ formatCalendarTime defaultTimeLocale
                                      "%Y-%m-%d %H:%M:%S" t ++ "] "
                            ++ msg)
logErr err = log' (if isUserError err then ioeGetErrorString err
                                      else show err)
log' msg = hPutStrLn stderr msg `IO.catch` \e -> exitWith (ExitFailure 74)

data Const' = Const' Const
            | Bool' Bool
instance Show Const' where
  showsPrec _ (Const' x)    = shows x
  showsPrec _ (Bool' False) = ("#f" ++)
  showsPrec _ (Bool' True)  = ("#t" ++)

serveRequest :: Assertions -> String -> [Sexpr SynTerm] -> IO (Sexpr Const')
serveRequest as "query" l         = liftM boolToSexpr (doQuery as l)
serveRequest as "query-results" l = liftM factsToSexpr (doQueryResults as l)
serveRequest as "assertion" l     = liftM unitToSexpr (doAssertion as l)
serveRequest _ cmd _              = fail ("unknown command: " ++ cmd)

boolToSexpr b = Sexpr.fromAtomList [Bool' b]
unitToSexpr _ = boolToSexpr True
factsToSexpr fs = Sexpr.fromList (map factToSexpr fs) where
  factToSexpr (Soutei.Atom NoCtx (Pred p _) as) =
      Sexpr.fromAtomList (Const' (SString p) : map (\(Val x) -> Const' x) as)

doQuery :: Assertions -> [Sexpr SynTerm] -> IO Bool
doQuery as (goal:facts) = do
  goal' <- mkAtom goal
  facts' <- mapM mkFact facts
  query runLimit as facts' goal'
doQuery _ _ = fail "query missing goal"

doQueryResults :: Assertions -> [Sexpr SynTerm] -> IO [Fact]
doQueryResults as (goal:facts) = do
  goal' <- mkAtom goal
  facts' <- mapM mkFact facts
  queryResults runLimit as facts' goal'

runLimit = Just 10000

mkAtom :: Sexpr (SynTerm) -> IO (SynHeadAtom)
mkAtom l = Sexpr.toAtomList l >>= \l -> case l of
  (Val (SString pred):args@(_:_)) ->
    return (Soutei.Atom NoCtx (Pred pred (length args)) args)
  _ -> fail "malformed or missing predicate"

mkFact :: Sexpr (SynTerm) -> IO Fact
mkFact fact = mkAtom fact >>= atomToFact

doAssertion :: Assertions -> [Sexpr (SynTerm)] -> IO ()
doAssertion as [Sexpr.Atom (Val ctx), Sexpr.Atom (Val (SString text))]
  | ctx == sysCtx = fail "may not replace system assertion"
  | ctx == appCtx = fail "may not add application assertion"
  | True = let s = case text of
                    "" -> Nothing
                    _  -> Just text
           in  putCtx "assertion" ctx s as
doAssertion _ _ = fail "malformed assertion request"

