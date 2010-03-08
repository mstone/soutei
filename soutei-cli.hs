{-# LANGUAGE PatternGuards #-}

module Main where

import System.Environment
import System.Exit
import Network
import System.IO
import Control.Exception (bracket)
import System.IO.Error
import Control.Exception (evaluate,assert)

docstrings = unlines [
  "Simple Soutei Query interface",
  "",
  "This program accepts query parameters as arguments, queries",
  "the soutei application and indicates the success as its",
  "return code. The ExitSuccess code indicates that Soutei advises for",
  "the requested action. Any other code (including abort codes) mean",
  "disapproval",
  "",
  "Synopsis",
  "  soutei-cli soutei-host-port -- key val1 ... -- key val1 ...",
  "",
  "where soutei-host-port is a string like localhost:1500",
  "The other arguments are passes as key val1 val2 tuples separated by --",
  "",
  "For example,",
  "./soutei-cli 127.0.0.1:1500 -- may channel read -- this-user SYSADM -- this-channel-owner ADMH system && echo ok"

  ]


req'id = "sm-req-1.0"

data QArgs = QArgs{ qa_host :: !String,
		    qa_port :: !Int,
		    qa_kv   :: ![[String]] } deriving Show

parse'args [] = error "No args"
parse'args (host'port : argkvs) = 
    QArgs host (parse_port port_str) (parse_kvs argkvs [])
    where 
    (host,port_str) = break (==':') host'port
    parse_port (':':str) | [(n,"")] <- reads str = n
    parse_port str = error $ "Bad port number arg: " ++ str

    parse_kvs [] acc | not $ null acc = reverse acc
    parse_kvs ("--":args) acc = parse_kvs args' (kv:acc)
	where (kv,args') = break (=="--") args
    parse_kvs args _ = error $ "bad keyword args: " ++ (show args)

parse_args args = error $ "bad args: " ++ (show args)


make'req kvs = (showParen True $ showString req'id . showChar ' ' .
		                 showString "query" . showChar ' ' . skvs) ""
    where skvs = foldr (\e z -> showParen True ((unqwords e)++) . z) id kvs
	  -- like unwords, but print the strings in double quotes, with escapes
	  -- if needed. Except the first word! Soutei takes it
	  unqwords (key:vals) = unwords (key : map show vals)

-- The OK reply is of the form "(sm-req-1.0 #t)" (followed by newline, perhaps)
check'reply rep | (_,'#':'t':_) <- break (=='#') rep = True
check'reply _ = False


soutei :: QArgs -> IO String
soutei qa  = do  bracket
		  (connectTo (qa_host qa) 
		             (PortNumber (fromIntegral (qa_port qa))))
		  (hClose)
		  (\h -> do
		         hPutStr h $ make'req (qa_kv qa)
		         hFlush h
                         -- shutdown s ShutdownSend
		         hGetLine h)
print'help str = do
		 putStrLn str; putStrLn ""
		 putStrLn docstrings
		 exitWith (ExitFailure 4)

main = do
       args <- getArgs
       if null args then print'help "no args" else return ()
       rep <- soutei (parse'args args)
       exitWith (if check'reply rep then ExitSuccess
		    else ExitFailure 2)
