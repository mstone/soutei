module Soutei.Client (
  ServiceAddress(..), parseServiceAddress,
  queryRemote
) where

import Control.Monad.Error ()  -- just for instance Monad Either
import Data.Maybe (fromMaybe)
import Network
import System.IO

import Soutei.Sexpr as Sexpr
import Soutei.Soutei as Soutei
import Soutei.Syntax

data ServiceAddress = HostPort HostName PortID

parseServiceAddress :: Monad m => String -> m ServiceAddress
parseServiceAddress s =
  let (host, port) = break (== ':') s
  in  case port of
        ':':port -> case reads port of
          [(port,"")] -> return (HostPort host (PortNumber (fromInteger port)))
          _           -> return (HostPort host (Service port))
        _ -> fail "cannot parse host:port"

instance Read ServiceAddress where
  readsPrec _ s = map (\r -> (r, "")) (eitherToList (parseServiceAddress s))

eitherToList :: Either String a -> [a]
eitherToList (Left _)  = []
eitherToList (Right x) = [x]

queryRemote :: ServiceAddress -> String -> [Fact] -> Goal -> IO Bool
queryRemote (HostPort host port) id facts goal =
  let q = Sexpr.fromList (Sexpr.Atom (Val (SString id)) :
                          Sexpr.Atom (Val (SString "query")) :
                          atomToSexpr goal :
                          map (atomToSexpr . factToAtom) facts)
  in  do h <- connectTo host port
         hPrint h q
         hFlush h
         r <- hGetContents h
         fromMaybe (fail "Unexpected reply from Soutei")
                   (lookup r [("(" ++ id ++ " #t)\n", return True),
                              ("(" ++ id ++ " #f)\n", return False)])

atomToSexpr :: HeadAtom v -> Sexpr.Sexpr (Term v)
atomToSexpr (Soutei.Atom NoCtx (Pred pred arity) args) =
  Sexpr.fromAtomList (Val (SString pred):args)

t = let facts = [Soutei.Atom NoCtx (Pred "foo" 1) [Val (SString "bar")]]
        goal  =  Soutei.Atom NoCtx (Pred "may" 1) [Val (SString "HTTP")]
    in  queryRemote (read "localhost:1500") "test" facts goal
