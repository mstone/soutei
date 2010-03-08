{-# OPTIONS -fglasgow-exts #-}
-- $Id: Assertions.hs 1936 2008-02-05 06:18:40Z oleg.kiselyov $

-- Routines for loading, storing, and running queries against assertions.
-- Primary interface is imperative because the memory index must be kept in
-- sync with the disk; functional alternatives end in F.

module Soutei.Assertions (
  Assertions, AssertionsF,
  fromDataDir, fromDataDirWithActions, 
  loadSysCtx, putCtx, putCtxRules, query, queryResults,
  emptyF, loadSysCtxF, putCtxF, putCtxRulesF, queryF, queryResultsF
) where

import Prelude hiding (lookup)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Data.Char (ord, isAlphaNum)
import Data.Map (Map, empty, lookup, insert, delete, union, fromList)
import Data.Maybe (fromMaybe, fromJust)
import Data.Version (Version(..), showVersion, parseVersion)
import Numeric (showHex)
import System.Directory
import System.IO
import System.IO.Error as IO
import System.IO.Unsafe (unsafePerformIO)
import Text.ParserCombinators.ReadP (readP_to_S, string)

import Soutei.Check
import Soutei.FBackTrackT (Stream, runM)
import qualified Soutei.Lirs as Lirs
import Soutei.Logic (Query, runQuery, false,
                     CtxQuery, PredQuery, ArgsQuery,
                     prove, proveResults, clausesQuery, funcQuery)
import Soutei.Parsec (parseM)
import Soutei.Soutei
import Soutei.Syntax (top, assertionL)

data Assertions   = Assertions {
                      put :: Const -> CtxIndex IO -> Maybe String -> IO (),
                      get :: Const -> IO (CtxIndex IO),
                      app :: AppPrims IO
                    }
data AssertionsF  = AssertionsF {
                      index :: Index,
                      appF  :: AppPrims Identity
                    }
type Index        = Map Const (CtxIndex Identity)
type CtxIndex m   = Map Pred (ArgsQuery m)
type AppPrims m   = Map Pred (ArgsQuery m, [ArgMode])

fromDataDir :: (IOError -> IO a) -> FilePath -> IO Assertions
fromDataDir err dataDir = do
  initDataDir dataDir
  let storage = Lirs.Storage (storeA dataDir)
                             (\ctx -> do  s <- loadA dataDir ctx
                                          rules <- parseA "data store" s
                                          uncheckedIndexRules rules
                                      `IO.catch` \e -> err e >> return empty)
  lirs <- Lirs.new storage 990 1000
  return (Assertions (Lirs.put lirs) (Lirs.get lirs) stdAppPrims)

fromDataDirWithActions :: 
    (IOError -> IO a) ->
    (Const -> IO (Maybe String)) ->        -- read action
    (Const -> (Maybe String) -> IO ()) ->  -- write action
    IO Assertions
fromDataDirWithActions err readA writeA = do
  let storage = Lirs.Storage (writeA)
                             (\ctx -> do  s <- readA ctx
                                          rules <- parseA "data store" s
                                          uncheckedIndexRules rules
                                      `IO.catch` \e -> err e >> return empty)
  lirs <- Lirs.new storage 990 1000
  return (Assertions (Lirs.put lirs) (Lirs.get lirs) stdAppPrims)

emptyF :: AssertionsF
emptyF = AssertionsF empty stdAppPrims

loadSysCtx :: FilePath -> Assertions -> IO ()
loadSysCtx initFile as = do
  s <- readInitFile initFile
  putCtx initFile sysCtx (Just s) as

loadSysCtxF :: FilePath -> AssertionsF -> IO AssertionsF
loadSysCtxF initFile as = do
  s <- readInitFile initFile
  putCtxF initFile sysCtx (Just s) as

readInitFile :: FilePath -> IO String
readInitFile initFile =
  doesFileExist initFile >>= \b -> if b
    then readFile initFile
    else fail ("system-assertion " ++ initFile ++ " not found")

-- Parse and add (compile and store) an assertion for a context
putCtx :: String -> Const -> Maybe String -> Assertions -> IO ()
putCtx source ctx s as = do rules <- parseA source s
                            putCtx' ctx s rules as

putCtxRules :: Const -> Maybe [SynRule] -> Assertions -> IO ()
putCtxRules ctx rules as =  let s = liftM (concatMap show) rules
                            in  putCtx' ctx s (fromMaybe [] rules) as

putCtx' :: Const -> Maybe String -> [SynRule] -> Assertions -> IO ()
putCtx' ctx s rules as = do
  ctxIndex <- indexRules (fmap snd (app as)) rules
  put as ctx ctxIndex s

putCtxF :: Monad m => String -> Const -> (Maybe String) ->
            AssertionsF -> m AssertionsF
putCtxF source ctx s as = do  rules <- parseA source s
                              putCtxF' ctx rules as

putCtxRulesF :: Monad m => Const -> Maybe [SynRule] -> AssertionsF ->
                  m AssertionsF
putCtxRulesF ctx rules as = putCtxF' ctx (fromMaybe [] rules) as

putCtxF' :: Monad m => Const -> [SynRule] -> AssertionsF -> m AssertionsF
putCtxF' ctx [] as = return as{index=delete ctx (index as)}
putCtxF' ctx rules as = do  ctxIdx <- indexRules (fmap snd (appF as)) rules
                            return as{index=insert ctx ctxIdx (index as)}

parseA :: Monad m => String -> Maybe String -> m [SynRule]
parseA source Nothing = return []
parseA source (Just s) = parseM (top assertionL) source s

indexRules :: (Monad m, Monad m') => CtxModes -> [SynRule] -> m (CtxIndex m')
indexRules appModes rules = liftM compilePreds (checkRules appModes rules)

uncheckedIndexRules :: (Monad m, Monad m') => [SynRule] -> m (CtxIndex m')
uncheckedIndexRules facts = liftM compilePreds (groupRules facts)

compilePreds :: Monad m => Map Pred [SynRule] -> CtxIndex m
compilePreds preds = ctxIdx where
  ctxIdx = fmap compile preds
  compile clauses = clausesQuery clauses predQuery
  predQuery pred = fromJust (lookup pred ctxIdx)

query :: Maybe Int -> Assertions -> [Fact] -> SynHeadAtom -> IO Bool
query t as facts goal = mkCtxQ as facts >>= query' t goal

queryF :: Monad m => Maybe Int -> AssertionsF -> [Fact] -> SynHeadAtom -> m Bool
queryF t as facts goal = liftM (runIdentity . query' t goal) (mkCtxQF as facts)

queryResults :: Maybe Int -> Assertions -> [Fact] -> SynHeadAtom -> IO [Fact]
queryResults t as facts goal = mkCtxQ as facts >>= queryResults' t goal

queryResultsF :: Monad m => Maybe Int -> AssertionsF -> [Fact] -> SynHeadAtom ->
                    m [Fact]
queryResultsF t as facts goal = liftM (runIdentity . queryResults' t goal)
                                      (mkCtxQF as facts)

query' :: Monad m => Maybe Int -> SynHeadAtom -> CtxQuery m -> m Bool
query' t goal ctxQ =  let q = prove sysCtx goal ctxQ
                      in  liftM (not . null) (runM t (Just 1) (runQuery q))

queryResults' :: Monad m => Maybe Int -> SynHeadAtom -> CtxQuery m -> m [Fact]
queryResults' t goal ctxQ = let q = proveResults sysCtx goal ctxQ
                            in  liftM (map fst) (runM t Nothing (runQuery q))

mkCtxQ :: Monad m => Assertions -> [Fact] -> m (CtxQuery IO)
mkCtxQ as facts = do  appIdx <- mkAppIdx (fmap fst (app as)) facts
                      return (asQuery as appIdx)

mkCtxQF :: Monad m => AssertionsF -> [Fact] -> m (CtxQuery Identity)
mkCtxQF as facts =  do  appIdx <- mkAppIdx (fmap fst (appF as)) facts
                        return (idxQuery (insert appCtx appIdx (index as)))

mkAppIdx :: (Monad m, Monad m') => CtxIndex m' -> [Fact] -> m (CtxIndex m')
mkAppIdx appPrims facts = let rules = map (flip Rule [] . factToAtom) facts
                          in  liftM (union appPrims) (uncheckedIndexRules rules)

asQuery :: Assertions -> CtxIndex IO -> CtxQuery IO
asQuery as appIdx ctx pred args frame
  | ctx == appCtx = ctxIdxQ appIdx
  | otherwise     = \s -> do  ctxIdx <- liftIO (get as ctx)
                              ctxIdxQ ctxIdx s
 where
  ctxIdxQ ctxIdx = case lookup pred ctxIdx of
    Just q  -> q args (asQuery as appIdx) frame
    Nothing -> false

idxQuery :: Index -> CtxQuery Identity
idxQuery idx ctx pred args frame = fromMaybe false $ do
                                    ctxIdx <- lookup ctx idx
                                    q <- lookup pred ctxIdx
                                    return (q args (idxQuery idx) frame)

-- primitives

ipofQuery :: Monad m => (ArgsQuery m, [ArgMode])
ipofQuery = (funcQuery ipof', [R L, R S]) where
  ipof' [SIP4Addr addr, SIP4Net net] = ip4of addr net
  ipof' _ = False

neqQuery :: Monad m => (ArgsQuery m, [ArgMode])
neqQuery = (funcQuery (\[x1, x2] -> x1 /= x2), [R S, R S])

stdAppPrims :: Monad m => AppPrims m
stdAppPrims = fromList [(Pred "ip-of" 2, ipofQuery),
                        (Pred "neq" 2, neqQuery)]

-- storage

initDataDir :: FilePath -> IO ()
initDataDir dataDir = let markerFile = markerFilename dataDir in
  doesDirectoryExist dataDir >>= \b -> if b
    then doesFileExist markerFile >>= \b -> if b
          then do marker <- readFile markerFile
                  checkMarker dataDir marker
                  return ()
          else do ents <- getDirectoryContents dataDir
                  if (null (filter (\ent -> ent /= "." && ent /= "..") ents))
                    then do writeFile markerFile marker
                            createDirectory (assertionsDir dataDir)
                    else fail (dataDir ++ " is not a Soutei data directory")
    else fail (dataDir ++ " is not a directory")

storeA :: FilePath -> Const -> (Maybe String) -> IO ()
storeA dataDir ctx = store where
  store Nothing   = do  b <- doesFileExist ctxFile
                        when b (removeFile ctxFile)
  store (Just s)  = do  writeFile tmpFile s
                        renameFile tmpFile ctxFile
  tmpFile = tmpFilename dataDir
  ctxFile = ctxFilename dataDir ctx

loadA :: FilePath -> Const -> IO (Maybe String)
loadA dataDir ctx = let ctxFile = ctxFilename dataDir ctx in do
  b <- doesFileExist ctxFile
  if b
    then liftM Just (readFile ctxFile)
    else return Nothing

marker = magic ++ showVersion dataDirVersion ++ "\n"
checkMarker dataDir s = case readP_to_S p s of
    [(Version [major, minor] [], "")] ->
          if major > dataDirMajor
            then fail (dataDir ++ " is newer than Soutei")
            else return (major, minor)
    _ ->  fail (dataDir ++ " is not a Soutei data directory (corrupt SOUTEI)")
 where
  p = do  string magic
          v <- parseVersion
          string "\n"
          return v
magic = "Soutei data directory, version "
dataDirVersion = Version [dataDirMajor, dataDirMinor] []
dataDirMajor = 0
dataDirMinor = 0

markerFilename :: FilePath -> FilePath
markerFilename dataDir = dataDir ++ "/SOUTEI"

tmpFilename :: FilePath -> FilePath
tmpFilename dataDir = dataDir ++ "/.tmp"

ctxFilename :: FilePath -> Const -> FilePath
ctxFilename dataDir ctx = assertionsDir dataDir ++ "/" ++ encode (show ctx)

assertionsDir :: FilePath -> FilePath
assertionsDir dataDir = dataDir ++ "/assertions"

encode :: String -> String
encode = concatMap encode' where
  encode' ch | isAlphaNum ch || ch `elem` "!#$&'()+,-.;=@_" = [ch]
             | otherwise = '%' : showHex (ord ch) ";"

