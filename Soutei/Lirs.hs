{-# LANGUAGE RecursiveDo #-}

-- $HeadURL: https://svn.metnet.navy.mil/svn/metcast/Mserver/trunk/soutei/haskell/Soutei/Lirs.hs $
-- $Id: Lirs.hs 2580 2010-08-06 00:11:56Z oleg.kiselyov $
-- svn propset svn:keywords "HeadURL Id" filename

-- LIRS: http://parapet.ee.princeton.edu/~sigm2002/papers/p31-jiang.pdf
-- Basic idea:  Maintain an LRU list, marking some entries "hot", based on
-- the distance between the last two accesses.  Maintain a separate list of
-- "cold" entries, from which we evict.

module Soutei.Lirs (
  Lirs, Storage(..), new, get, put
) where

import Prelude hiding (last, lookup)
import Control.Monad
import Data.IORef
import Data.Map (Map, empty, lookup, insert, delete, elems, size)
import Data.Maybe (fromMaybe)
import System.Random (randomRIO)

data Lirs k v sv  = Lirs {
                      cache :: IORef (Map k (Entry k v)),
                      lru :: List (Entry k v),
                      cold :: List (Entry k v),
                      storage :: Storage k v sv,
                      maxHot, maxSize :: Int
                      -- TODO: limit the number of entries, ie size of lru
                    }
data Storage k v sv = Storage {
                        store :: k -> sv -> IO (),
                        load :: k -> IO v
                      }
data Entry k v    = Entry {
                      key :: k,
                      status :: IORef (Status k v)
                    }
data Status k v   = Hot (LruElem k v) v
                  | Cold (Maybe (LruElem k v)) (ColdElem k v) v
                  | Out (LruElem k v)
type LirsElem k v = Elem (Entry k v)
type LruElem k v  = LirsElem k v
type ColdElem k v = LirsElem k v

printLirs lirs = do
  m <- readIORef (cache lirs)
  mapM (\e -> liftM ((,) (key e)) (readIORef (status e))) (elems m) >>= print
  toHList (lru lirs) >>= print . map key
  toHList (cold lirs) >>= print . map key

instance Show (Status k v) where
  showsPrec _ (Hot _ _)     = ("hot" ++)
  showsPrec _ (Cold _ _ _)  = ("cold" ++)
  showsPrec _ (Out _)       = ("out" ++)

new :: Storage k v sv -> Int -> Int -> IO (Lirs k v sv)
new s maxHot maxSize = do when (maxHot < 1 || maxSize < 1 || maxSize < maxHot)
                            (fail "bad sizes")
                          cache <- newIORef empty
                          lruList <- newList
                          coldList <- newList
                          return (Lirs cache lruList coldList s maxHot maxSize)

-- This doesn't count as a hit, but keep if the cache is not yet full.
put :: Ord k => Lirs k v sv -> k -> v -> sv -> IO ()
put lirs k v sv = do
  (store (storage lirs)) k sv
  m <- readIORef (cache lirs)
  case lookup k m of
    Just entry -> modifyIORef (status entry) (setVal v)
    Nothing    -> if size m < maxHot lirs
                    then newHot lirs k v append
                    else if size m < maxSize lirs
                      then newCold lirs k v Nothing append
                      else return ()
 where
  setVal v (Hot lruElem _)            = Hot lruElem v
  setVal v (Cold lruElem coldElem _)  = Cold lruElem coldElem v
  setVal v s@(Out _)                  = s

get :: Ord k => Lirs k v sv -> k -> IO v
get lirs k = liftM fst (get' lirs k)

get' lirs k = do
  m <- readIORef (cache lirs)
  case lookup k m of
    Just entry -> do
      lruElem' <- prepend (lru lirs) entry
      readIORef (status entry) >>= \s -> case s of
        Hot lruElem v -> do
          remove (lru lirs) lruElem
          writeIORef (status entry) (Hot lruElem' v)
          prune lirs
          return (v, True)
        Cold (Just lruElem) coldElem v -> do
          remove (lru lirs) lruElem
          remove (cold lirs) coldElem
          writeIORef (status entry) (Hot lruElem' v)
          demoteHot lirs
          return (v, True)
        Cold Nothing coldElem v -> do
          remove (cold lirs) coldElem
          coldElem' <- prepend (cold lirs) entry
          writeIORef (status entry) (Cold (Just lruElem') coldElem' v)
          return (v, True)
        Out lruElem -> do
          v <- load (storage lirs) k
          remove (lru lirs) lruElem
          writeIORef (status entry) (Hot lruElem' v)
          demoteHot lirs
          evictCold lirs
          return (v, False)
    Nothing -> do v <- load (storage lirs) k
                  if size m < maxHot lirs
                    then newHot lirs k v prepend
                    else if size m < maxSize lirs
                      then newCold lirs k v (Just prepend) prepend
                      else do newCold lirs k v (Just prepend) prepend
                              evictCold lirs
                  return (v, False)

newHot lirs k v lruIns = mdo  let entry = Entry k status
                              status <- newIORef (Hot lruElem v)
                              lruElem <- lruIns (lru lirs) entry
                              modifyIORef (cache lirs) (insert k entry)

newCold lirs k v lruIns coldIns = mdo
  let entry = Entry k status
  status <- newIORef (Cold lruElem coldElem v)
  lruElem <- case lruIns of
                Just ins -> liftM Just (ins (lru lirs) entry)
                Nothing  -> return Nothing
  coldElem <- coldIns (cold lirs) entry
  modifyIORef (cache lirs) (insert k entry)

forget lirs k = modifyIORef (cache lirs) (delete k)

demoteHot lirs = do
  Just lruElem@Elem{elemData = entry} <- readIORef (last (lru lirs))
  remove (lru lirs) lruElem
  coldElem <- prepend (cold lirs) entry
  modifyIORef (status entry) (\(Hot _ v) -> Cold Nothing coldElem v)
  prune lirs

evictCold lirs = do
  Just coldElem@Elem{elemData = entry} <- readIORef (last (cold lirs))
  remove (cold lirs) coldElem
  readIORef (status entry) >>= \s -> case s of
    Cold (Just lruElem) _ v -> writeIORef (status entry) (Out lruElem)
    Cold Nothing _ _        -> forget lirs (key entry)

prune lirs = do
  Just lruElem@Elem{elemData = entry} <- readIORef (last (lru lirs))
  readIORef (status entry) >>= \s -> case s of
    Hot _ _           -> return ()
    Cold _ coldElem v -> do remove (lru lirs) lruElem
                            writeIORef (status entry) (Cold Nothing coldElem v)
                            prune lirs
    Out _             -> do remove (lru lirs) lruElem
                            forget lirs (key entry)
                            prune lirs

-- mutable doubly-linked list

data Elem a = Elem {
                elemData :: a,
                prev, next :: IORef (Maybe (Elem a))
              }
data List a = List { first, last :: (IORef (Maybe (Elem a))) }

newList :: IO (List a)
newList = liftM2 List (newIORef Nothing) (newIORef Nothing)

toHList :: List a -> IO [a]
toHList l = walk (first l) where
  walk p = readIORef p >>= \e -> case e of
    Just e -> liftM (elemData e :) (walk (next e))
    Nothing -> return []

remove :: List a -> Elem a -> IO ()
remove l e = do
  p <- readIORef (prev e)
  n <- readIORef (next e)
  case p of
    Just p  -> writeIORef (next p) n
    Nothing -> writeIORef (first l) n
  case n of
    Just n  -> writeIORef (prev n) p
    Nothing -> writeIORef (last l) p

prepend :: List a -> a -> IO (Elem a)
prepend l x = do
  firstElem <- readIORef (first l)
  elem <- liftM2 (Elem x) (newIORef Nothing) (newIORef firstElem)
  case firstElem of
    Just f  -> writeIORef (prev f) (Just elem)
    Nothing -> writeIORef (last l) (Just elem)
  writeIORef (first l) (Just elem)
  return elem

append :: List a -> a -> IO (Elem a)
append l x = do
  lastElem <- readIORef (last l)
  elem <- liftM2 (Elem x) (newIORef lastElem) (newIORef Nothing)
  case lastElem of
    Just l  -> writeIORef (next l) (Just elem)
    Nothing -> writeIORef (first l) (Just elem)
  writeIORef (last l) (Just elem)
  return elem

-- tests

testStorage = Storage (\k v -> return ()) (\k -> return ())
testLirs = new testStorage

testPaper = do
  lirs <- testLirs 2 3
  let g n = get lirs n >> printLirs lirs
  g 1 -- [(1,hot)]                          [1]       []  
  g 4 -- [(1,hot),(4,hot)]                  [4,1]     []  
  g 2 -- [(1,hot),(2,cold),(4,hot)]         [2,4,1]   [2] 
  g 3 -- [(1,hot),(2,out),(3,cold),(4,hot)] [3,2,4,1] [3] 
  g 2 -- [(1,cold),(2,hot),(3,out),(4,hot)] [2,3,4]   [1] 
  g 1 -- [(1,cold),(2,hot),(3,out),(4,hot)] [1,2,3,4] [1] 
  g 4 -- [(1,cold),(2,hot),(4,hot)]         [4,1,2]   [1] 
  g 1 -- [(1,hot),(2,cold),(4,hot)]         [1,4]     [2] 
  g 5 -- [(1,hot),(4,hot),(5,cold)]         [5,1,4]   [5] 

testCyclic workingSize maxHot maxSize reps = do
  lirs <- testLirs maxHot maxSize
  let g n = liftM snd (get' lirs n)
  mapM (\i -> g i) [1..workingSize]
  flip mapM_ [1..reps] $ \n -> do
    mapM_ (\i -> assert    (show i) (g i)) [1..maxHot]
    mapM_ (\i -> assertNot (show i) (g i)) [maxHot+1..workingSize]
    print n
tc = testCyclic 10001 9900 10000 1000

assert :: String -> IO Bool -> IO ()
assert msg io = io >>= \r -> when (not r) (fail ("assertion failed: " ++ msg))
assertNot msg io = assert msg (liftM not io)

testRandom workingSize maxHot maxSize num = do
  lirs <- testLirs maxHot maxSize
  replicateM_ num $ do
    i <- randomRIO (1::Int, workingSize)
    get lirs i
tr = testRandom 10000 9900 10000 100000000
