{-# OPTIONS -fglasgow-exts #-}

-- A monad TRANSFORMER for BFS or DFS traversal
-- The function dfs'or'bfs2 below determines which is which

module Soutei.GBBFS (Stream, yield, runM) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity

import Data.IORef -- for tests

--import Debug.Trace -- for tests
import System.IO.Unsafe -- because Debug.Trace.trace doesn't work after interr
import System.Mem.StableName -- for tests
trace str x = unsafePerformIO (putStrLn str) `seq` x


-- The following two lines choose DFS or BFS traversal
-- dfs'or'bfs2 r1 r2 = r1 ++ r2
dfs'or'bfs2 r1 r2 = r2 ++ r1


newtype Monad m => Stream m a = Stream{unStream :: m (StreamE m a)}
type StreamE m a = (Maybe a, [Stream m a])

instance Monad m => Monad (Stream m) where
  return x = Stream (return (Just x, []))

  m >>= f = Stream (unStream m >>= bind)
   where
   bind (ans, r) = return $ let q' = map (>>=f) r
			    in (Nothing, maybe q' (\x -> f x : q') ans)


instance Monad m => MonadPlus (Stream m) where
  mzero = Stream (return (Nothing, []))

  mplus m1 m2 = Stream (unStream m1 >>= mplus')
   where
   mplus' (ans, r) = return $ (ans,  m2:r)

instance MonadTrans Stream where
    lift m = Stream (m >>= \x -> return (Just x, []))

instance MonadIO m => MonadIO (Stream m) where
    liftIO = lift . liftIO

yield :: Monad m => Stream m a -> Stream m a
yield = mplus mzero

-- run the Monad, to a specific depth, and give at most
-- specified number of answers. The monad `m' may be strict (like IO),
-- so we can't count on the laziness of the `[a]'
runM :: Monad m => Maybe Int -> Maybe Int -> Stream m a -> m [a]
runM d b m = runM' d b [m]
runM' d b m | trace ("Queue size " ++ show (length m)) False = undefined
runM' _ (Just 0)  _  = return []			-- out of breadth
runM' _ _        []  = return []			-- finished
runM' d b      (m:r) = unStream m >>= runM'' d b r
runM'' _ _ [] (Nothing,[]) = return []
runM'' _ _ [] (Just a, []) = return [a]
runM'' d b r' (Just a, r) = 
    do t <- runM' d (liftM pred b) (dfs'or'bfs2 r r'); return (a:t)
runM'' (Just 0) _ _ (Nothing, r) = return []	-- exhausted depth
runM'' d b r' (Nothing, r) = runM' (liftM pred d) b (dfs'or'bfs2 r r')

------------------------------------------------------------------------
--			Tests

-- Testing that order solutions are generated

nat :: MonadPlus m => m Int
nat = return 0 `mplus` (nat >>= return . succ)

testn0 = runIdentity $ runM Nothing (Just 10) nat
testn1 = runIdentity $ runM Nothing (Just 32) 
	                  (nat >>= \i -> nat >>= \j -> return (i,j))

{- For the true BFS, the solutions are generated in the order
   i+j=const
*GBBFS> testn1
[(0,0),(0,1),(1,0),(0,2),(1,1),(2,0),(0,3),(1,2),(2,1),(3,0),
 (0,4),(1,3),(2,2),(3,1),(4,0),(0,5),(1,4),(2,3),(3,2),(4,1),(5,0)]
-}

-- This test shows that effects associated with the solutions are
-- NOT duplicated
-- Compare with the corresponding test in FBIDFS.hs
natio :: Stream IO Int
natio = do n <- natio'; liftIO $ print n; return n
 where natio' = return 0 `mplus` (natio' >>= return . succ)
	  
testn2  = runM Nothing (Just 5) natio >>= print

-- Don't try the following with the regular List monad or List comprehension!
-- That would diverge instantly: all `i', `j', and `k' are infinite
-- streams

pythagorean_triples :: MonadPlus m => m (Int,Int,Int)
pythagorean_triples =
    let number = (return 0) `mplus` (number >>= return . succ) in
    do
    i <- number
    guard $ i > 0
    j <- number
    guard $ j > 0
    k <- number
    guard $ k > 0
    guard $ i*i + j*j == k*k
    return (i,j,k)

-- If you run this in GHCi, you can see that Indetity is a lazy monad 
-- and IO is strict: evaluating `test' prints the answers as they are computed.
-- OTH, testio runs silently for a while and then prints all the answers
-- at once
test = runIdentity $ runM Nothing (Just 7) pythagorean_triples
testio = runM Nothing (Just 7) pythagorean_triples >>= print


-- The following code is not in general MonadPlus: it uses Incomplete
-- explicitly. But it supports left recursion! Note that in OCaml, for example,
-- we _must_ include that Incomplete data constructor to make
-- the recursive definition well-formed.  
-- The code does *not* get stuck in the generation of primitive tuples
-- like (0,1,1), (0,2,2), (0,3,3) etc.
pythagorean_triples' :: Monad m => Stream m (Int,Int,Int)
pythagorean_triples' =
    let number = (yield number >>= return . succ) `mplus` return 0  in
    do
    i <- number
    j <- number
    k <- number
    guard $ i*i + j*j == k*k
    return (i,j,k)

test'   = runIdentity $ runM Nothing (Just 27) pythagorean_triples'
testio' = runM Nothing (Just 27) pythagorean_triples' >>= print

pythagorean_triples'' :: Stream IO (Int,Int,Int)
pythagorean_triples'' =
    let number = (yield number >>= return . succ) `mplus` return 0  in
    do
    i <- number
    j <- number
    k <- number
    liftIO $ print (i,j,k)
    guard $ i*i + j*j == k*k
    return (i,j,k)

testio'' = runM Nothing (Just 7) pythagorean_triples'' >>= print

-- a serious test of left recursion (due to Will Byrd)
flaz x = yield (flaz x) `mplus` (yield (flaz x) `mplus`
				      if x == 5 then return x else mzero)
test_flaz = runIdentity $ runM Nothing (Just 15) (flaz 5)


test_gt0 :: Stream IO Int
test_gt0 =
    do
    g_count <- liftIO $ newIORef 0
    t_count <- liftIO $ newIORef 0
    let clear = (liftIO $ writeIORef g_count 0) >> 
		(liftIO $ writeIORef t_count 0)
    let g = do
	    liftIO $ modifyIORef g_count succ
	    return 0 `mplus` (g >>= \y -> return $ 3*y + 1)
        t x = do
	      -- liftIO $ print $ "t: " ++ (show x)
	      liftIO $ modifyIORef t_count succ
	      (if x == - 10 then return x else mzero) `mplus` t (x-1)
    clear
    r <- g >>= t
    gc <- liftIO $ readIORef g_count
    tc <- liftIO $ readIORef t_count
    clear
    liftIO $ putStrLn $ "g_count: " ++ (show gc) ++ 
	                ", t_count: " ++ (show tc)
    return r

-- runM Nothing (Just 2) test_gt0 >>= print

test_gt1 :: Stream IO Int
test_gt1 =
    do
    g_count <- liftIO $ newIORef 0
    t_count <- liftIO $ newIORef 0
    let clear = (liftIO $ writeIORef g_count 0) >> 
		(liftIO $ writeIORef t_count 0)
    let g = do
	    liftIO $ modifyIORef g_count succ
	    (yield g >>= \y -> return $ 3*y + 1) `mplus` return 0 
        t x = do
	      -- liftIO $ print $ "t: " ++ (show x)
	      liftIO $ modifyIORef t_count succ
	      (yield $ t (x-1)) `mplus` 
		(if x == - 10 then return x else mzero)  
    clear
    r <- g >>= t
    gc <- liftIO $ readIORef g_count
    tc <- liftIO $ readIORef t_count
    clear
    liftIO $ putStrLn $ "g_count: " ++ (show gc) ++ 
	                ", t_count: " ++ (show tc)
    return r


-- test for the scheduling order
-- it seems like BFS...
test_sched n = runM (Just n) Nothing (t "")
    where
    t l | trace l False = undefined
    t l = Stream(return (Nothing, [(mplus (t ('0':l)) (t ('1':l)))]))


pythagorean_triplesi :: MonadPlus m => () -> m (Int,Int,Int)
pythagorean_triplesi () =
    let number l = trace ("Label: " ++ l)
		   (return 0) `mplus` (number ('R':l)>>= return . succ) in
    do
    i <- number "i"
    guard $ i > 0
    j <- number ("j" ++ (replicate i 'R'))
    guard $ j > 0
    k <- number ("k" ++ (replicate i 'R') ++ "," ++ (replicate j 'R'))
    guard $ k > 0
    guard $ i*i + j*j == k*k
    return (i,j,k)

testi n = runM (Just n) Nothing (pythagorean_triplesi ())
