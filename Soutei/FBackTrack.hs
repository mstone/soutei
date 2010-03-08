{-# OPTIONS -fglasgow-exts #-}

-- Simple Fair back-tracking monad
-- Based on the Scheme code book-si, `Stream implementation, with incomplete'
-- as of Feb 18, 2005

module Soutei.FBackTrack where

import Control.Monad

data Stream a = Nil | One a | Choice a (Stream a) | Incomplete (Stream a)
	      | IncompleteR (Stream a)
    deriving Show

instance Monad Stream where
  return = One

  Nil          >>= f = Nil
  One a        >>= f = f a
  Choice a r   >>= f = f a `mplus` (Incomplete (r >>= f))
{-
  Incomplete i >>= f = case i of
			      Incomplete j -> Incomplete (j >>= f)
			      _ -> i >>= f
-}
  IncompleteR i >>= f = IncompleteR (i >>= f)
  Incomplete i >>= f = Incomplete (i >>= f)

instance MonadPlus Stream where
  mzero = Nil

  mplus Nil r'          = Incomplete r'
  mplus (One a) r'      = Choice a r'
  mplus (Choice a r) r' = Choice a (mplus r' r) -- interleaving!
  --mplus (Incomplete i) r' = Incomplete (mplus i r')
  mplus r@(Incomplete i) r' = 
      case r' of
	      Nil         -> r
	      One b       -> Choice b i
	      Choice b r' -> Choice b (mplus i r')
	      -- Choice _ _ -> Incomplete (mplus r' i)
	      Incomplete j ->  (Incomplete (mplus i j))
	      IncompleteR j ->  IncompleteR (mplus i j)
  mplus r@(IncompleteR i) r' = IncompleteR (mplus r' i)

-- run the Monad, to a specific depth
runM :: Maybe Int -> Stream a -> [a]
runM _ Nil = []
runM _ (One a) = [a]
runM d (Choice a r) = a : (runM d r)
runM (Just 0) (Incomplete r) = []	-- exhausted depth
runM (Just 0) (IncompleteR r) = []	-- exhausted depth
runM d (Incomplete r) = runM (liftM pred d) r
runM d (IncompleteR r) = runM (liftM pred d) r

-- Don't try the following with the regular List monad or List comprehension!
-- That would diverge instantly: all `i', `j', and `k' are infinite
-- streams

pythagorean_triples :: MonadPlus m => m (Int,Int,Int)
pythagorean_triples =
    let number = (return 0) `mplus` (number >>= (return . (+1))) in
    do
    i <- number
    guard $ i > 0
    j <- number
    guard $ j > 0
    k <- number
    guard $ k > 0
    guard $ i*i + j*j == k*k
    return (i,j,k)

test = take 7 $ runM Nothing pythagorean_triples

-- even more fun

pythagorean_triples2 :: Stream (Int,Int,Int)
pythagorean_triples2 =
    let number = (IncompleteR number >>= \n -> return (n+1)) `mplus` return 0 in
    do  i <- number
        j <- number
        k <- number
        guard $ i*i + j*j == k*k
        return (i,j,k)

test2 = take 7 $ filter (\(x,y,_) -> x*y/=0) $ runM Nothing pythagorean_triples2

-- test different branching strategies
-- note--ensure that structures aren't shared

-- m = y (m + m + ... + m)
t1 n = y (msum (map ($ n) (replicate n t1)))

-- m = (y m + y m + ... + y m)
t2 n = msum (map (y . ($ n)) (replicate n t2))

-- m = y (m + y (m + y (... + y m)...))
t3 n = y (foldr1 (\m m' -> mplus m (y m')) (map ($ n) (replicate n t3)))
t3' n = foldr (\m m' -> y (mplus m m')) mzero (map ($ n) (replicate n t3'))

-- m = y $ y $ ... $ y (m + m + ... + m)
t4 n = foldr1 (.) (replicate n y) (msum (map ($ n) (replicate n t4)))

-- m = y $ y $ ... $ y (m + m + ... + m)  (~ ln n y's)
t5 n = let n' = 1 + floor (log (fromIntegral n))
           t n = foldr1 (.) (replicate n' y) (msum (map ($ n) (replicate n t)))
       in  t n

y = IncompleteR
