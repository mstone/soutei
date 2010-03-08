{-# OPTIONS -fglasgow-exts #-}

-- $Id: Soutei.hs 2155 2009-05-19 03:40:47Z oleg.kiselyov $

module Soutei.Soutei where

import Control.Monad
import Data.Bits
import Data.Word
import Test.QuickCheck

-- The Soutei language

data Const            = SString String
                      | SNumber !Integer
                      | SIP4Addr !IP4Addr
                      | SIP4Net !IP4Net
                      deriving (Eq, Ord)
newtype ConstC v      = ConstC Const
data IP4Addr          = IP4Addr !Word32
                      deriving (Eq, Ord)
data IP4Net           = IP4Net !IP4Addr !Int
                      deriving (Eq, Ord)
data Pred             = Pred String !Int
                      deriving (Eq, Ord)
newtype SynVar        = SynVar String
                      deriving (Eq, Ord)
data Var v            = Anon
                      | Named v
                      deriving Eq
-- No data constructor--used to guarantee a fact has no variables
data NoVar
data Term v           = Var v
                      | Val Const
                      deriving Eq
-- c is the container type constructor of context (NoCtx or MaybeCtx)
data Atom c v         = Atom {
                          ctx  :: c (Term v),
                          pred :: Pred,
                          args :: [Term v] }
type HeadAtom v       = Atom NoCtx v
type BodyAtom v       = Atom MaybeCtx v
data NoCtx a          = NoCtx
data MaybeCtx a       = JustCtx a
                      | NothingCtx
data Rule v           = Rule (HeadAtom v) [BodyAtom v]
type Fact             = Atom NoCtx NoVar
type SynTerm          = Term (Var SynVar)
type SynAtom c        = Atom c (Var SynVar)
type SynHeadAtom      = HeadAtom (Var SynVar)
type SynBodyAtom      = BodyAtom (Var SynVar)
type SynRule          = Rule (Var SynVar)
type Goal             = SynHeadAtom

atomToFact :: Monad m => Atom NoCtx v -> m Fact
atomToFact atom = fmapM (\_ -> fail "facts may not have variables") atom

factToAtom :: Fact -> Atom NoCtx v
factToAtom fact = fmap undefined fact

class FunctorM f where
    fmapM :: Monad m => (a -> m b) -> f a -> m (f b)

instance FunctorM Maybe where
  fmapM f Nothing = return Nothing
  fmapM f (Just x) = f x >>= return . Just

instance Functor Term where
  fmap f (Var v) = Var (f v)
  fmap f (Val x) = Val x
instance FunctorM Term where
  fmapM f (Var v) = liftM Var (f v)
  fmapM f (Val x) = return (Val x)

instance Functor c => Functor (Atom c) where
  fmap f (Atom ctx pred args) = Atom (fmap (fmap f) ctx)
                                     pred
                                     (map (fmap f) args)
instance FunctorM c => FunctorM (Atom c) where
  fmapM f (Atom ctx pred args) = do
    ctx' <- fmapM (fmapM f) ctx
    args' <- mapM (fmapM f) args
    return (Atom ctx' pred args')

instance Functor Rule where
  fmap f (Rule h b) = Rule (fmap f h) (map (fmap f) b)
instance FunctorM Rule where
  fmapM f (Rule h b) = liftM2 Rule (fmapM f h) (mapM (fmapM f) b)

instance Functor NoCtx where
  fmap f NoCtx = NoCtx
instance FunctorM NoCtx where
  fmapM f NoCtx = return NoCtx

instance Functor MaybeCtx where
  fmap f (JustCtx x) = JustCtx (f x)
  fmap f NothingCtx = NothingCtx
instance FunctorM MaybeCtx where
  fmapM f (JustCtx x) = liftM JustCtx (f x)
  fmapM f NothingCtx = return NothingCtx

sysCtx = SString "system"
appCtx = SString "application"

fact :: String -> [Const] -> Atom NoCtx v
fact pred args = Atom NoCtx (Pred pred (length args)) (map Val args)

goal :: String -> [Const] -> Goal
goal pred args = Atom NoCtx (Pred pred (length args)) (map Val args)

-- IPAddr/IPNet

bytesToIP4Addr :: [Word8] -> IP4Addr
bytesToIP4Addr = IP4Addr . bytesToBits 4

ip4AddrToBytes :: IP4Addr -> [Word8]
ip4AddrToBytes (IP4Addr addr) = bitsToBytes 4 addr

bytesToBits :: Bits a => Int -> [Word8] -> a
bytesToBits = toBits 0 where
  toBits acc 0 []     = acc
  toBits acc n (b:bs) = toBits (shiftL acc 8 .|. fromIntegral b) (n-1) bs

bitsToBytes :: (Bits a, Integral a) => Int -> a -> [Word8]
bitsToBytes n x = [fromIntegral (shiftR x ((n-i)*8) .&. 255) | i <- [1..n]]

ip4of :: IP4Addr -> IP4Net -> Bool
ip4of (IP4Addr addr) (IP4Net (IP4Addr netAddr) netBits) =
  let mask = complement (shiftL 1 (32 - netBits) - 1)
  in  addr .&. mask == netAddr .&. mask

-- QuickCheck

-- only generate strings for simplicity
instance Arbitrary Const where
  arbitrary = liftM SString (logSized (\n -> liftM show (choose (0, n))))
  coarbitrary = undefined

instance Arbitrary v => Arbitrary (Var v) where
  arbitrary = oneof [return Anon, liftM Named arbitrary]
  coarbitrary = undefined

instance Arbitrary v => Arbitrary (Term v) where
  arbitrary = oneof [liftM Var arbitrary, liftM Val arbitrary]
  coarbitrary = undefined

logSized f = sized (\n -> f (floor (log (fromIntegral (n+1)) / log 2) :: Int))

-- Grr--Word32 isn't an instance of Random, and Int may be smaller
instance Arbitrary Word32 where
  arbitrary = liftM (bytesToBits 4) (replicateM 4 arbitrary)
  coarbitrary = undefined

instance Arbitrary Word8 where
  arbitrary = liftM fromIntegral (choose (0,255::Int))
  coarbitrary = undefined

prop_ip4of :: Word32 -> Property
prop_ip4of addr = forAll (choose (0,32)) $ \netBits ->
                  forAll (choose (0,31)) $ \flipBit ->
                  ip4of (IP4Addr (addr `complementBit` flipBit))
                        (IP4Net (IP4Addr addr) netBits) ==
                    (netBits + flipBit < 32)

