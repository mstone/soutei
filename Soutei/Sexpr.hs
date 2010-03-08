module Soutei.Sexpr (
  Sexpr(..),
  sexpr, cons, whiteSpace,
  toList, toAtomList, fromList, fromAtomList
) where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (newline)

data Sexpr a  = Atom a
              | Cons (Sexpr a) (Sexpr a)
              | Nil
              deriving (Eq)

instance Show a => Show (Sexpr a) where
  showsPrec _ (Atom x)      = shows x
  showsPrec _ l@(Cons x y)  = showParen True (shows x . showsTail y) where
    showsTail (Atom x)      = (" . "++) . shows x
    showsTail (Cons x y)    = (' ':) . shows x . showsTail y
    showsTail Nil           = id
  showsPrec _ Nil           = ("()"++)

-- do not eat trailing whitespace, because we want to process a request from
-- a lazy stream (eg socket) as soon as we see the closing paren.
sexpr :: Parser a -> Parser (Sexpr a)
sexpr p = liftM Atom p
      <|> cons p
sexprL p = lexeme (sexpr p)

cons :: Parser a -> Parser (Sexpr a)
cons p  = between (lexeme (string "(")) (string ")") tailL where
  tailL = do  dotL
              sexprL p
      -- require a space between consecutive atoms
      <|> liftM2 Cons (liftM Atom p)
                        (   (whiteSpace1 >> tailL)
                        <|> liftM2 Cons (consL p) tailL
                        <|> return Nil)
      <|> liftM2 Cons (consL p) tailL
      <|> return Nil
consL p = lexeme (cons p)

lexeme p    = do  r <- p
                  whiteSpace
                  return r
whiteSpace  = many space
whiteSpace1 = many1 space
dotL        = lexeme (string ".")

toList :: Monad m => Sexpr a -> m [Sexpr a]
toList Nil          = return []
toList (Cons x xs)  = liftM (x:) (toList xs)
toList (Atom x)     = fail "toList: not a pure list"

toAtomList :: Monad m => Sexpr a -> m [a]
toAtomList s        = toList s >>= mapM toAtom where
  toAtom (Atom x)   = return x
  toAtom _          = fail "toAtomList: element is not an atom"

fromList :: [Sexpr a] -> Sexpr a
fromList = foldr Cons Nil

fromAtomList :: [a] -> Sexpr a
fromAtomList = fromList . map Atom
