{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE UndecidableInstances #-}

-- $Id: Syntax.hs 2462 2010-02-04 05:03:57Z oleg.kiselyov $

module Soutei.Syntax (
  top, assertionL, headL, term, const
) where

import Prelude hiding (head, const)
import Control.Monad
import Data.Bits
import Data.Monoid (mconcat)
import Text.ParserCombinators.Parsec hiding (newline)

import Soutei.Parsec
import Soutei.Soutei

-- show

instance Show Const where
  -- if it can be parsed as a symbol, show it as a symbol (cosmetic)
  showsPrec _ (SString s) = case parseM symbol "" s of
                              Just s' | s' == s -> (s++)
                              _                 -> shows s
  showsPrec _ (SNumber n) = shows n
  showsPrec _ (SIP4Addr a) = ("#p"++) . shows a
  showsPrec _ (SIP4Net  n) = ("#n"++) . shows n

instance Show (ConstC v) where
  showsPrec _ (ConstC x) = shows x

instance Show IP4Addr where
  showsPrec _ addr = ip4AddrToBytes addr `showsSep` "."

instance Show IP4Net where
  showsPrec _ (IP4Net addr bits) = shows addr . ('/':) . shows bits

instance Show Pred where
  showsPrec _ (Pred pred arity) = (pred++) . ('/':) . shows arity

instance Show SynVar where
  showsPrec _ (SynVar v)  = (v++)

instance Show v => Show (Var v) where
  showsPrec _ Anon      = ('?':)
  showsPrec _ (Named v) = ('?':) . shows v

instance Show v => Show (Term v) where
  showsPrec _ (Val x) = shows x
  showsPrec _ (Var v) = shows v

instance (Show (c (Term v)), Show v) => Show (Atom c v) where
  showsPrec _ (Atom ctx (Pred pred arity) args) =
    shows ctx . (pred++) .  showParen True (args `showsSep` ", ")

instance Show v => Show (Rule v) where
  showsPrec _ (Rule a []) = shows a . (".\n"++)
  showsPrec _ (Rule a as) = shows a . (" :-\n    "++) .
                             (as `showsSep` ",\n    ") . (".\n"++)
  showList rules = mconcat (map shows rules)

instance Show (NoCtx a) where
  showsPrec _ NoCtx       = id

instance Show a => Show (MaybeCtx a) where
  showsPrec _ (JustCtx a) = shows a . (" says "++)
  showsPrec _ (NothingCtx)= id

showsSep :: Show a => [a] -> String -> ShowS
showsSep [x] _    = shows x
showsSep (x:xs) s = shows x . (s++) . showsSep xs s

-- parser

-- L suffix means a lexeme parser (eats trailing space); sort of annoying,
-- but I prefer to be explicit, because the distinction sometimes matters.

assertionL :: Parser [SynRule]
assertionL = many statementL
statementL :: Parser SynRule
statementL = do h <- headL
                b <-  do  inCaseL
                          bodyL
                      <|>
                      return []
                stopL
                return (Rule h b)
headL :: Parser SynHeadAtom
headL     = predAtomL NoCtx
bodyL :: Parser [SynBodyAtom]
bodyL     = atomL `sepBy` commaL
predAtomL :: c SynTerm -> Parser (SynAtom c)
predAtomL ctx = do  pred <- symbolL
                    args <- parensL (termL `sepBy` commaL)
                    return (Atom ctx (Pred pred (length args)) args)
ctxAtomL :: Parser SynBodyAtom
ctxAtomL  = do  ctx  <- term
                whiteSpace1
                says
                whiteSpace1
                predAtomL (JustCtx ctx)
atomL :: Parser SynBodyAtom
atomL     = try (predAtomL NothingCtx)
        <|> ctxAtomL
termL :: Parser SynTerm
termL     = lexeme term
term :: Parser SynTerm
term      = liftM Var var
        <|> liftM Val const

-- lexemes

top p       = between whiteSpace eof p
lexeme p    = do  r <- p
                  whiteSpace
                  return r
space'      = junk space <|> junk comment
whiteSpace  = many space'
whiteSpace1 = many1 space'
comment     = do  char ';'
                  manyTill anyChar (newline <|> eof)
              <?> "comment"
newline     = junk (char '\n')
          <|> junk (try (string "\r\n"))
          <|> junk (char '\r')

stopL       = lexeme (string ".")
inCaseL     = lexeme (string ":-")
commaL      = lexeme (string ",")
parensL p   = between (lexeme (string "(")) (lexeme (string ")")) p
says        = string "says"

var         = do  char '?'
                  liftM (Named . SynVar) symbol <|> return Anon
symbolL     = lexeme symbol
symbol      = do  c <- symbolStart
                  s <- many symbolChar
                  return (c:s)
              <?> "symbol"
-- based on Scheme, except disallow leading '?', and allow '@"
symbolStart = letter <|> oneOf "!$%&*/:<=>@~_^"
symbolChar  = symbolStart <|> digit <|> oneOf ".+-?"
          <?> "symbol character"
const       = liftM SString qString
          <|> liftM SString symbol
          <|> liftM SNumber number
          <|> liftM SIP4Addr ip4Addr
          <|> liftM SIP4Net ip4Net
          <?> "constant"
qString     = do  char '"'
                  manyTill ch (char '"') where
  ch        = do  char '\\'
                  c <- anyChar
                  case c of
                    'n'       -> return '\n'
                    '"'       -> return '"'
                    '\\'      -> return '\\'
                    otherwise -> fail "bad escape sequence"
          <|> anyChar
number      = natural
ip4Addr     = do  try (string "#p")
                  ip4Addr'
ip4Net      = do  try (string "#n")
                  addr <- ip4Addr'
                  char '/'
                  bits <- ip4NetBits
                  return (IP4Net addr bits)
ip4Addr'    = liftM bytesToIP4Addr (ip4AddrByte `sepBy4` char '.')
ip4AddrByte = liftM fromIntegral (naturalBet 0 255 "ip address octet")
ip4NetBits  = liftM fromIntegral (naturalBet 1 32 "ip address range bits")
natural :: Parser Integer
natural     = liftM read (many1 digit)
naturalBet n1 n2 what = do  n <- natural
                            when (n < n1 || n > n2)
                                 (fail ("bad " ++ what ++ " " ++ show n ++
                                  ", not between " ++ show n1 ++ " and "
                                  ++ show n2))
                            return n

sepByN :: Int -> Parser a -> Parser b -> Parser [a]
sepByN n p sep = sb n where
  sb n = liftM2 (:) p (sb' (n-1))
  sb' 0 = return []
  sb' n = sep >> sb n
sepBy4 = sepByN 4

-- tests

testParser :: String -> IO ()
testParser s = do a' <- parseM assertionL "test" s
                  let s' = show a'
                  if (s == s')
                    then putStrLn "parsed"
                    else fail ("parse error: " ++ show s ++ " -> " ++ show s')

testParserError :: String -> IO ()
testParserError s = case parse assertionL "test" s of
                      Left e  -> putStrLn "parse error"
                      Right x -> fail ("should not have parsed: " ++ s)

-- most tests cribbed from binder-parse.scm
test = do testParser ""
          testParserError "may"
          testParserError "may(1)"
          testParser "may(1).\n"
          testParserError "may(\")"
          testParserError "may(?, ?, ?)"
          testParser "may(?, ?, ?).\n"
          testParser "may(?, _, ?_).\n"
          testParser "may(?x, \"ok x\", ?zZ).\n"
          testParser ("may(?x, ?Y, ?) :-\n" ++
                      "    application says ip(?ip).\n")
          testParser ("may(2, ?Y, ?) :-\n" ++
                      "    ?Y says ip(?ip, #p10.0.0.1),\n" ++
                      "    ok(?ip).\n")
