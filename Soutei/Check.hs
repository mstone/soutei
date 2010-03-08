{-# OPTIONS -fglasgow-exts #-}

module Soutei.Check (
  ArgMode(..), TermMode(..), CtxModes,
  checkRules, groupRules
) where

import Prelude hiding (lookup)
import Control.Monad.Reader
import Control.Monad.Writer
import Data.List (transpose)
import Data.Map (Map, empty, lookup, member, insert, insertWith, fromList)

import Soutei.Parsec (uncheckedParse)
import Soutei.Soutei
import Soutei.Syntax

type CtxModes = Map Pred [ArgMode]

checkRules :: Monad m => CtxModes -> [SynRule] -> m (Map Pred [SynRule])
checkRules appModes rules = do  (m, preds) <- checkErrs (groupRules' rules)
                                checkErrs (checkUndefined m preds)
                                checkErrs (modeCheck appModes preds)
                                return m

groupRules :: Monad m => [SynRule] -> m (Map Pred [SynRule])
groupRules rules = liftM fst (checkErrs (groupRules' rules))

groupRules' :: [SynRule] -> Check (Map Pred [SynRule], [(Pred, [SynRule])])
groupRules' rules = group (empty, []) rules where
  group a [] = return a
  group (m, preds) (rule@(Rule (Atom _ pred _) _) : rules) = do
    when (member pred m)
      (err ("multiple definitions of " ++ show pred))
    groupPred (rule:) rules
   where
    groupPred l (rule@(Rule (Atom _ pred' _) _) : rules)
      | pred' == pred = groupPred (l . (rule:)) rules
    groupPred l rules = let clauses = l []
                        in  group (insert pred clauses m,
                                   (pred, clauses):preds) rules

checkUndefined :: Map Pred [SynRule] -> [(Pred, [SynRule])] -> Check ()
checkUndefined m preds = mapEnterClauseM_ (mapEnterM_ checkRule) preds where
  checkRule (Rule h b) = mapEnterM_ checkAtom b
  checkAtom (Atom NothingCtx pred _) =
    when (not (member pred m))
      (errPos ("undefined predicate " ++ show pred))
  checkAtom _ = return ()

-- mode checker

data ArgMode = R TermMode | P TermMode deriving Eq
data TermMode = L | S deriving (Eq, Ord)

instance Ord ArgMode where
  R m `compare` R m' = m' `compare` m
  R _ `compare` P _  = LT
  P _ `compare` R _  = GT
  P m `compare` P m' = m `compare` m'

instance Show TermMode where
  showsPrec _ L = ("range limited"++)
  showsPrec _ S = ("statically range limited"++)

type ModeEnv = Map SynVar TermMode

{-
   At the top level, we pass in a data structure that maps built-in predicates onto their 
   argument modes; this never changes.
   
   For each predicate:
      First, mode-check each clause:
        For each body atom:
         Look at the context for the atom: 
            * If it's "application", then either the predicate is
         built-in (so we look up its argument modes) or it's not (so
         we assign all arguments the mode (P S): we're saying that in
         the application namespace, all predicates force their
         arguments to be statically limited.)
            * If the context is something that's not "application", then
          we assign all arguments the mode (P L): in any namespaces other
          than "application", all predicates bind their arguments (but not
          necessarily to a statically-known value).
            * If there is no context, then as in the "application" rule, we
         look up the predicate in the data structure that tells us the modes
         for predicates in the current context; if it's found, then we know
         the argument modes, and if not, we assign all arguments (P L).

         Now that we have a mode for each argument, for each (arg, mode) pair,
         we do the following checks:
             * If arg is an anonymous variable, and the mode requires anything,
             we report an error because an anonymous variable can't have a mode.
             * If arg is a named variable and the mode is provided, then we
             insert (arg, mode) into the environment if mode is a refinement on
             whatever mode arg is already bound to in the environment.
             * If arg is a named variable and the mode is required, then we look
             up arg in the environment and check that the mode that's already in
             the environment contains at least as much information as mode does.
            
      Second, determine the modes for the head, using the information we just
      computed about the variables in the body. The goal is to ensure that
      every argument in the head provides some mode.
         We create a list of modes corresponding to each argument in the head:
           * If the argument is a constant, then the mode is (P S): that is, this
           rule provides a statically known value for that argument.
           * If the argument is an anonymous variable, then we report an error,
           because we can't have anonymous variables in the head: an anonymous
           variable can't provide a mode.
           * If the argument is a named variable, then look up its mode and indicate
           that it provides that mode. If the mode isn't found in the environment,
           then we report an error.

      Third, now that for each predicate, we have a mode list for each of the rules
      that combine it, we combine these lists together so that we have a single mode
      list for each predicate, by taking the lower bound of each element in the list
      over all of the defining rules.

   Now that we know the argument modes for each predicate,
   iterate with this new information, until none of the modes
   for any of the predicates change.

-}

modeCheck :: CtxModes -> [(Pred, [SynRule])] -> Check CtxModes
modeCheck appModes preds = iter (iterEnterClause addPred preds)
 where
  iter :: (CtxModes -> Check CtxModes) -> Check CtxModes
  iter f = iter' empty where
    iter' m = let (m', errs) = runWriter (f m)
              in  if m' == m then tell errs >> return m
                             else iter' m'

  addPred :: (Pred, [SynRule]) -> CtxModes -> CheckPos PredPos CtxModes
  addPred (pred, clauses) m = liftM (addPredMode pred m) (checkPred m clauses)

  addPredMode :: Pred -> CtxModes -> Maybe [ArgMode] -> CtxModes
  addPredMode pred m (Just argModes) = insert pred argModes m
  addPredMode _    m Nothing         = m

  checkPred :: CtxModes -> [SynRule] -> CheckPos PredPos (Maybe [ArgMode])
  checkPred modes clauses = liftM lwb (mapEnterM checkRule clauses)
   where
    lwb :: [Maybe [ArgMode]] -> Maybe [ArgMode]
    lwb clauseModes = fmap (map minimum . transpose) (sequence clauseModes)

    checkRule :: SynRule -> CheckPos ClausePos (Maybe [ArgMode])
    checkRule (Rule (Atom NoCtx _ args) b) = do
      e <- iterEnter checkAtom b empty
      liftM sequence (mapEnterM (checkArg e) args)
     where
      checkArg :: ModeEnv -> SynTerm -> CheckPos HeadArgPos (Maybe ArgMode)
      checkArg e t@(Val _)          = return (Just (P S))
      checkArg e t@(Var Anon)       = noMode (show t ++ " in head")
      checkArg e t@(Var (Named v))  = case lookup v e of
        Just m  -> return (Just (P m))
        Nothing -> noMode ("Can not derive mode for head term " ++ show t)
      noMode msg = errPos msg >> return Nothing

    checkAtom :: SynBodyAtom -> ModeEnv -> CheckPos BodyPos ModeEnv
    checkAtom (Atom ctx pred args) e =
      let argModes = case ctx of
            JustCtx (Val (SString "application")) ->
              case lookup pred appModes of
                Just argModes -> argModes
                Nothing       -> repeat (P S)
            NothingCtx ->
              case lookup pred modes of
                Just argModes -> argModes
                Nothing -> repeat (P L)
            _ -> repeat (P L)
      in  case ctx of
            JustCtx ctx -> withReaderT enterBodyCtx (checkArg ctx (R L) e)
            NothingCtx  -> return e
          >>= iterEnter' checkArg (zip args argModes)
     where
      checkArg :: Show pos => SynTerm -> ArgMode -> ModeEnv ->
                    CheckPos pos ModeEnv
      checkArg t@(Val _)          _     e = return e
      checkArg t@(Var Anon)       (P _) e = return e
      checkArg t@(Var Anon)       (R m) e = expected t Nothing m >> return e
      checkArg t@(Var (Named v))  (P m) e = return (insertWith max v m e)
      checkArg t@(Var (Named v))  (R m) e = case lookup v e of
                                    Just m' | m' >= m -> return e
                                    m' -> expected t m' m >> return e
      expected :: Show pos => SynTerm -> Maybe TermMode -> TermMode ->
                    CheckPos pos ()
      expected t m m' = errPos (show t ++ showMaybe m ++ " found where " ++
                                show m' ++ " expected")
      showMaybe Nothing = " (mode unknown)"
      showMaybe (Just x) = " (" ++ show x ++ ")"

-- A monad that helps with error reporting.
-- Position stuff is a mess.

type Check a = Writer Errors a
type CheckPos pos a = ReaderT pos (Writer Errors) a
type Errors = [String] -> [String]

data PredPos    = PredPos Pred
data ClausePos  = ClausePos PredPos Int
data HeadArgPos = HeadArgPos ClausePos Int
data BodyPos    = BodyPos ClausePos Int
data BodyCtxPos = BodyCtxPos BodyPos
data BodyArgPos = BodyArgPos BodyPos Int

instance Show PredPos where
  showsPrec _ (PredPos pred)    = shows pred
instance Show ClausePos where
  showsPrec _ (ClausePos p n)   = shows p . (", clause "++) . shows n
instance Show HeadArgPos where
  showsPrec _ (HeadArgPos p n)  = shows p . (", arg "++) . shows n
instance Show BodyPos where
  showsPrec _ (BodyPos p n)     = shows p . (", atom "++) . shows n
instance Show BodyCtxPos where
  showsPrec _ (BodyCtxPos p)    = shows p . (", context"++)
instance Show BodyArgPos where
  showsPrec _ (BodyArgPos p n)  = shows p . (", arg "++) . shows n

class Enter pos pos' obj | pos' -> pos obj, pos obj -> pos' where
  enter :: pos -> Int -> pos'

instance Enter PredPos ClausePos SynRule where enter = ClausePos
instance Enter ClausePos HeadArgPos (SynTerm) where enter = HeadArgPos
instance Enter ClausePos BodyPos (SynBodyAtom) where enter = BodyPos
instance Enter BodyPos BodyArgPos (SynTerm) where enter = BodyArgPos

enterPred         = PredPos
enterBodyCtx      = BodyCtxPos

mapEnter :: Enter pos pos' obj => (obj -> CheckPos pos' a) -> [obj] ->
              [CheckPos pos a]
mapEnter f xs = zipWith body [1..] xs where
  body n x = withReaderT (\pos -> enter pos n) (f x)
mapEnterM f xs = sequence (mapEnter f xs)
mapEnterM_ f xs = sequence_ (mapEnter f xs)

iterEnter :: Enter pos pos' obj => (obj -> a -> CheckPos pos' a) -> [obj] ->
              (a -> CheckPos pos a)
iterEnter f xs = iter (zipWith body [1..] xs) where
  body n x z = withReaderT (\pos -> enter pos n) (f x z)

iterEnter' :: Enter pos pos' obj => (obj -> b -> a -> CheckPos pos' a) ->
              [(obj, b)] -> (a -> CheckPos pos a)
iterEnter' f xs = iter (zipWith body [1..] xs) where
  body n (x, y) z = withReaderT (\pos -> enter pos n) (f x y z)

mapEnterClause :: ([SynRule] -> CheckPos PredPos a) ->
                  [(Pred, [SynRule])] -> [Check a]
mapEnterClause f preds = map body preds where
  body (pred, clauses) = runReaderT (f clauses) (enterPred pred)
mapEnterClauseM f preds = sequence (mapEnterClause f preds)
mapEnterClauseM_ f preds = sequence_ (mapEnterClause f preds)

iterEnterClause :: ((Pred, [SynRule]) -> a -> CheckPos PredPos a) ->
                   [(Pred, [SynRule])] -> a -> Check a
iterEnterClause f preds = iter (map body preds) where
  body p@(pred, _) z = runReaderT (f p z) (enterPred pred)

err :: String -> Check ()
err msg = tell (msg :)

errPos :: Show pos => String -> CheckPos pos ()
errPos msg = ask >>= \pos -> tell ((show pos ++ ": " ++ msg) :)

checkErrs :: Monad m => Check a -> m a
checkErrs m = let (r, errs) = runWriter m in case errs [] of
                []    -> return r
                errs  -> fail (foldr1 (\s1 s2 -> s1 ++ "\n" ++ s2) errs)

iter :: Monad m => [a -> m a] -> (a -> m a)
iter = foldr (\f g x -> f x >>= g) return

-- tests

undefT      = (1, ["foo(a) :- bar(a)."])
anonHeadT   = (1, ["foo(?)."])
unusedHeadT = (1, ["foo(?a)."])
rsT         = (1, ["foo(1) :- application says rs(?a)."])
rsT'        = (2, ["foo(?a) :- application says rs(?a)."])
rsAnonT     = (1, ["foo(1) :- application says rs(?)."])
rlT         = (1, ["foo(1) :- application says rl(?a)."])
rlT'        = (2, ["foo(?a) :- application says rl(?a)."])
rlAnonT     = (1, ["foo(1) :- application says rl(?)."])
saysT       = (1, ["foo(1) :- ?a says foo(1)."])
saysAnonT   = (1, ["foo(1) :- ? says foo(1)."])
saysOkT     = (0, ["foo(1) :- someone says foo(?a),\
                             \?a says foo(1)."])
rsFactT     = (0, ["foo(1).",
                   "bar(?a) :- foo(?a), application says rs(?a)."])
rsAppFactT  = (0, ["bar(?a) :- application says foo(?a),\
                              \application says rs(?a)."])
rlRuleT     = (0, ["bar(?a) :- someone says foo(?a),\
                              \application says rl(?a)."])
rsRuleT     = (1, ["bar(?a) :- someone says foo(?a),\
                              \application says rs(?a)."])
rsChainT    = (0, ["foo(1).",
                   "bar(?a) :- foo(?a).",
                   "baz(?a) :- bar(?a), application says rs(?a)."])
multiT      = (1, ["foo(1).",
                   "foo(?x) :- foo(?x).",
                   "bar(?a) :- foo(?a), application says rs(?a)."])
improveT    = (0, ["foo(1).",
                   "bar(?a) :- bar(?a), foo(?a).",
                   "baz(?a) :- bar(?a), application says rs(?a)."])
deproveT    = (0, ["foo(1).",
                   "bar(?a) :- foo(?a), bar(?a).",
                   "baz(?a) :- bar(?a), application says rs(?a)."])

test = do assertErrs anonHeadT
          assertErrs unusedHeadT
          assertErrs rsT
          assertErrs rsT'
          assertErrs rsAnonT
          assertErrs rlT
          assertErrs rlT'
          assertErrs rlAnonT
          assertErrs saysT
          assertErrs saysAnonT
          assertErrs saysOkT
          assertErrs rsFactT
          assertErrs rsAppFactT
          assertErrs rlRuleT
          assertErrs rsRuleT
          assertErrs rsChainT
          assertErrs multiT
          assertErrs improveT
          assertErrs deproveT

assertErrs :: (Int, [String]) -> IO ()
assertErrs (n, ps) = assert (length (testCheck ps) == n)

assert :: Bool -> IO ()
assert True  = putStrLn "assertion passed"
assert False = fail "assertion failed"

testCheck :: [String] -> [String]
testCheck s = let rules = uncheckedParse assertionL (concat s)
                  appModes = fromList [(Pred "rs" 1, [R S]),
                                       (Pred "rl" 1, [R L])]
                  (_, errs) = runWriter $ do  (m, preds) <- groupRules' rules
                                              checkUndefined m preds
                                              modeCheck appModes preds
              in  errs []

