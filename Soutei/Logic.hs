{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}

-- $HeadURL: https://svn.metnet.navy.mil/svn/metcast/Mserver/trunk/soutei/haskell/Soutei/Logic.hs $
-- $Id: Logic.hs 2582 2010-08-09 23:33:02Z oleg.kiselyov $
-- svn propset svn:keywords "HeadURL Id" filename

module Soutei.Logic (
  Query,
  runQuery, incVarCount, withVarCount, envLookup, envUpdate, newVar, subst,
  unify, (==+), (&&+), (||+), true, false, liftBool,
  CtxQuery, PredQuery, ArgsQuery,
  prove, proveResults, clausesQuery, funcQuery
) where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Parallel.Strategies
import Data.Maybe (isJust)
import Test.QuickCheck

import Soutei.FBackTrackT (Stream, yield, runM)
import Soutei.Syntax (assertionL, headL)
import Soutei.Parsec (uncheckedParse)
import Soutei.Soutei

{-
  Overall evaluation strategy:

    1. Compile rules starting in parsed form (SynRule) by analyzing the
    patterns of variable use, turning every variable in the head into a
    HeadVar, and in the body to a BodyVar.  Both types contain
      - an offset that is unique to each variable, which will be used to
        assign a run-time variable
      - a description of its use pattern (below)

    2. The run-time approach is essentially the embedding of logic
    programming into a non-determinism monad, as in Seres and Spivey.  To
    translate rules into this embedding, we need to create fresh variable
    names each time a rule is instantiated (so that we don't alias with
    earlier instantiations).  So we keep an integer counter that is
    incremented on each instantiation, and give fresh names to rule
    variables by adding their offsets to the counter.
    
    We maintain an environment of variable bindings and use unification in
    the usual way.  However, by analyzing variable use patterns (details
    below), we perform some optimizations that minimize the size of the
    environment by avoiding unnecessary bindings.

  The result of compiling an assertion is not represented explicitly; rather
  the technique of run-time-compilation is used to achieve sharing of the
  compiled form.  Most of the *Query functions below are thus written for
  partial application.  The order of arguments is the same order in which
  the information becomes available.  There are basically three times we get
  more information:
    1. Submission of a new assertion.
    2. Start of query. (NB: we don't take advantage of this currently)
    3. Instantiation of a rule.
-}

{-
  The compiled form annotates each variable in the rule, according to how it
  is used.  (The annotations are used in optimizations, described above
  unifyHeadArg.)  Variables we may need to instantiate at run-time are
  numbered with a FrameVar, indicating (uniquely within the rule) their
  offset from the value of the counter when the rule is instantiated.  The
  rest of the annotations are different for variables in the head and in the
  body.

  An example that will clarify the explanation.  The following rule:

    a(?a, ?b, ?c, ?d, ?d) :-
     a(?a, ?b, ?b, ?c),
     a(?c, ?d, ?f, ?f, ?g),
     a(?g, ?e).

  compiles to:

    a(HeadOnly, HeadOnly, HeadOnly, HeadFst 0, HeadDup 3 0) :-
      a(Bound 0 Only, Bound 1 FstAtom 1, Bound 1 Dup 1, Bound 2 Fst 2),
      a(Bound 2 Dup 2, Bound 3 Dup 0, Free FstAtom 3, Free Dup 3, Free Fst 4),
      a(Free Dup 4, Free Only).

  Variables in the head (regardless of their use in the body) are tagged
  with one of the HeadVar constructors:

  - HeadOnly, the variable appears exactly once in the head.
  - HeadFst, the variable appears more than once in the head, and this is
    the first occurrence.
  - HeadDup, the variable appears more than once in the head, and this is
    not the first occurrence.  The BoundVar component refers to the position
    of the first occurrence.

  Variables in the body are either free (not appearing in the head) or bound
  (appearing in the head).  Bound variables have a BoundVar component that
  refers to the position of the first occurrence of that variable in the
  head.  Both free and bound variables (EXCEPT those that occur more than
  once in the head, see below) are tagged with one of the BodyVar
  constructors:

  - Only, the variable appears exactly once in the body.
  - FstAtom, the variable appears more than once in the body, and this is
    the first occurrence, and there is another occurrence in the same atom.
  - Fst, the variable appears more than once in the body, and this is the
    first occurrence, and there is no other occurrence in the same atom.
  - Dup, the variable occurs more than once in the body, or occurs more than
    once in the head (see below).

  As a special case, bound variables that occur more than once in the head
  are always tagged with Dup.

  For the use of these annotations, see unifyHeadArg.
-}

newtype BoundVar = BV Int -- index into head
                 deriving (Eq, Show)
newtype FrameVar = FV Int -- allocation offset
                 deriving (Eq, Show)

data HeadVar = HeadOnly                     -- once in head
             | HeadFst !FrameVar            -- first of many in head
             | HeadDup !BoundVar !FrameVar  -- duplicate in head
             deriving (Eq, Show)

data BodyVar = Bound !BoundVar !BodyUse
             | Free !BodyUse
             deriving (Eq, Show)

data BodyUse  = Only              -- only instance in body
              | FstAtom !FrameVar -- first instance in body, dup in same atom
              | Fst !FrameVar     -- first instance in body, no dup in same atom
              | Dup !FrameVar     -- another instance (or multiply in head)
              deriving (Eq, Show)

-- runtime infrastructure

newtype VarCount = VarCount Int      -- counter for fresh variable names
newtype RunVar = RV Int deriving Eq  -- variables become Ints at run-time
instance Show RunVar where
  showsPrec _ (RV n) = shows n

data ArgVar = ArgInit   { argToRun :: !RunVar } -- already initialized (in env)
            | ArgUninit { argToRun :: !RunVar } -- not initialized
            | ArgDoInit { argToRun :: !RunVar } -- must be initialized by head
            | ArgUnused                         -- ignore
            deriving (Eq, Show)

{-
  These types represent Query computations with some information missing.
  As this information is supplied, we partially apply as per run-time
  evaluation.
  - A ClausesQuery takes information about the assertion and produces an
    ArgsQuery.
    - The parsed form of the rules.
    - A continuation to instantiate a rule in the same assertion
      (constructed by "tying the knot").
  - An ArgsQuery takes information about the caller and assertion database,
    and produces a FrameQuery.
    - The arguments of the caller, in compiled form.
    - A continuation to instantiate a rule in another assertion.
  - A FrameQuery takes information about the instantiation ("frame") and
    produces a Query.
    - The values of the caller's caller's arguments.
    - The value of the variable counter that the caller was instantiated at.
    TODO: why is the frame not just the caller's arguments as ArgVars??
-}

type ClausesQuery m = [SynRule] -> PredQuery m -> ArgsQuery m
type ArgsQuery m = [Term BodyVar] -> CtxQuery m -> FrameQuery m
type FrameQuery m = Frame -> Query m
type Frame = ([Term ArgVar], VarCount)
-- A continuation to instantiate a predicate in the same context
type PredQuery m = Pred -> ArgsQuery m
-- A continuation to instantiate a predicate in another context
type CtxQuery m = Const -> Pred -> [Term BodyVar] -> FrameQuery m

prove :: Monad m => Const -> SynHeadAtom -> CtxQuery m -> Query m
prove ctx goal =
  let (Atom NoCtx p as, num) = compileGoal goal
  in  \ctxQ -> withVarCount $ \n -> incVarCount num &&+ ctxQ ctx p as ([], n)

proveResults :: Monad m => Const -> SynHeadAtom -> CtxQuery m -> QState ->
                  Stream m (Fact, QState)
proveResults ctx goal =
  let (Atom NoCtx p as, num) = compileGoalBindings goal
      q ctxQ = withVarCount $ \n -> incVarCount num &&+ ctxQ ctx p as ([], n)
  in  \ctxQ s@(n,_) -> do let as' = map (bodyToArg ([], n)) as
                          s'@(_,e') <- q ctxQ s
                          let r = Atom NoCtx p (map (Val . substInst e') as')
                          return (r, s')

clausesQuery :: forall m {- scoped type var -}. Monad m => ClausesQuery m
clausesQuery rules predQ =
  let rqs = map ruleQuery rules
  in  \as ctxQ frame -> let as' = map (bodyToArg frame) as
                        in  foldr (||+) false (map (\rq -> rq ctxQ as') rqs)
 where
  cost = length rules
  ruleQuery :: SynRule -> CtxQuery m -> [Term ArgVar] -> Query m
  ruleQuery (Rule (Atom _ _ as) b) =
    let ((head, body), num) = compile as b
        hq  = headQuery head
        bqs = map proveAtom body
    in  \ctxQ as -> withVarCount $ \n ->
          let frame = (as, n)
              bqs' = map (\bq -> bq ctxQ frame) bqs
          in  incVarCount num &&+
              hq frame &&+
              -- use foldr1 so the last atom is tail recursive; with foldr,
              -- we end up with "bq &&+ true", not tail recursive
              if null bqs' then true
                           else foldr1 (.) (replicate cost yield) .
                                foldr1 (&&+) bqs'
  headQuery :: [Term HeadVar] -> FrameQuery m
  headQuery head frame@(as, n) =
    maybeToStream $ foldr (&&+) true (zipWith' (unifyHeadArg frame) head as)
  proveAtom :: BodyAtom BodyVar -> CtxQuery m -> FrameQuery m
  proveAtom (Atom NothingCtx p as)  = predQ p as
  proveAtom (Atom (JustCtx c) p as) = \ctxQ frame ->
    let c' = bodyToArg frame c
    in  \s@(_,e) -> ctxQ (substInst e c') p as frame s
  -- ensure that both lists are forced to the end; using zipWith, the [] of
  -- the second list is not forced, causing us to retain a chain of frames
  zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
  zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
  zipWith' f []     []     = []

compile :: [SynTerm] -> [SynBodyAtom] ->
            (([Term HeadVar], [BodyAtom BodyVar]), Int)
compile h b = runState compile' 0 where
  compile' = do (h', headVars) <- compileHead h
                b' <- compileBody headVars b
                return (h', b')

compileHead :: [SynTerm] ->
          State Int ([Term HeadVar], [(SynVar, (HeadVar, FrameVar, BoundVar))])
compileHead h = mdo (h', (headVars, dups')) <-
                      runStateT (mapM (comp dups') (zip [0..] h)) ([], [])
                    return (h', headVars)
 where
  comp dups' (i, t) = fmapM comp' t where
    comp' Anon = error "should not mode check"
    comp' (Named v) = StateT $ \(headVars, dups) -> case lookup v headVars of
      Just (_, fv, bv) -> return (HeadDup bv fv, (headVars, v : dups))
      Nothing -> do fv <- newFV
                    let hv | v `elem` dups' = HeadFst fv
                           | otherwise      = HeadOnly
                    return (hv, ((v, (hv, fv, BV i)) : headVars, dups))

compileBody :: [(SynVar, (HeadVar, FrameVar, BoundVar))] -> [SynBodyAtom] ->
                  State Int [BodyAtom BodyVar]
compileBody headVars b =
  mdo (b', (_, dups')) <-
        runStateT (mapM (compileAtom (const Only) headVars dups') b) ([], [])
      return b'

-- freeOnlyf tells us how to treat vars that would be Free Only, in order to
-- prevent optimizing them away when we want bindings.
compileAtom :: FunctorM f => (FrameVar -> BodyUse) ->
                [(SynVar, (HeadVar, FrameVar, BoundVar))] ->
                [SynVar] -> f (Var SynVar) ->
                StateT ([(SynVar, FrameVar)], [SynVar]) (State Int) (f BodyVar)
compileAtom freeOnlyf headVars bodyDups' atom = mdo
  (atom', atomDups') <- runStateT (fmapM (comp atomDups') atom) []
  return atom'
 where
  comp atomDups' Anon = liftM (Free . freeOnlyf) (lift (lift newFV))
  comp atomDups' (Named v) = case lookup v headVars of
    Just (HeadFst _, fv, bv)  -> return (Bound bv (Dup fv))
    Just (HeadOnly, fv, bv)   -> liftM (Bound bv) (bu (return fv) (const Only))
    Nothing                   -> liftM Free (bu newFV freeOnlyf)
   where
    bu mfv onlyF = StateT $ \atomDups -> StateT $ \(bodyVars, bodyDups) ->
      case lookup v bodyVars of
        Just fv -> return ((Dup fv, v:atomDups), (bodyVars, v : bodyDups))
        Nothing -> do fv <- mfv
                      let bu | v `elem` atomDups' = FstAtom fv
                             | v `elem` bodyDups' = Fst fv
                             | otherwise          = onlyF fv
                      return ((bu, atomDups), ((v, fv) : bodyVars, bodyDups))

compileGoal' ca goal = runState (evalStateT (ca [] [] goal) ([], [])) 0

compileGoal, compileGoalBindings :: SynHeadAtom -> (HeadAtom BodyVar, Int)
compileGoal = compileGoal' (compileAtom (const Only))
compileGoalBindings = compileGoal' (compileAtom Fst)

newFV :: State Int FrameVar
newFV = State $ \n -> (FV n, n+1) `using` seqPair r0 rwhnf

{-
  In unifyHeadArg and bodyToArg, we make use of the HeadVar and BodyVar
  annotations created during rule compilation to avoid initializing (putting
  into the Query environment) every variable.  Keeping the environment small
  saves memory--we can even run non-terminating queries like "may(?a) :-
  may(?a)." in constant space--and speeds lookup.

  The obvious way to handle variables is to initialize every variable (with
  a fresh name) in the environment when a rule is instantiated.  The
  optimization is that in most cases, variables can be reused.  For example,
  if we have the rule

    a(?x) :- b(?x), c(?x).

  and try to prove a(?z), we can simply use ?z in place of ?x.  Many common
  usages can be optimized in a similar way.  In fact, it is the common cases
  that can most easily optimized.  We don't worry as much about optimizing
  unusual patterns, such as a variable appearing multiply in one atom (head
  or body), which could get more complicated.

  Key to implementation is the ArgVar type.  When a rule is instantiated,
  each of the variables in the caller is represented by an ArgVar,
  indicating 1) what RunVar it corresponds to, and 2) its initialization
  status at the time when it will be unified with the head.  There are four
  cases:
  - ArgInit v, corresponds to RunVar v, which is initialized
  - ArgUninit v, corresponds to RunVar v, which is not yet initialized
  - ArgDoinit v, corresponds to RunVar v, which is not yet initialized, but
    must be initialized during unification with the head
  - ArgUnused, never will be initialized (so does not correspond to any
    RunVar), effectively ignorable

  (Note: ArgDoInit is not strictly necessary; instead the caller could just
  do the initialization itself.  However, it would take extra care to avoid
  initializing before starting the body, which seems wasteful.)

  Here is how unification of a Term ArgVar with the corresponding argument
  in the head (unifyHeadArg) works:
  - If the head argument is a constant, we unify with it:
    - If the Term ArgVar is also a constant, simply compare the two.
    - If the ArgVar is already initialized, this is normal unification.
    - If the ArgVar is not initialized, initialize the RunVar unbound.
    - If the ArgVar is unused, unification succeeds trivially.
  - If the head argument is a variable that appears once in the head, we
    will alias it with the Term ArgVar; ie, subsequent uses of the variable
    will refer back to the caller.  This means we don't need to do any work
    during unification, except to fulfill the DoInit obligation.  In that
    case, we initialize the RunVar unbound.
  - If the head argument is the first of multiple appearances of a variable
    in the head, we must ensure that the ArgVar gets initialized for
    unification at later appearances.
    - If the Term ArgVar is a constant, do nothing.
    - If the ArgVar is not initialized, we initialize the RunVar unbound.
    - If the ArgVar is unused, we initialize a new RunVar using the offset
      of the head variable.
  - If the head argument is a subsequent appearance of a variable, we unify
    with the initialized RunVar (or constant) from the previous case
    (unifyArgArg).  argToRun' looks up the RunVar at which the previous
    ArgVar was initialized (or returns the constant, in that case).
    - If the Term ArgVar is a constant or initialized, unify with the
      previous Term RunVar.
    - If the ArgVar is not initialized, we initialize the RunVar with the
      previous Term RunVar.
    - If the ArgVar is unused, unification succeeds trivially.

  When a rule is instantiated, we must determine the ArgVars in the call to
  the rule in order to proceed with head unification.  To determine the
  ArgVar of a BodyVar argument, we need to refer to the Term ArgVars of the
  caller, and figure out the initialization status of the BodyVar based on
  knowledge of what will have already happened to the variable earlier in
  the head and the body.
  - If the BodyVar is bound, grab the ArgVar of the caller that corresponds
    to the first appearance of the variable in the head.  Recall that
    variables that appear multiply in the head are tagged Dup in the body,
    so except for the Dup case, we can assume that the variable appeared
    exactly once in the head.
    - If the ArgVar was already initialized or DoInit, it is now
      initialized, so becomes Init.
    - If the ArgVar was unused, then we treat the BodyVar as free.
    - If the ArgVar was not initialized, then
      - If it appeared multiply in the head, it will have been initialized
        per the HeadFst case in unification.
      - If it appeared previously in the body, it will have been
        initialized (this plus the previous case cover the Dup tag).
        - If it appeared in a previous atom, it will have been initialized
          (by well-modedness).
        - If it appeared for the first time as an argument in the same atom,
          it will have been initialized by the first appearance.
      - If it is the first appearance, and appears again in the same atom,
        it must be initialized during unification, so that subsequent
        appearances in the same atom can assume it was initialized.  So it
        gets tagged DoInit.
      - Otherwise, the initialization status remains UnInit.
  - If the BodyVar is free (or bound to an unused ArgVar):
    - If it is the only appearance, it can effectively be ignored, so gets
      tagged Unused.
    - If it is the first appearance, and appears again in the same atom, it
      must be initialized during unification, so that subsequent appearances
      in the same atom can assume it was initialized (previous case).  So it
      gets tagged DoInit.
    - If it is the first appearance, and appears again but not in the same
      atom, it gets tagged UnInit.  By well-modedness, it will be
      initialized by the time it is used in later atoms.
    - If it is a subsequent appearance, it will have been initialized, so
      gets tagged Init.
-}

-- Unify one head term with the corresponding argument in the caller.  The
-- Frame arg contains the caller's arguments and the current VarCount.
unifyHeadArg :: Frame -> Term HeadVar -> Term ArgVar -> Env -> Maybe Env
unifyHeadArg (as, n) = u where
  u (Val x)                   (Val x')            = liftBool (x == x')
  u (Val x)                   (Var (ArgInit v))   = unify (Var v) (Val x)
  u (Val x)                   (Var (ArgUninit v)) = envUpdate (v, (Val x))
  u (Val x)                   (Var (ArgDoInit v)) = envUpdate (v, (Val x))
  u (Val x)                   (Var ArgUnused)     = true
  u (Var HeadOnly)            (Var (ArgDoInit v)) = newVar v
  u (Var HeadOnly)            _                   = true
  u (Var (HeadFst n'))        (Var (ArgUninit v)) = newVar v
  u (Var (HeadFst n'))        (Var (ArgDoInit v)) = newVar v
  u (Var (HeadFst n'))        (Var ArgUnused)     = newVar (reloc n n')
  u (Var (HeadFst n'))        _                   = true
  u (Var (HeadDup (BV i) n')) t                   = unifyArgArg (as !! i) t
   where
    -- Unify two occurrences of a variable in the head.  First arg is the
    -- first occurrence.  The first occurrence must have already been
    -- unified with its argument.
    unifyArgArg :: Term ArgVar -> Term ArgVar -> Env -> Maybe Env
    unifyArgArg v (Val x)              = unify (argToRun' v) (Val x)
    unifyArgArg v (Var ArgUnused)      = true
    unifyArgArg v (Var (ArgInit v'))   = unify (argToRun' v) (Var v')
    unifyArgArg v (Var (ArgUninit v')) = envUpdate (v', argToRun' v)
    unifyArgArg v (Var (ArgDoInit v')) = envUpdate (v', argToRun' v)

    argToRun' :: Term ArgVar -> Term RunVar
    argToRun' (Val x)         = Val x
    argToRun' (Var ArgUnused) = Var (reloc n n')
    argToRun' (Var v)         = Var (argToRun v)

-- Frame arg contains the caller's arguments and the current VarCount.
bodyToArg :: Frame -> Term BodyVar -> Term ArgVar
bodyToArg _       (Val x) = Val x
bodyToArg (as, n) (Var v) = b2a v where
  b2a :: BodyVar -> Term ArgVar
  b2a (Bound (BV i) bu) = bound2a bu (as !! i) where
    -- Second arg is initialization status at the start of this rule.  Based
    -- on its BodyUse, it may have changed, so find the new status.
    bound2a :: BodyUse -> Term ArgVar -> Term ArgVar
    bound2a _             (Var (ArgDoInit v)) = Var (ArgInit v)
    bound2a (FstAtom n')  (Var (ArgUninit v)) = Var (ArgDoInit v)
    bound2a (Dup n')      (Var (ArgUninit v)) = Var (ArgInit v)
    bound2a bu            (Var ArgUnused)     = free2a bu
    bound2a _             t                   = t
  b2a (Free bu)         = free2a bu

  -- Find the initialization status of a free variable based on its BodyUse.
  free2a :: BodyUse -> Term ArgVar
  free2a Only         = Var ArgUnused
  free2a (FstAtom n') = Var (ArgDoInit (reloc n n'))
  free2a (Fst n')     = Var (ArgUninit (reloc n n'))
  free2a (Dup n')     = Var (ArgInit (reloc n n'))

reloc :: VarCount -> FrameVar -> RunVar
reloc (VarCount n) (FV n') = RV (n+n')

substInst :: Env -> Term ArgVar -> Const
substInst e t = case subst e (fmap argToRun t) of
                  Val ctx -> ctx
                  _       -> error "should not mode check"

funcQuery :: Monad m => ([Const] -> Bool) -> ArgsQuery m
funcQuery f as _ frame =
  let as' = map (bodyToArg frame) as
  in  \s@(_,e) -> if f (map (substInst e) as') then true s else false s

-- embedded logic language

{-
  - A Query takes the current run-time state.
    - The current value of the variable counter.
    - The binding environment.
-}

type Query m  = QState -> Stream m QState
type Binding  = (RunVar, Maybe (Term RunVar))
newtype Env   = Env [Binding]
              deriving Show
type QState    = (VarCount, Env)

runQuery :: (QState -> Stream m a) -> Stream m a
runQuery p    = p (VarCount 0, Env [])

withVarCount :: (VarCount -> Query m) -> Query m
withVarCount f s@(n,_) = f n s

incVarCount :: Monad m => Int -> Query m
incVarCount num (VarCount n, e) = return ((VarCount (n+num), e)
                                          `using` seqPair rwhnf r0)

maybeToStream :: Monad m => (Env -> Maybe Env) -> Query m
maybeToStream f (n,e) = case f e of
  Just e' -> return (n,e')
  Nothing -> mzero

envLookup :: Env -> RunVar -> Maybe (Term RunVar)
envLookup (Env e) v = case lookup v e of
  Just t  -> t
  Nothing -> error "envLookup: var missing from environment (missing newVar?)"

envUpdate :: (RunVar, Term RunVar) -> Env -> Maybe Env
envUpdate (v,x) (Env e) = Just (Env ((v, Just x) : e))

newVar :: RunVar -> Env -> Maybe Env
newVar v (Env e) = Just (Env ((v, Nothing) : e))

subst :: Env -> Term RunVar -> Term RunVar
subst e (Var v) = case envLookup e v of
  Just t  -> subst e t
  Nothing -> Var v
subst _ x@(Val _) = x

-- We can "shallow" unify because the terms have no structure
unify :: Term RunVar -> Term RunVar -> Env -> Maybe Env
unify t t' e              = unify' (subst e t) (subst e t') e where
  unify' t t' | t == t'   = return
  unify' (Var v) t        = envUpdate (v, t)
  unify' t (Var v)        = envUpdate (v, t)
  unify' _ _              = const mzero

infixr 4 ==+
(==+) = unify

infixr 3 &&+
(&&+) :: Monad m => (a -> m a) -> (a -> m a) -> (a -> m a)
p &&+ q = \s -> p s >>= q

infixr 2 ||+
(||+) :: MonadPlus m => (a -> m a) -> (a -> m a) -> (a -> m a)
p ||+ q = \s -> p s `mplus` q s

true, false :: MonadPlus m => a -> m a
true s  = return s
false s = mzero

liftBool :: MonadPlus m => Bool -> a -> m a
liftBool True   = true
liftBool False  = false

-- tests

testCompile = let [Rule (Atom _ _ as) b] =
                    uncheckedParse assertionL $ unlines [
                      "a(?w, 0, ?x, ?y, ?z, ?z) :- ",
                      " a(?w, ?x, ?x, ?y, ?z, ?a, ?, ?b, ?b, ?c),",
                      " a(?y, ?c)."]
                  ((head, b'), num) = compile as b
                  body = map (\(Atom _ _ as) -> as) b'
                  headExp = [Var HeadOnly,
                             Val (SNumber 0),
                             Var HeadOnly,
                             Var HeadOnly,
                             Var (HeadFst (FV 3)),
                             Var (HeadDup (BV 4) (FV 3))]
                  bodyExp = [[Var (Bound (BV 0) Only),
                              Var (Bound (BV 2) (FstAtom (FV 1))),
                              Var (Bound (BV 2) (Dup (FV 1))),
                              Var (Bound (BV 3) (Fst (FV 2))),
                              Var (Bound (BV 4) (Dup (FV 3))),
                              Var (Free Only),
                              Var (Free Only),
                              Var (Free (FstAtom (FV 6))),
                              Var (Free (Dup (FV 6))),
                              Var (Free (Fst (FV 7)))],
                             [Var (Bound (BV 3) (Dup (FV 2))),
                              Var (Free (Dup (FV 7)))]]
              in  do  assert (head == headExp)
                      assert (body == bodyExp)
                      assert (num == 8)

testCompileGoal = let goal = uncheckedParse headL "a(?a, ?, ?b, ?b)"
                      (Atom NoCtx _ args, num) = compileGoalBindings goal
                      argsExp = [Var (Free (Fst (FV 0))),
                                 Var (Free (Fst (FV 1))),
                                 Var (Free (FstAtom (FV 2))),
                                 Var (Free (Dup (FV 2)))]
                  in  do  assert (args == argsExp)
                          assert (num == 3)

-- Test HeadVar/BodyVar/ArgVar combinations.  Generate trees of well-moded
-- rules that use bound and free variables in different ways.

data R = R [SynTerm] B deriving Show
data B = Foo [([SynTerm], R)] | Bar deriving Show
varA = Var (Named (SynVar "a"))
varB = Var (Named (SynVar "b"))
val1 = Val (SNumber 1)

instance Arbitrary R where
  arbitrary = liftM2 R (twoElements [varA, val1]) arbitrary

instance Arbitrary B where
  arbitrary = oneof [arbFoos, return Bar] where
    arbFoos = do  first <- liftM (: [varA]) (elements terms)
                  r1 <- arbitrary
                  choose (False, True) >>= \b -> if b
                    then do second <- twoElements terms
                            r2 <- arbitrary
                            return (Foo [(first, r1), (second, r2)])
                    else return (Foo [(first, r1)])
    terms = [varA, varB, val1, Var Anon]

twoElements l = replicateM 2 (elements l)

rToRules (R as b) n = do
  n <- get
  put (n+1)
  (b', r') <- case b of
    Foo b -> liftM unzip (mapM bToAtom b)
    Bar   -> return ([Atom NothingCtx (Pred "bar" 1) [varA]],
                     [])
  return (Rule (Atom NoCtx (Pred ("foo" ++ show n) 2) as) b' :
          concat r')
bToAtom (as, r) = do  n <- get
                      r' <- rToRules r n
                      return (Atom NothingCtx (Pred ("foo" ++ show n) 2) as,
                              r')

prop_compCases r =
  let rules = Rule (Atom NoCtx (Pred "bar" 1) [val1]) [] :
              evalState (rToRules r 0) 0
      goal = Atom NoCtx (Pred "foo0" 2) [Var Anon, val1]
      envs = testRunProve' rules [] goal
  in  case envs of
        [Env e] -> all (\v -> subst (Env e) (Var v) == val1) (map fst e)
        _       -> False

-- This would loop if the unifier didn't check whether a variable was
-- assigned before assigning it to another variable.
testUnifyLoop   = assertSucc ["a(?x, ?y, ?y, ?x, ?x)."]
                              "a(?a, ?a, ?b, ?b, 0)"
-- Spins until it times out
testInfLoop     = assertFail ["a(?x) :- a(?x)."]
                              "a(?x)"
-- b spins forever, but a(0). is still reached
testInfLoopFair = assertSucc ["a(?x) :- a(?x).",
                              "a(0)."]
                              "a(?x)"
-- a(0). succeeds at every step of the loop.
testInfSucc     = assertSucc ["a(0).",
                              "a(?x) :- a(?x)."]
                              "a(?x)"
-- b is the disjunction of c, an infinite success, and d.  None of c's
-- successes lead to a success for a, but d is still reached.
testFairDisj    = assertSucc ["a(?x) :- b(?x), d(?x).",
                              "b(?x) :- c(?x).",
                              "b(?x) :- d(?x).",
                              "c(0).",
                              "c(?x) :- c(?x).",
                              "d(1)."]
                              "a(?x)"
-- b gives two answers, c gives infinite success for both, d rejects b's
-- first answer, but we still get to the second.
testFairConj    = assertSucc ["a(?x) :- b(?x), c(?y), d(?x).",
                              "b(0).",
                              "b(1).",
                              "c(0).",
                              "c(?x) :- c(?x).",
                              "d(1)."]
                              "a(?x)"

path = ["path(?x, ?y) :- path(?x, ?z), edge(?z, ?y).",
        "path(?x, ?y) :- edge(?x, ?y).",
        "edge(1, 2).",
        "edge(2, 3).",
        "edge(3, 4).",
        "edge(3, 1).",
        "edge(1, 5).",
        "edge(5, 4)."]
testPath = do
    assertSucc path "path(1, 2)"
    assertSucc path "path(1, 3)"
    assertSucc path "path(1, 4)"
    assertSucc path "path(1, 5)"
    assertSucc path "path(2, 1)"
    assertSucc path "path(2, 3)"
    assertSucc path "path(2, 4)"
    assertSucc path "path(2, 5)"
    assertSucc path "path(3, 1)"
    assertSucc path "path(3, 2)"
    assertSucc path "path(3, 4)"
    assertSucc path "path(3, 5)"
    assertFail path "path(4, 1)"
    assertFail path "path(4, 2)"
    assertFail path "path(4, 3)"
    assertFail path "path(4, 5)"
    assertFail path "path(5, 1)"
    assertFail path "path(5, 2)"
    assertFail path "path(5, 3)"
    assertSucc path "path(5, 4)"

testFuncQuery = do  assertSuccPrims [] appendPrim "append(a, bb, abb)"
                    assertFailPrims [] appendPrim "append(a, bb, ab)"
 where
  appendPrim = [(Pred "append" 3, funcQuery append)]
  append [SString s1, SString s2, SString s3] = s1 ++ s2 == s3
  append _ = False

test = do testCompile
          testCompileGoal
          testUnifyLoop
          testInfLoop
          testInfLoopFair
          testInfSucc
          testFairDisj
          testFairConj
          testPath
          testFuncQuery
          putStrLn "All tests passed"

assertSucc = assertEnv (not . null)
assertFail = assertEnv null
assertEnv f r g = assert (f (testRunProve r [] g))

assertSuccPrims = assertEnvPrims (not . null)
assertFailPrims = assertEnvPrims null
assertEnvPrims f r p g = assert (f (testRunProve r p g))

assert :: Bool -> IO ()
assert True  = putStrLn "assertion passed"
assert False = fail "assertion failed"

testRunProve :: [String] -> [(Pred, ArgsQuery Identity)] -> String -> [Env]
testRunProve r p g =  let rules = uncheckedParse assertionL (concat r)
                          goal = uncheckedParse headL g
                      in  testRunProve' rules p goal
testRunProve' r p g = testRun (prove sysCtx g (simpleRulesPrimsQuery r p))
testRun q = map snd (runIdentity (runM (Just 1000) Nothing (runQuery q)))

simpleRulesPrimsQuery :: Monad m => [SynRule] -> [(Pred, ArgsQuery m)] ->
                          CtxQuery m
simpleRulesPrimsQuery rules prims ctx p as frame =
      liftBool (ctx == sysCtx) &&+
        (clausesQuery (filter filtRule rules) predQ as ctxQ frame ||+
         foldr (||+) false (map (\(p, q) -> q as ctxQ frame)
                                (filter filtPrim prims)))
 where
  filtRule (Rule (Atom _ p' _) _) = p == p'
  filtPrim (p', _) = p == p'
  predQ p as ctxQ = ctxQ ctx p as
  ctxQ = simpleRulesPrimsQuery rules prims

-- QuickCheck (ideas taken from QuickCheck paper)

instance Arbitrary RunVar where
  arbitrary = liftM RV (logSized (\n -> choose (0, n)))

-- Generate a binding for every possible RunVar.
instance Arbitrary Env where
  arbitrary = liftM Env (logSized (\n -> foldM bind [] [0..n]))
   where
    bind e n = do v <- oneof [liftM (Just . Var) arbitrary,
                              liftM (Just . Val) arbitrary,
                              return Nothing]
                  pos <- choose (0, n)
                  return (insertAt pos (RV n, v) e)

insertAt 0 x' xs      = x':xs
insertAt n x' (x:xs)  = x : insertAt (n-1) x' xs

acyclic :: Env -> Bool
acyclic (Env e) = acyclic' e [] where
  acyclic' [] _                 = True
  acyclic' e@((v,t):_) checked  = chase v [] where
    chase v seen
      | v `elem` checked  = acyclic' (tail e) (seen ++ checked)
      | v `elem` seen     = False
      | otherwise         = case envLookup (Env e) v of
          Just (Var v')     -> chase v' (v:seen)
          _                 -> acyclic' (tail e) (v:seen ++ checked)

prop_acyclic :: Env -> Term RunVar -> Term RunVar -> Property
prop_acyclic e t1 t2 = acyclic e ==>
      let r = testRunUnify t1 t2 e
      in  not (null r) ==> case r of 
                              [e']  -> acyclic e'

prop_unify :: Env -> Term RunVar -> Term RunVar -> Property
prop_unify e t1 t2 = acyclic e ==>
      let r = testRunUnify t1 t2 e
      in  not (null r) ==> case r of 
            [e']  ->  let t1' = subst e' t1
                          t2' = subst e' t2
                      in  t1' == t2'

prop_setget :: Env -> RunVar -> Term RunVar -> Property
prop_setget e v t = acyclic e ==>
      case testRunUnify (Var v) t e of
            [e']  -> label "set" $
                      subst e' (Var v) == subst e' t
            []    -> label "did not set" $
                      case subst e (Var v) of
                        Var _ -> False
                        _     -> True

testRunUnify t1 t2 e = map snd (runIdentity (runM Nothing Nothing
                          (maybeToStream (unify t1 t2) (VarCount 0, e))))
