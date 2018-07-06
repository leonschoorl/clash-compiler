{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016     , Myrtle Software Ltd,
                    2017     , Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Type and instance definitions for Rewrite modules
-}

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TemplateHaskell            #-}

module Clash.Rewrite.Types where

import Control.Concurrent.Supply             (Supply, freshId)
import Control.Lens                          (Getter, at, use, (&), (.=), (<<%=), (%=), (<%~), _1, to)
import Control.Monad
import Control.Monad.Fix                     (MonadFix (..), fix)
import Control.Monad.Reader                  (MonadReader (..))
import Control.Monad.State                   (MonadState (..))
import Control.Monad.Writer                  (MonadWriter (..))
import Data.HashMap.Strict                   (HashMap, adjust, alter, insertWith, singleton, unionWith)
import qualified Data.HashMap.Strict         as HashMap
import Data.HashSet                          (HashSet)
import Data.IntMap.Strict                    (IntMap)
import Data.Maybe                            (fromMaybe)
import Data.Monoid                           (Any)
import Text.Printf                           (printf)
import Unbound.Generics.LocallyNameless      (Fresh (..))
import Unbound.Generics.LocallyNameless.Name (Name (..))

import SrcLoc (SrcSpan)

import Clash.Core.Evaluator      (PrimEvaluator)
import Clash.Core.Name           (nameOcc)
import Clash.Core.Term           (Term, TmName, TmOccName)
import Clash.Core.Type           (Type)
import Clash.Core.TyCon          (TyCon, TyConName, TyConOccName)
import Clash.Core.Var            (Id, TyVar)
import Clash.Driver.Types        (BindingMap, DebugLevel)
import Clash.Netlist.Types       (HWType)
import Clash.Util

import Clash.Core.Pretty (showDoc)
import GHC.Stack
-- | Context in which a term appears
data CoreContext
  = AppFun Term      -- ^ Function position of an application
  | AppArg Term      -- ^ Argument position of an application
  | TyAppC           -- ^ Function position of a type application
  | LetBinding Id [Id] -- ^ RHS of a Let-binder with the sibling LHS'
  | LetBody    [Id]  -- ^ Body of a Let-binding with the bound LHS'
  | LamBody    Id    -- ^ Body of a lambda-term with the abstracted variable
  | TyLamBody  TyVar -- ^ Body of a TyLambda-term with the abstracted
                     -- type-variable
  | CaseAlt    [Id]  -- ^ RHS of a case-alternative with the variables bound by
                     -- the pattern on the LHS
  | CaseScrut        -- ^ Subject of a case-decomposition
  | CastBody         -- ^ Body of a Cast
  deriving (Eq,Show)

-- | A call graph counts the number of occurrences that a functions 'g' is used
-- in 'f'.
type CallGraph = HashMap TmOccName (HashMap TmOccName Word)

pprCallGraph :: CallGraph -> String
pprCallGraph gr = go $ map (fmap HashMap.toList) $ HashMap.toList gr
  where
    go :: [(TmOccName,[(TmOccName,Word)])] -> String
    go [] = ""
    go ((nm,calls):rest)
      = show nm ++ "  calls:\n"
        ++ pprCalls calls ++ go rest
    pprCalls [] = "\n"
    pprCalls ((nm,cnt):rest) = printf "%3dx %s\n" cnt (show nm) ++ pprCalls rest

-- | Add a call from f to g to the graph
addCall :: CallGraph -> TmOccName -> TmOccName -> CallGraph
addCall cg f g = alter addCallTo f cg
  where
    addCallTo :: Maybe (HashMap TmOccName Word) -> Maybe (HashMap TmOccName Word)
    addCallTo hm = case hm of
      Nothing -> Just $ singleton g 1
      Just h  -> Just $ insertWith (+) g 1 h

addCalls :: CallGraph -> TmOccName -> HashMap TmOccName Word -> CallGraph
addCalls cg f calls = insertWith (unionWith (+)) f calls cg

removeCalls :: CallGraph -> TmOccName -> HashMap TmOccName Word -> CallGraph
removeCalls cg f (HashMap.toList -> toRemove) = alter go f cg
  where
    go :: Maybe (HashMap TmOccName Word) -> Maybe (HashMap TmOccName Word)
    go (Just c) = Just $ foldl removeCallsInner c toRemove
    go _ = error $ $(curLoc) ++ "function " ++ show f ++ "not in callgraph."
    removeCallsInner :: HashMap TmOccName Word -> (TmOccName, Word) -> HashMap TmOccName Word
    removeCallsInner cg2 (nm,cnt) = alter (removeCallsActual cnt) nm cg2
      where
        removeCallsActual :: Word -> Maybe Word -> Maybe Word
        removeCallsActual cntRemove (fromMaybe 0 -> cntCurrent)
          | cntRemove > cntCurrent = error $ $(curLoc) ++ "try to remove " ++ show cntRemove ++ " calls from "++show f++" to "++ show nm ++", but the are only "++show cntCurrent ++ "."
          | otherwise = case cntCurrent - cntRemove of
                          0 -> Nothing
                          cnt -> Just cnt


-- TODO remove g if it's not called anymore?
-- | Remove a call from f to g from the graph
removeCall :: HasCallStack => CallGraph -> TmOccName -> TmOccName -> CallGraph
removeCall cg f g =
  let cg' = alter removeCallTo f cg
      callsRemaining = fromMaybe 0 $ join $ HashMap.lookup g <$> (HashMap.lookup f cg')
  in shout ("removing call : " ++ showDoc f ++ " to " ++ showDoc g ++ "\ncallsRemaining: " ++ show callsRemaining) $
     if callsRemaining > 0
     then cg'
     else callGraphRemoved cg' g
  where
    notFound = "ERROR can't remove a call from " ++ show f
               ++ " to " ++ show g ++ ", because there are none."
               ++ "\nCallgraph:\n" ++ pprCallGraph cg
    removeCallTo :: Maybe (HashMap TmOccName Word) -> Maybe (HashMap TmOccName Word)
    removeCallTo hm = case hm of
      Nothing -> error $ $(curLoc) ++ notFound
      -- Nothing -> shout ($(curLoc) ++ notFound) Nothing
      Just h  ->
                 -- let h' = alter removeCallTo2 g h
                 let (callLeft,h') = h & (at g) <%~ removeCallTo2
                 -- in if null h' then Nothing else Just h'
                 in Just h' -- leave empty call map
    removeCallTo2 :: Maybe Word -> Maybe Word
    removeCallTo2 wm = case wm of
      Nothing -> error $ $(curLoc) ++ notFound
      -- Nothing -> shout ($(curLoc) ++ notFound) Nothing
      Just w  -> case w-1 of
                   0  -> Nothing
                   w' -> Just w'

-- TODO if we keep using this, check the complexity of this, and check for recursion
filterTransitiveDeps :: CallGraph -> [TmOccName] -> CallGraph
filterTransitiveDeps cg = go
  where
    go :: [TmOccName] -> CallGraph
    go [] = HashMap.empty
    go (nm:nms) = HashMap.insert nm calls rest
      where
        Just calls = HashMap.lookup nm cg
        rest = go (HashMap.keys calls ++ nms)

callGraphRemoved :: CallGraph -> TmOccName -> CallGraph
callGraphRemoved cg g
  | any (HashMap.member g) cg = cg
  | otherwise =
    let moreCallsDeleted = HashMap.keys $ HashMap.lookupDefault HashMap.empty g cg
    in foldl callGraphRemoved (HashMap.delete g cg) moreCallsDeleted

-- | State of a rewriting session
data RewriteState extra
  = RewriteState
  { _transformCounter :: {-# UNPACK #-} !Int
  -- ^ Number of applied transformations
  , _bindings         :: !BindingMap
  -- ^ Global binders
  , _uniqSupply       :: !Supply
  -- ^ Supply of unique numbers
  , _curFun           :: (TmName,SrcSpan) -- Initially set to undefined: no strictness annotation
  -- ^ Function which is currently normalized
  , _nameCounter      :: {-# UNPACK #-} !Int
  -- ^ Used for 'Fresh'
  , _extra            :: !extra
  -- ^ Additional state
  , _callGraph        :: CallGraph -- TODO add '!' ?
  -- ^ Call graph
  }

makeLenses ''RewriteState

curFunOccName :: Getter (RewriteState extra) TmOccName
curFunOccName = curFun . _1 . to nameOcc

-- | Read-only environment of a rewriting session
data RewriteEnv
  = RewriteEnv
  { _dbgLevel       :: DebugLevel
  -- ^ Lvl at which we print debugging messages
  , _typeTranslator :: HashMap TyConOccName TyCon -> Bool -> Type
                    -> Maybe (Either String HWType)
  -- ^ Hardcode Type -> HWType translator
  , _tcCache        :: HashMap TyConOccName TyCon
  -- ^ TyCon cache
  , _tupleTcCache   :: IntMap TyConName
  -- ^ Tuple TyCon cache
  , _evaluator      :: PrimEvaluator
  -- ^ Hardcoded evaluator (delta-reduction)}
  , _allowZero      :: Bool
  -- ^ Zero bit wide things are representable
  , _topEntities    :: HashSet TmOccName
  -- ^ Functions that are considered TopEntities
  }

makeLenses ''RewriteEnv

-- | Monad that keeps track how many transformations have been applied and can
-- generate fresh variables and unique identifiers. In addition, it keeps track
-- if a transformation/rewrite has been successfully applied.
newtype RewriteMonad extra a = R
  { runR :: RewriteEnv -> RewriteState extra -> (a,RewriteState extra,Any) }

instance Functor (RewriteMonad extra) where
  fmap f m = R (\r s -> case runR m r s of (a,s',w) -> (f a,s',w))

instance Applicative (RewriteMonad extra) where
  pure  = return
  (<*>) = ap

instance Monad (RewriteMonad extra) where
  return a = R (\_ s -> (a, s, mempty))
  m >>= k  = R (\r s -> case runR m r s of
                          (a,s',w) -> case runR (k a) r s' of
                                        (b,s'',w') -> let w'' = mappend w w'
                                                      in seq w'' (b,s'',w''))

instance MonadState (RewriteState extra) (RewriteMonad extra) where
  get     = R (\_ s -> (s,s,mempty))
  put s   = R (\_ _ -> ((),s,mempty))
  state f = R (\_ s -> case f s of (a,s') -> (a,s',mempty))

instance Fresh (RewriteMonad extra) where
  fresh (Fn s _) = do
    n <- nameCounter <<%= (+1)
    let n' = toInteger n
    n' `seq` return (Fn s n')
  fresh nm@(Bn {}) = return nm

instance MonadUnique (RewriteMonad extra) where
  getUniqueM = do
    sup <- use uniqSupply
    let (a,sup') = freshId sup
    uniqSupply .= sup'
    a `seq` return a

instance MonadWriter Any (RewriteMonad extra) where
  writer (a,w) = R (\_ s -> (a,s,w))
  tell   w     = R (\_ s -> ((),s,w))
  listen m     = R (\r s -> case runR m r s of (a,s',w) -> ((a,w),s',w))
  pass   m     = R (\r s -> case runR m r s of ((a,f),s',w) -> (a, s', f w))

instance MonadReader RewriteEnv (RewriteMonad extra) where
   ask       = R (\r s -> (r,s,mempty))
   local f m = R (\r s -> runR m (f r) s)
   reader f  = R (\r s -> (f r,s,mempty))

instance MonadFix (RewriteMonad extra) where
  mfix f = R (\r s -> fix $ \ ~(a,_,_) -> runR (f a) r s)

-- | Monadic action that transforms a term given a certain context
type Transform m = [CoreContext] -> Term -> m Term

-- | A 'Transform' action in the context of the 'RewriteMonad'
type Rewrite extra = Transform (RewriteMonad extra)

removeCallM :: HasCallStack => TmOccName -> TmOccName -> RewriteMonad extra ()
removeCallM f g = do
  -- callGraph %= (\gr -> removeCall gr f g)
  graph <- use callGraph
  let graph' = removeCall graph f g
  callGraph .= graph'
  graph' `seq` return ()

removeCallToM :: HasCallStack => TmOccName -> RewriteMonad extra ()
removeCallToM g =
  do
    f <- use curFunOccName
    removeCallM f g

removeCallsM :: TmOccName -> HashMap TmOccName Word -> RewriteMonad extra ()
removeCallsM f calls = callGraph %= (\gr -> removeCalls gr f calls)

removeCallsToM :: HashMap TmOccName Word -> RewriteMonad extra ()
removeCallsToM calls =
  do
    f <- use curFunOccName
    removeCallsM f calls

addCallM :: TmOccName -> TmOccName -> RewriteMonad extra ()
addCallM f g = callGraph %= (\gr -> addCall gr f g)

addCallToM :: TmOccName -> RewriteMonad extra ()
addCallToM g =
  do
    f <- use curFunOccName
    addCallM f g

addCallsM :: TmOccName -> HashMap TmOccName Word -> RewriteMonad extra ()
addCallsM f calls = callGraph %= (\gr -> addCalls gr f calls)

addCallsToM :: HashMap TmOccName Word -> RewriteMonad extra ()
addCallsToM calls =
  do
    f <- use curFunOccName
    addCallsM f calls

lookupCallsFrom :: TmOccName -> RewriteMonad extra (HashMap TmOccName Word)
lookupCallsFrom f = do
  graph <- use callGraph
  case HashMap.lookup f graph of
    Just calls -> return calls
    Nothing -> error $ "lookupCallsFrom: " ++ show f ++ " doesn't not exist in the callgraph"

-- cleanupCallGraph :: CallGraph -> CallGraph
