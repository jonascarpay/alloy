{-# LANGUAGE TemplateHaskell #-}

module Eval.Lenses where

import Eval.Types
import Lens.Micro.Platform

makeLenses ''EvalEnv

--  TODO maybe just make a type class for these
rtProg ::
  Applicative m =>
  (var -> m var') ->
  (lbl -> m lbl') ->
  (fun -> m fun') ->
  (RTProg var lbl fun -> m (RTProg var' lbl' fun'))
rtProg fv fl ff = go
  where
    go (Decl typ val k) = Decl typ <$> rtValue fv fl ff val <*> rtProg (traverse fv) fl ff k
    go (Assign lhs rhs k) = Assign <$> rtPlace fv fl ff lhs <*> rtValue fv fl ff rhs <*> go k
    go (Break lbl val) = Break <$> fl lbl <*> rtValue fv fl ff val
    go (Continue lbl) = Continue <$> fl lbl
    go (ExprStmt val k) = ExprStmt <$> rtValue fv fl ff val <*> go k

rtValue ::
  Applicative m =>
  (var -> m var') ->
  (lbl -> m lbl') ->
  (fun -> m fun') ->
  (RTValue var lbl fun -> m (RTValue var' lbl' fun'))
rtValue fv fl ff = go
  where
    go (RTArith op l r) = RTArith op <$> go l <*> go r
    go (RTComp op l r) = RTComp op <$> go l <*> go r
    go (Call fun args) = Call <$> ff fun <*> traverse go args
    go (PlaceVal plc) = PlaceVal <$> rtPlace fv fl ff plc
    go (Block blk) = Block <$> rtProg fv (traverse fl) ff blk
    go (RTCond cond true false) = RTCond <$> go cond <*> go true <*> go false
    go (RTPrim p) = pure $ RTPrim p

rtPlace ::
  Applicative m =>
  (var -> m var') ->
  (lbl -> m lbl') ->
  (fun -> m fun') ->
  (RTPlace var lbl fun -> m (RTPlace var' lbl' fun'))
rtPlace fv fl ff = go
  where
    go (Place var) = Place <$> fv var
    go (Deref val) = Deref <$> rtValue fv fl ff val

rtProgLabels :: Traversal (RTProg var lbl fun) (RTProg var lbl' fun) lbl lbl'
rtProgLabels f = rtProg pure f pure

rtProgVars :: Traversal (RTProg var lbl fun) (RTProg var' lbl fun) var var'
rtProgVars f = rtProg f pure pure

rtValVars :: Traversal (RTValue var lbl fun) (RTValue var' lbl fun) var var'
rtValVars f = rtValue f pure pure

rtValLabels :: Traversal (RTValue var lbl fun) (RTValue var lbl' fun) lbl lbl'
rtValLabels f = rtValue pure f pure
