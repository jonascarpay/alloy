{-# LANGUAGE TemplateHaskell #-}

module Eval.Lenses where

import Eval.Types
import Lens.Micro.Platform

makeLenses ''EvalEnv

rtProg ::
  Applicative m =>
  (var -> m var') ->
  (lbl -> m lbl') ->
  (fun -> m fun') ->
  (RTProg var lbl fun -> m (RTProg var' lbl' fun'))
rtProg fv fl ff = go
  where
    go (Decl typ val k) = Decl typ <$> rtVal fv fl ff val <*> rtProg (traverse fv) fl ff k
    go (Assign plc) = Assign <$> rtPlace fv fl ff plc
    go (Break lbl val) = Break <$> fl lbl <*> rtVal fv fl ff val
    go (ExprStmt val k) = ExprStmt <$> rtVal fv fl ff val <*> go k

rtVal ::
  Applicative m =>
  (var -> m var') ->
  (lbl -> m lbl') ->
  (fun -> m fun') ->
  (RTVal var lbl fun -> m (RTVal var' lbl' fun'))
rtVal fv fl ff = go
  where
    go (RBin op l r) = RBin op <$> go l <*> go r
    go (Call fun args) = Call <$> ff fun <*> traverse go args
    go (PlaceVal plc) = PlaceVal <$> rtPlace fv fl ff plc
    go (Block blk) = Block <$> rtProg fv (traverse fl) ff blk

rtPlace ::
  Applicative m =>
  (var -> m var') ->
  (lbl -> m lbl') ->
  (fun -> m fun') ->
  (RTPlace var lbl fun -> m (RTPlace var' lbl' fun'))
rtPlace fv fl ff = go
  where
    go (Place var) = Place <$> fv var
    go (Deref val) = Deref <$> rtVal fv fl ff val

rtProgLabels :: Traversal (RTProg var lbl fun) (RTProg var lbl' fun) lbl lbl'
rtProgLabels f = rtProg pure f pure
