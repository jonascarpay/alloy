module Eval.Typecheck (typeCheck) where

import Control.Monad.Reader
import Data.Void
import Eval.Lib
import Eval.Types
import Lens.Micro.Platform

valueType :: RTValue typ var lbl fun -> typ
valueType (RTArith _ _ _ t) = t
valueType (RTComp _ _ _ t) = t
valueType (RTCond _ _ _ t) = t
valueType (PlaceVal _ t) = t
valueType (Block _ t) = t
valueType (RTPrim _ t) = t
valueType (Call _ _ t) = t

placeType :: RTPlace typ var lbl fun -> typ
placeType (Place _ t) = t
placeType (Deref _ t) = t

type Check = ReaderT Deps (Either String)

typeCheck ::
  RTValue () (Bind Int Void) Void (Either FuncIX Hash) ->
  Deps ->
  Either String (RTValue Type (Bind Int Void) Void (Either FuncIX Hash))
typeCheck body = runReaderT (pure $ body & types .~ TInt)
