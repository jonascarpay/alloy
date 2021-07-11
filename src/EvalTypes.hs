{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module EvalTypes where

import Bound.Scope.Simple
import Control.Applicative
import Control.Monad.Except
import Control.Monad.Writer
import Data.Functor.Identity
import Data.Hashable
import Data.Hashable.Lifted
import Data.IORef
import Data.Map (Map)
import Data.Void
import Expr
import GHC.Generics

data Value f a
  = VExt a
  | VClosure (Scope () Expr (Thunk (Lazy a)))
  | VRun Deps (CompVal a)
  | VFunc Deps FuncID
  | VType Type
  | VAttr (Map String (f (Value f a)))

newtype Hash = Hash Int
  deriving newtype (Eq, Ord, Hashable)

newtype Deps = Deps (Map Hash Fundef)
  deriving newtype (Semigroup, Monoid)

newtype Thunk a = Thunk (IORef (Either (Eval a) a))

type Lazy = Value Thunk

defer :: Eval a -> Eval (Thunk a)
defer = fmap Thunk . liftIO . newIORef . Left

refer :: a -> Eval (Thunk a)
refer = fmap Thunk . liftIO . newIORef . Right

force :: Thunk a -> Eval a
force (Thunk ref) = do
  liftIO (readIORef ref) >>= \case
    Right a -> pure a
    Left m -> do
      a <- m
      liftIO $ writeIORef ref (Right a)
      pure a

newtype Eval a = Eval (ExceptT String IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadError String)

type Comp = WriterT Deps Eval

newtype LabelID = LabelID Int
  deriving newtype (Hashable)

newtype FuncID = FuncID Hash
  deriving newtype (Hashable)

data Fundef = Fundef
  { funcArgs :: [Type],
    funcRet :: Type,
    funcBody ::
      Scope
        (Maybe Int)
        CompVal
        Void
  }
  deriving stock (Generic)
  deriving anyclass (Hashable)

newtype CompVal a
  = CompVal
      ( RVal
          (Place CompVal)
          (Either FuncID)
          CompProg
          a
      )
  deriving stock (Generic1)
  deriving anyclass (Hashable1)

newtype CompProg a
  = CompProg
      ( Prog
          (Either Type)
          (Place CompVal)
          CompVal
          (Either Label)
          a
      )
  deriving stock (Generic1)
  deriving anyclass (Hashable1)

describeValue :: Value f a -> String
describeValue VClosure {} = "closure"
describeValue VRun {} = "runtime expression"
describeValue VFunc {} = "runtime function"
describeValue VType {} = "type"
