{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Print (printNF) where

import Data.ByteString (ByteString)
import Eval.Types (NF)
import Print.Doc (toDoc)
import Print.Render (render)

printNF :: NF -> ByteString
printNF = render . toDoc
