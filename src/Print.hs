{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Print (printNF) where

import Data.Text (Text)
import Eval.Types (NF)
import Print.Doc (toDoc)
import Print.Render (render)

printNF :: NF -> Text
printNF = render . toDoc
