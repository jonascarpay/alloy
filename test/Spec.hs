{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import EvalTests
import RTTests
import Test.Hspec

main :: IO ()
main = hspec $ do
  evalTests
  rtTests
