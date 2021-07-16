{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import EvalTests
import RTTests
import Test.Tasty
import Test.Tasty.Focus

main :: IO ()
main =
  defaultMain $
    withFocus $
      testGroup
        "alloy-test"
        [ evalTests
        -- , rtTests
        ]
