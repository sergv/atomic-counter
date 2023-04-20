----------------------------------------------------------------------------
-- |
-- Module      :  TestMain
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns      #-}

module TestMain (main) where

import Control.Concurrent
import Data.Semigroup
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC

import Control.Concurrent.Counter.Lifted.IO qualified as C

import TestUtils

main :: IO ()
main = do
  setNumCapabilities 1
  defaultMain $ testGroup "All"
    [ adjustOption (\(QC.QuickCheckTests x) -> QC.QuickCheckTests (max x 500)) $
      QC.testProperty "Correctness" $
        \(Threads ts) -> ioProperty $ do
          res <- spawnAndCall ts (C.new 0) (\ref t -> runThread t sleep (C.add ref)) >>= C.get
          let Sum expected =
                foldMap (\Thread{tIncrement, tIterations} -> Sum $ tIncrement * unIterations tIterations) ts
          pure $ res === expected
    , adjustOption (\(QC.QuickCheckTests x) -> QC.QuickCheckTests (max x 10000)) $
      QC.testProperty "Correctness, no delays" $
        \(Threads ts) -> ioProperty $ do
          res <- spawnAndCall ts (C.new 0) (\ref t -> runThread t (\_ -> pure ()) (C.add ref)) >>= C.get
          let Sum expected =
                foldMap (\Thread{tIncrement, tIterations} -> Sum $ tIncrement * unIterations tIterations) ts
          pure $ res === expected
    ]
