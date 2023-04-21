----------------------------------------------------------------------------
-- |
-- Module      :  TestMain
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns      #-}

module TestMain (main) where

import Control.Concurrent
import Control.Exception
import Data.IORef
import Data.Semigroup
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC

import Control.Concurrent.Counter.Lifted.IO qualified as C

import TestUtils

newtype Lock = Lock { _unLock :: C.Counter }

newLock :: IO Lock
newLock = Lock <$> C.new 0

acquire :: Lock -> IO ()
acquire (Lock c) = go
  where
    go = do
      !x <- C.cas c 0 1
      if x == 1
      then do
        -- Could try 'threadDelay 1' instead.
        yield
        go
      else pure ()

release :: Lock -> IO ()
release (Lock c) =
  C.set c 0

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
          res <- spawnAndCall ts (C.new 0) (\ref t -> runThread t (\_delay -> pure ()) (C.add ref)) >>= C.get
          let Sum expected =
                foldMap (\Thread{tIncrement, tIterations} -> Sum $ tIncrement * unIterations tIterations) ts
          pure $ res === expected

    , adjustOption (\(QC.QuickCheckTests x) -> QC.QuickCheckTests (max x 100000)) $
      QC.testProperty "Hand-made lock" $
        \(Threads ts) -> ioProperty $ do
          (ref, _lock) <- spawnAndCall ts ((,) <$> newIORef 0 <*> newLock) $ \(ref, lock) t ->
            runThread t (\_delay -> pure ()) $ \incr ->
              bracket_ (acquire lock) (release lock) $
                modifyIORef' ref (+ incr)
          res <- readIORef ref
          let Sum expected =
                foldMap (\Thread{tIncrement, tIterations} -> Sum $ tIncrement * unIterations tIterations) ts
          pure $ res === expected
    ]
