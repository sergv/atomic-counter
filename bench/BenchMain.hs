----------------------------------------------------------------------------
-- |
-- Module      :  BenchMain
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE UnboxedTuples       #-}

module BenchMain (main) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.IORef
import Data.Primitive.Types
import Data.Semigroup
import GHC.Exts
import GHC.IO
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.Bench
import Test.Tasty.QuickCheck qualified as QC
import Test.Tasty.Runners qualified as Tasty

import Control.Concurrent.Counter.Lifted.IO qualified as C

import TestUtils

#if MIN_VERSION_base(4, 16, 0)
readAddr :: Addr# -> Word
readAddr addr = W# (indexWordOffAddr# addr 0#)

withAddr :: (Addr# -> IO a) -> IO a
withAddr f = IO $ \s1 -> case newPinnedByteArray# (sizeOf# (undefined :: Word)) s1 of
  (# s2, mbarr #) ->
    let !addr = mutableByteArrayContents# mbarr in
      case writeWordOffAddr# addr 0# 0## s2 of
        s3 -> case unIO (f addr) s3 of
          (# s4, res #) -> case touch# mbarr s4 of
            s5 -> (# s5, res #)

incrementAddr :: Addr# -> Int -> IO ()
incrementAddr addr (I# delta) = IO $ \s1 -> case fetchAddWordAddr# addr (int2Word# delta) s1 of
  (# s2, _ #) -> (# s2, () #)
#endif

incrementIORef :: IORef Int -> Int -> IO ()
incrementIORef !x !delta = atomicModifyIORef' x (\old -> (old + delta, ()))

incrementIORefInconsistent :: IORef Int -> Int -> IO ()
incrementIORefInconsistent !x !delta = do
  n <- readIORef x
  writeIORef x $! n + delta

incrementMVar :: MVar Int -> Int -> IO ()
incrementMVar !x !delta = do
  !n <- takeMVar x
  putMVar x $! n + delta

incrementTMVar :: TMVar Int -> Int -> IO ()
incrementTMVar !x !delta = atomically $ do
  !n <- takeTMVar x
  putTMVar x $! n + delta

incrementTVar :: TVar Int -> Int -> IO ()
incrementTVar !x !delta = atomically $ modifyTVar' x (+ delta)

incrementCounter :: C.Counter -> Int -> IO ()
incrementCounter !x !delta = void (C.add x delta)


main :: IO ()
main = do
  let tests =
        [ localOption (QC.QuickCheckTests 10000) $
          QC.testProperty "Correctness" $
            \(Threads ts) -> ioProperty $ do

              a <- spawnAndCall ts (newIORef 0)   (\ref t -> runThread t (\_ -> pure ()) (incrementIORef ref)) >>= readIORef
              b <- spawnAndCall ts (newMVar 0)    (\ref t -> runThread t (\_ -> pure ()) (incrementMVar ref)) >>= takeMVar
              c <- spawnAndCall ts (newTMVarIO 0) (\ref t -> runThread t (\_ -> pure ()) (incrementTMVar ref)) >>= atomically . takeTMVar
              d <- spawnAndCall ts (newTVarIO 0)  (\ref t -> runThread t (\_ -> pure ()) (incrementTVar ref)) >>= atomically . readTVar
              -- e <- spawnAndCall ts (newIORef 0)   (\ref t -> runThread t (incrementIORefInconsistent ref *> sleep delay)) >>= readIORef
#if MIN_VERSION_base(4, 16, 0)
              f <- withAddr $ \addr -> do
                spawnAndCall ts (pure ()) (\() t -> runThread t (\_ -> pure ()) (incrementAddr addr))
                evaluate (readAddr addr)
#endif

              g <- spawnAndCall ts (C.new 0) (\ref t -> runThread t (\_ -> pure ()) (incrementCounter ref)) >>= C.get

              let Sum expected =
                    foldMap (\Thread{tIncrement, tIterations} -> Sum $ tIncrement * unIterations tIterations) ts

              pure $
                a === expected .&&.
                b === expected .&&.
                c === expected .&&.
                d === expected .&&.
#if MIN_VERSION_base(4, 16, 0)
                fromIntegral f === expected .&&.
#endif
                g === expected
        ]

  let benchmarks =
        [ bgroup ("Read/write contention with " ++ show (unIterations n) ++ " iterations and " ++ show (length threads) ++ " threads")
          [ bench "Counter" $
            whnfIO (spawnAndCall threads (C.new 0)      (\ref _ -> callN n (incrementCounter ref 1)))
          , bench "IORef inconsistent" $
            whnfIO (spawnAndCall threads (newIORef 0)   (\ref _ -> callN n (incrementIORefInconsistent ref 1)))
          , bench "IORef atomic" $
            whnfIO (spawnAndCall threads (newIORef 0)   (\ref _ -> callN n (incrementIORef ref 1)))
          , bench "MVar" $
            whnfIO (spawnAndCall threads (newMVar 0)    (\ref _ -> callN n (incrementMVar ref 1)))
          , bench "TMVar" $
            whnfIO (spawnAndCall threads (newTMVarIO 0) (\ref _ -> callN n (incrementTMVar ref 1)))
          , bench "TVar" $
            whnfIO (spawnAndCall threads (newTVarIO 0)  (\ref _ -> callN n (incrementTVar ref 1)))
#if MIN_VERSION_base(4, 16, 0)
          , bench "Addr" $
            whnfIO $ withAddr $ \addr -> do
              spawnAndCall
                threads
                (pure ())
                (\() _ -> callN n (incrementAddr addr 1))
              evaluate (readAddr addr)
#endif
          ]
        | repeats <- [1, 2, 4, 6, 8, 12, 16, 20, 32, 64, 128]
        , let threads = [1..repeats]
        , n <- [Iterations 10, Iterations 100, Iterations 1000, Iterations 10000]
        ]

  defaultMainWithIngredients benchIngredients $
    localOption (Tasty.NumThreads 1) $
      testGroup "All" $
      tests ++ benchmarks
