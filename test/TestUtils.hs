----------------------------------------------------------------------------
-- |
-- Module      :  TestUtils
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}

module TestUtils
  ( Delay(..)
  , sleep
  , Iterations(..)
  , callN
  , Thread(..)
  , runThread
  , Threads(..)
  , spawnAndCall
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Generics (Generic)
import Test.QuickCheck

-- In microseconds
newtype Delay = Delay { unDelay :: Int }
  deriving (Eq, Show)

sleep :: MonadIO m => Delay -> m ()
sleep (Delay n) = case n of
  0 -> pure ()
  k -> liftIO $ threadDelay k

instance Arbitrary Delay where
  arbitrary = Delay <$> chooseInt (0, 10)
  shrink = map Delay . filter (\x -> 0 <= x && x <= 25) . shrink . unDelay

newtype Iterations = Iterations { unIterations :: Int }
  deriving (Eq, Show)

instance Arbitrary Iterations where
  arbitrary = Iterations <$> chooseInt (0, 50)
  shrink = map Iterations . filter (\x -> 0 <= x && x <= 50) . shrink . unIterations

callN :: Applicative m => Iterations -> m a -> m ()
callN (Iterations !n) action = go n
  where
    go !k =
      if k > 0
      then action *> go (k - 1)
      else pure ()

data Thread = Thread
  { tDelay      :: Delay
  , tIncrement  :: Int
  , tIterations :: Iterations
  } deriving (Eq, Show, Generic)

instance Arbitrary Thread where
  arbitrary = Thread <$> arbitrary <*> chooseInt (-1000, 1000) <*> arbitrary
  shrink = filter ((<= 1000) . abs . tIncrement) . genericShrink

runThread :: MonadIO m => Thread -> (Delay -> m a) -> (Int -> m b) -> m ()
runThread Thread{tDelay, tIncrement, tIterations} doSleep f =
  callN tIterations (f tIncrement *> doSleep tDelay)

newtype Threads = Threads { unThreads :: NonEmpty Thread }
  deriving (Eq, Show)

instance Arbitrary Threads where
  arbitrary = do
    n <- chooseInt (0, 31)
    Threads <$> ((:|) <$> arbitrary <*> replicateM n arbitrary)
  shrink = map Threads . genericShrink . unThreads

spawnAndCall :: Traversable f => f b -> IO a -> (a -> b -> IO ()) -> IO a
spawnAndCall threads mkRes action = do
  res <- mkRes
  traverse_ wait =<< traverse (async . action res) threads
  pure res

