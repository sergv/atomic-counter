----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.Counter.Lifted.IO
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
--
-- Lifted 'Control.Concurrent.Counter.Lifted.Counter' specialized to
-- operate in the 'IO' monad.
----------------------------------------------------------------------------

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications    #-}

module Control.Concurrent.Counter.Lifted.IO
  ( Counter

  -- * Create
  , new

  -- * Read/write
  , get
  , set

  -- * Arithmetic operations
  , add
  , sub

  -- * Bitwise operations
  , and
  , or
  , xor
  , nand
  ) where

import Prelude hiding (and, or)

import Data.Coerce
import GHC.Exts (RealWorld)
import GHC.IO
import GHC.ST

import Control.Concurrent.Counter.Lifted.ST qualified as Lifted


-- | Memory location that supports select few atomic operations.
--
-- Isomorphic to @IORef Int@.
newtype Counter = Counter (Lifted.Counter RealWorld)

-- | Pointer equality
instance Eq Counter where
  (==) = coerce ((==) @(Lifted.Counter RealWorld))

{-# INLINE new #-}
-- | Create new counter with initial value.
new :: Int -> IO Counter
new = coerce . stToIO . Lifted.new


{-# INLINE get #-}
-- | Atomically read the counter's value.
get :: Counter -> IO Int
get = coerce Lifted.get

{-# INLINE set #-}
-- | Atomically assign new value to the counter.
set :: Counter -> Int -> IO ()
set = coerce Lifted.set


{-# INLINE add #-}
-- | Atomically add an amount to the counter and return its old value.
add :: Counter -> Int -> IO Int
add = coerce Lifted.add

{-# INLINE sub #-}
-- | Atomically subtract an amount from the counter and return its old value.
sub :: Counter -> Int -> IO Int
sub = coerce Lifted.sub


{-# INLINE and #-}
-- | Atomically combine old value with a new one via bitwise and. Returns old counter value.
and :: Counter -> Int -> IO Int
and = coerce Lifted.and

{-# INLINE or #-}
-- | Atomically combine old value with a new one via bitwise or. Returns old counter value.
or :: Counter -> Int -> IO Int
or = coerce Lifted.or

{-# INLINE xor #-}
-- | Atomically combine old value with a new one via bitwise xor. Returns old counter value.
xor :: Counter -> Int -> IO Int
xor = coerce Lifted.xor

{-# INLINE nand #-}
-- | Atomically combine old value with a new one via bitwise nand. Returns old counter value.
nand :: Counter -> Int -> IO Int
nand = coerce Lifted.nand
