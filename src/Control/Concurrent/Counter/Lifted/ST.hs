----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.Counter.Lifted.ST
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
--
-- Counters that support some atomic operations. Safe to use from
-- multiple threads and likely faster than using IORef or TVar for the
-- same operation (terms and conditions apply).
----------------------------------------------------------------------------

{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

module Control.Concurrent.Counter.Lifted.ST
  ( Counter

  -- * Create
  , new

  -- * Read/write
  , get
  , set
  , cas

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

import GHC.Exts (Int(..), Int#, State#)
import GHC.ST

import qualified Control.Concurrent.Counter.Unlifted as Unlifted

-- | Memory location that supports select few atomic operations.
--
-- Isomorphic to @STRef s Int@.
data Counter s = Counter (Unlifted.Counter s)

-- | Pointer equality
instance Eq (Counter s) where
  Counter x == Counter y = Unlifted.sameCounter x y


{-# INLINE new #-}
-- | Create new counter with initial value.
new :: Int -> ST s (Counter s)
new (I# initVal) = ST $ \s1 -> case Unlifted.new initVal s1 of
  (# s2, c #) -> (# s2, Counter c #)


{-# INLINE get #-}
-- | Atomically read the counter's value.
get
  :: Counter s
  -> ST s Int
get (Counter c) = ST $ \s1 -> case Unlifted.get c s1 of
  (# s2, x #) -> (# s2, I# x #)

{-# INLINE set #-}
-- | Atomically assign new value to the counter.
set
  :: Counter s
  -> Int
  -> ST s ()
set (Counter c) (I# x) = ST $ \s1 -> case Unlifted.set c x s1 of
  (# s2 #) -> (# s2, () #)

{-# INLINE cas #-}
-- | Atomic compare and swap, i.e. write the new value if the current
-- value matches the provided old value. Returns the value of the
-- element before the operation
cas
  :: Counter s
  -> Int -- ^ Expected old value
  -> Int -- ^ New value
  -> ST s Int
cas (Counter c) (I# x) (I# y) = ST $ \s1 -> case Unlifted.cas c x y s1 of
  (# s2, z #) -> (# s2, I# z #)

{-# INLINE add #-}
-- | Atomically add an amount to the counter and return its old value.
add :: Counter s -> Int -> ST s Int
add = toST Unlifted.add

{-# INLINE sub #-}
-- | Atomically subtract an amount from the counter and return its old value.
sub :: Counter s -> Int -> ST s Int
sub = toST Unlifted.sub


{-# INLINE and #-}
-- | Atomically combine old value with a new one via bitwise and. Returns old counter value.
and :: Counter s -> Int -> ST s Int
and = toST Unlifted.and

{-# INLINE or #-}
-- | Atomically combine old value with a new one via bitwise or. Returns old counter value.
or :: Counter s -> Int -> ST s Int
or = toST Unlifted.or

{-# INLINE xor #-}
-- | Atomically combine old value with a new one via bitwise xor. Returns old counter value.
xor :: Counter s -> Int -> ST s Int
xor = toST Unlifted.xor

{-# INLINE nand #-}
-- | Atomically combine old value with a new one via bitwise nand. Returns old counter value.
nand :: Counter s -> Int -> ST s Int
nand = toST Unlifted.nand

{-# INLINE toST #-}
toST
  :: (Unlifted.Counter s -> Int# -> State# s -> (# State# s, Int# #))
  -> Counter s -> Int -> ST s Int
toST f = \(Counter c) (I# x) -> ST $ \s1 -> case f c x s1 of
  (# s2, old #) -> (# s2, I# old #)
