----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.Counter.Unlifted
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
--
-- Counters that support some atomic operations. Safe to use from
-- multiple threads and likely faster than using 'Data.IORef.IORef' or
-- 'Control.Concurrent.STM.TVar.TVar' for the same operation (terms and
-- conditions apply).
--
-- This module defines unlifted newtype wrapper and corresponding operations,
-- they're not suitable for use with e.g. monads or being stored in other
-- data structures that expect lifted types. For general use start with
-- 'Control.Concurrent.Counter.Counter' module.
----------------------------------------------------------------------------

{-# LANGUAGE CPP              #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE UnboxedTuples    #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Control.Concurrent.Counter.Unlifted
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

  -- * Compare
  , sameCounter
  ) where

import Prelude hiding (and, or)

import GHC.Exts

#include "MachDeps.h"
#ifndef SIZEOF_HSINT
#error "MachDeps.h didn't define SIZEOF_HSINT"
#endif

#define ADD_HASH(x) x#

-- | Memory location that supports select few atomic operations.
newtype Counter s = Counter (MutableByteArray# s)

{-# INLINE new #-}
-- | Create new counter with initial value.
new :: Int# -> State# s -> (# State# s, Counter s #)
new initVal = \s1 -> case newByteArray# ADD_HASH(SIZEOF_HSINT) s1 of
  (# s2, arr #) ->
    case writeIntArray# arr 0# initVal s2 of
      s3 -> (# s3, Counter arr #)


{-# INLINE get #-}
-- | Atomically read the counter's value.
get :: Counter s -> State# s -> (# State# s, Int# #)
get (Counter arr) = atomicReadIntArray# arr 0#

{-# INLINE set #-}
-- | Atomically assign new value to the counter.
set :: Counter s -> Int# -> State# s -> State# s
set (Counter arr) = atomicWriteIntArray# arr 0#


{-# INLINE add #-}
-- | Atomically add an amount to the counter and return its old value.
add :: Counter s -> Int# -> State# s -> (# State# s, Int# #)
add (Counter arr) = fetchAddIntArray# arr 0#

{-# INLINE sub #-}
-- | Atomically subtract an amount from the counter and return its old value.
sub :: Counter s -> Int# -> State# s -> (# State# s, Int# #)
sub (Counter arr) = fetchSubIntArray# arr 0#


{-# INLINE and #-}
-- | Atomically combine old value with a new one via bitwise and. Returns old counter value.
and :: Counter s -> Int# -> State# s -> (# State# s, Int# #)
and (Counter arr) = fetchAndIntArray# arr 0#

{-# INLINE or #-}
-- | Atomically combine old value with a new one via bitwise or. Returns old counter value.
or :: Counter s -> Int# -> State# s -> (# State# s, Int# #)
or (Counter arr) = fetchOrIntArray# arr 0#

{-# INLINE xor #-}
-- | Atomically combine old value with a new one via bitwise xor. Returns old counter value.
xor :: Counter s -> Int# -> State# s -> (# State# s, Int# #)
xor (Counter arr) = fetchXorIntArray# arr 0#

{-# INLINE nand #-}
-- | Atomically combine old value with a new one via bitwise nand. Returns old counter value.
nand :: Counter s -> Int# -> State# s -> (# State# s, Int# #)
nand (Counter arr) = fetchNandIntArray# arr 0#


-- | Compare the underlying pointers of two counters.
sameCounter :: Counter s -> Counter s -> Bool
sameCounter (Counter x) (Counter y) =
  isTrue# (sameMutableByteArray# x y)
