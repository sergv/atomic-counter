----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.Counter
-- Copyright   :  (c) Sergey Vinokurov 2022
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
--
-- Work with lifted 'Counter' values in the 'IO' monad. Please see other
-- modules in this package for 'Control.Monad.ST.ST' monad and for unlifted values.
----------------------------------------------------------------------------

module Control.Concurrent.Counter
  ( module Control.Concurrent.Counter.Lifted.IO
  ) where

import Control.Concurrent.Counter.Lifted.IO
