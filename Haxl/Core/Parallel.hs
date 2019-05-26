-- Copyright (c) 2014-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is distributed under the terms of a BSD license,
-- found in the LICENSE file.

{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Psuedo-parallel operations.  Most users should import "Haxl.Core"
-- instead.
--
module Haxl.Core.Parallel
  ( -- * Parallel operations
    pAnd
  , pOr
  ) where

import Haxl.Core.Monad
