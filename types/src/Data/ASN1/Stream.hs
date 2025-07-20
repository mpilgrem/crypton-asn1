{- |
Module      : Data.ASN1.Stream
License     : BSD-style
Copyright   : (c) 2010-2013 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown
-}

module Data.ASN1.Stream
  ( ASN1Repr
    -- * Utilities
  , getConstructedEnd
  , getConstructedEndRepr
  ) where

import           Data.ASN1.Types ( ASN1 (..) )
import           Data.ASN1.Types.Lowlevel ( ASN1Event )

-- | Type synonym representing pairs of a ASN.1 value and a list of ASN.1
-- events.
--
-- This association is sometimes needed in order to know the exact byte sequence
-- leading to an ASN.1 value. For example, in the case of a cryptographic
-- signature.
type ASN1Repr = (ASN1, [ASN1Event])

-- | For the given list of ASN.1 values, assumed to follow a 'Start' value:
--
-- If the list is empty, return a pair of empty lists.
--
-- Otherwise, return a list of values up to (but excluding) the corresponding
-- 'End' value (if any), and a list of the remaining ASN.1 values.
getConstructedEnd ::
     Int
     -- ^ The number of additional 'Start' values encountered, @0@ initially.
  -> [ASN1]
  -> ([ASN1], [ASN1])
-- The given list is empty.
getConstructedEnd _ [] = ([], [])

-- The first item is another 'Start'.
getConstructedEnd i (x@(Start _) : xs) =
  let (ys, zs) = getConstructedEnd (i + 1) xs
  in  (x : ys, zs)

-- The first item is the corresponding 'End'.
getConstructedEnd 0 ((End _) : xs) = ([], xs)

-- The first item is an 'End', but not the corresponding 'End'.
getConstructedEnd i (x@(End _) : xs) =
  let (ys, zs) = getConstructedEnd (i - 1) xs
  in  (x : ys, zs)

-- The first item is not an 'End' or another 'Start'.
getConstructedEnd i (x : xs) =
  let (ys, zs) = getConstructedEnd i xs
  in  (x : ys, zs)

-- | For the given list of 'ASN1Repr' pairs:
--
-- If the list is empty, return a pair of empty lists.
--
-- If the first item represents a 'Start' value, return a list of pairs up to
-- the corresponding 'End' value (if any) (including the 'Start' and any 'End')
-- and a list of the remaining 'ASN1Repr' pairs.
--
-- Otherwise, return a list of that first item and a list of the
-- remaining pairs.
getConstructedEndRepr :: [ASN1Repr] -> ([ASN1Repr], [ASN1Repr])
-- The given list is empty.
getConstructedEndRepr [] = ([], [])

-- The first item represents a 'Start'.
getConstructedEndRepr (x@(Start _, _) : xs) =
  let (ys, zs) = getConstructedEndRepr' 1 xs
  in  (x : ys, zs)

-- The first item does not represent a 'Start'.
getConstructedEndRepr (x : xs) = ([x], xs)

-- | For the given list of 'ASN1Repr' pairs:
--
-- If the list is empty, return a pair of empty lists.
--
-- If there is no corresponding 'Start', return an empty list and the list of
-- 'ASN1Repr' pairs.
--
-- Otherwise, return a list of values up to (and including) the corresponding
-- 'End' value (if any), and a list of the remaining  'ASN1Repr' pairs.
getConstructedEndRepr' ::
     Int
     -- ^ The number of 'Start' values encountered.
  -> [ASN1Repr]
  -> ([ASN1Repr], [ASN1Repr])
-- The given list is empty.
getConstructedEndRepr' _ [] = ([], [])

-- There is no, or no longer a, corresponding 'Start'.
getConstructedEndRepr' 0 xs = ([], xs)

-- The first item is another 'Start'.
getConstructedEndRepr' i (x@(Start _, _) : xs) =
  let (ys, zs) = getConstructedEndRepr' (i + 1) xs
  in  (x : ys, zs)

-- The first item is an 'End'.
getConstructedEndRepr' i (x@(End _, _):xs) =
  let (ys, zs) = getConstructedEndRepr' (i - 1) xs
  in  (x : ys, zs)

-- The first item is not an 'End' or another 'Start'.
getConstructedEndRepr' i (x : xs) =
  let (ys, zs) = getConstructedEndRepr' i xs
  in  (x : ys, zs)
