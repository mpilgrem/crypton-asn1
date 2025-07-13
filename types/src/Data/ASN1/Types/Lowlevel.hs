{- |
Module      : Data.ASN1.Types.Lowlevel
License     : BSD-style
Copyright   : (c) 2010-2013 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown
-}

module Data.ASN1.Types.Lowlevel
  ( -- * Raw types
    ASN1Class(..)
  , ASN1Tag
  , ASN1Length(..)
  , ASN1Header(..)
    -- * Events types
  , ASN1Event(..)
  ) where

import Data.ByteString (ByteString)

-- | Element class.
data ASN1Class =
    Universal
  | Application
  | Context
  | Private
  deriving (Eq, Enum, Ord, Show)

-- | ASN.1 Tag.
type ASN1Tag = Int

-- | ASN.1 Length with all different formats.
data ASN1Length =
    LenShort Int
    -- ^ Short form with only one byte. Length has to be < 127.
  | LenLong Int Int
  -- ^ Long form of N bytes.
  | LenIndefinite
    -- ^ Length is indefinite. Expect an EOC in the stream to finish the type.
  deriving (Eq, Show)

-- | ASN.1 Header with the class, tag, constructed flag and length.
data ASN1Header = ASN1Header !ASN1Class !ASN1Tag !Bool !ASN1Length
  deriving (Eq, Show)

-- | Represent one event from an ASN.1 data stream.
data ASN1Event =
    Header ASN1Header     -- ^ ASN.1 Header.
  | Primitive !ByteString -- ^ Primitive
  | ConstructionBegin     -- ^ Constructed value start.
  | ConstructionEnd       -- ^ Constructed value end.
  deriving (Eq, Show)
