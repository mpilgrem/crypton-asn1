{- |
Module      : Data.ASN1.Types
License     : BSD-style
Copyright   : (c) 2010-2013 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown
-}

module Data.ASN1.Types
  ( ASN1 (..)
  , ASN1S
  , ASN1Class (..)
  , ASN1Tag
  , ASN1ConstructionType (..)
  , ASN1StringEncoding (..)
  , ASN1TimeType (..)
  , ASN1Object (..)
  , ASN1CharacterString (..)
  , asn1CharacterString
  , asn1CharacterToString
  , module Data.ASN1.OID
  ) where

import           Data.ASN1.BitArray ( BitArray )
import           Data.ASN1.OID ( OID )
import           Data.ASN1.Types.Lowlevel ( ASN1Class (..), ASN1Tag )
import           Data.ASN1.Types.String
                   ( ASN1CharacterString (..), ASN1StringEncoding (..)
                   , asn1CharacterString, asn1CharacterToString
                   )
import           Data.ByteString ( ByteString )
import           Data.Hourglass ( DateTime, TimezoneOffset )

-- | Define the types of container.
data ASN1ConstructionType =
    Sequence
  | Set
  | Container ASN1Class ASN1Tag
  deriving (Eq, Show)

-- | Different ASN.1 time representations.
data ASN1TimeType =
    TimeUTC
    -- ^ ASN.1 UTCTime Type: limited between 1950-2050.
  | TimeGeneralized
    -- ^ ASN.1 GeneralizedTime Type.
  deriving (Eq, Ord, Show)

-- | Define high level ASN.1 objects.
data ASN1 =
    Boolean Bool
  | IntVal Integer
  | BitString BitArray
  | OctetString ByteString
  | Null
  | OID OID
  | Real Double
  | Enumerated Integer
  | ASN1String ASN1CharacterString
  | ASN1Time ASN1TimeType DateTime (Maybe TimezoneOffset)
  | Other ASN1Class ASN1Tag ByteString
  | Start ASN1ConstructionType
  | End ASN1ConstructionType
  deriving (Eq, Show)

-- | Represent a chunk of ASN.1 Stream. This is equivalent to ShowS but for an
-- ASN.1 Stream.
type ASN1S = [ASN1] -> [ASN1]

-- | Define an object that can be converted to and from ASN.1.
class ASN1Object a where
  -- | Transform an object into a chunk of ASN.1 stream.
  toASN1 :: a -> ASN1S

  -- | Returns either an object along the remaining ASN.1 stream, or an error.
  fromASN1 :: [ASN1] -> Either String (a, [ASN1])
