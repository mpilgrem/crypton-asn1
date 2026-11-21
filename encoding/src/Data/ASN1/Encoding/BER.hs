{- |
Module      : Data.ASN1.Encoding.BER
License     : BSD-style
Stability   : experimental

BER-specific interface to the generic ASN.1 codec.

This module specialises the existing generic operations with the BER marker;
it does not implement another parser or change their validation or errors.
The generic type classes and marker API remain available for callers selecting
rules polymorphically. Import this module qualified to make the rules explicit
at each call. BER encoding is not provided.
-}

module Data.ASN1.Encoding.BER
  ( -- * BER-based decoding of a stream
    decodeASN1
  , decodeASN1Repr
    -- * Strict bytestring alternatives
  , decodeASN1'
  , decodeASN1Repr'
  ) where

import           Data.ASN1.BinaryEncoding ( BER (..) )
import qualified Data.ASN1.Encoding as Generic
import           Data.ASN1.Error ( ASN1Error )
import           Data.ASN1.Stream ( ASN1Repr )
import           Data.ASN1.Types ( ASN1 )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

-- | Decode a lazy BER bytestring.
decodeASN1 :: L.ByteString -> Either ASN1Error [ASN1]
decodeASN1 = Generic.decodeASN1 BER

-- | Decode a lazy BER bytestring with its representation.
decodeASN1Repr :: L.ByteString -> Either ASN1Error [ASN1Repr]
decodeASN1Repr = Generic.decodeASN1Repr BER

-- | Decode a strict BER bytestring.
decodeASN1' :: B.ByteString -> Either ASN1Error [ASN1]
decodeASN1' = Generic.decodeASN1' BER

-- | Decode a strict BER bytestring with its representation.
decodeASN1Repr' :: B.ByteString -> Either ASN1Error [ASN1Repr]
decodeASN1Repr' = Generic.decodeASN1Repr' BER
