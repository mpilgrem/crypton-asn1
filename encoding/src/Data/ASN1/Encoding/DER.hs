{- |
Module      : Data.ASN1.Encoding.DER
License     : BSD-style
Stability   : experimental

DER-specific interface to the generic ASN.1 codec.

This module specialises the existing generic operations with the DER marker;
it does not implement another parser or change their validation or errors.
The generic type classes and marker API remain available for callers selecting
rules polymorphically. Import this module qualified to make the rules explicit
at each call, for example @DER.decodeASN1'@.
-}

module Data.ASN1.Encoding.DER
  ( -- * DER-based decoding and encoding of a stream
    decodeASN1
  , decodeASN1Repr
  , encodeASN1
    -- * Strict bytestring alternatives
  , decodeASN1'
  , decodeASN1Repr'
  , encodeASN1'
  ) where

import           Data.ASN1.BinaryEncoding ( DER (..) )
import qualified Data.ASN1.Encoding as Generic
import           Data.ASN1.Error ( ASN1Error )
import           Data.ASN1.Stream ( ASN1Repr )
import           Data.ASN1.Types ( ASN1 )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

-- | Decode a lazy DER bytestring.
decodeASN1 :: L.ByteString -> Either ASN1Error [ASN1]
decodeASN1 = Generic.decodeASN1 DER

-- | Decode a lazy DER bytestring with its representation.
decodeASN1Repr :: L.ByteString -> Either ASN1Error [ASN1Repr]
decodeASN1Repr = Generic.decodeASN1Repr DER

-- | Encode ASN.1 values as DER in a lazy bytestring.
encodeASN1 :: [ASN1] -> L.ByteString
encodeASN1 = Generic.encodeASN1 DER

-- | Decode a strict DER bytestring.
decodeASN1' :: B.ByteString -> Either ASN1Error [ASN1]
decodeASN1' = Generic.decodeASN1' DER

-- | Decode a strict DER bytestring with its representation.
decodeASN1Repr' :: B.ByteString -> Either ASN1Error [ASN1Repr]
decodeASN1Repr' = Generic.decodeASN1Repr' DER

-- | Encode ASN.1 values as DER in a strict bytestring.
encodeASN1' :: [ASN1] -> B.ByteString
encodeASN1' = Generic.encodeASN1' DER
