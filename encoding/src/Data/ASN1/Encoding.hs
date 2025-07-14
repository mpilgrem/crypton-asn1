{- |
Module      : Data.ASN1.Encoding
License     : BSD-style
Copyright   : (c) 2010-2013 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown
-}

module Data.ASN1.Encoding
  ( -- * generic class for decoding and encoding stream
    ASN1Decoding (..)
  , ASN1DecodingRepr (..)
  , ASN1Encoding (..)
    -- * strict bytestring version
  , decodeASN1'
  , decodeASN1Repr'
  , encodeASN1'
  ) where

import           Data.ASN1.Error ( ASN1Error )
import           Data.ASN1.Stream ( ASN1Repr )
import           Data.ASN1.Types ( ASN1 )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

-- | Describe an ASN.1 decoding, that transforms a bytestream into an
-- asn1stream.
class ASN1Decoding a where
  -- | Decode a lazy bytestring into an ASN.1 stream.
  decodeASN1 :: a -> L.ByteString -> Either ASN1Error [ASN1]

-- | Transition class.
class ASN1DecodingRepr a where
  -- | Decode a lazy bytestring into an ASN.1 stream.
  decodeASN1Repr :: a -> L.ByteString -> Either ASN1Error [ASN1Repr]

-- | Describe an ASN.1 encoding, that transform an asn1stream into a bytestream.
class ASN1Encoding a where
  -- | Encode a stream into a lazy bytestring.
  encodeASN1 :: a -> [ASN1] -> L.ByteString

-- | Decode a strict bytestring into an ASN.1 stream.
decodeASN1' :: ASN1Decoding a => a -> B.ByteString -> Either ASN1Error [ASN1]
decodeASN1' encoding bs = decodeASN1 encoding $ L.fromChunks [bs]

-- | Decode a strict bytestring into an ASN1Repr stream.
decodeASN1Repr' ::
     ASN1DecodingRepr a
  => a
  -> B.ByteString
  -> Either ASN1Error [ASN1Repr]
decodeASN1Repr' encoding bs = decodeASN1Repr encoding $ L.fromChunks [bs]

-- | Encode a stream into a strict bytestring.
encodeASN1' :: ASN1Encoding a => a -> [ASN1] -> B.ByteString
encodeASN1' encoding = B.concat . L.toChunks . encodeASN1 encoding
