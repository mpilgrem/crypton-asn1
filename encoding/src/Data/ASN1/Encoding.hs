{- |
Module      : Data.ASN1.Encoding
License     : BSD-style
Copyright   : (c) 2010-2013 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

Generalised Abstract Syntax Notation One (ASN.1) decoding and encoding
interface, that you can use with encoding rules that belong to the
'ASN1Decoding', 'ASN1DecodingRepr', and/or 'ASN1Encoding' type classes.

For example (lazy bytestring):

> module Main ( main ) where
>
> import           Data.ASN1.BinaryEncoding ( DER (..) )
> import           Data.ASN1.Encoding ( ASN1Decoding (..) )
> import qualified Data.ByteString.Lazy as B
>
> main :: IO ()
> main = do
>   bs <- B.readFile "octets"
>   case decodeASN1 DER bs of
>     Right asn1s -> print asn1s
>     Left asn1Error -> putStrLn $ "Error: " <> show asn1Error

or (strict bytestring):

> module Main ( main ) where
>
> import           Data.ASN1.BinaryEncoding ( DER (..) )
> import           Data.ASN1.Encoding ( decodeASN1' )
> import qualified Data.ByteString as B
>
> main :: IO ()
> main = do
>   bs <- B.readFile "octets"
>   case decodeASN1' DER bs of
>     Right asn1s -> print asn1s
>     Left asn1Error -> putStrLn $ "Error: " <> show asn1Error
-}

module Data.ASN1.Encoding
  ( -- * Generic class for decoding and encoding a stream
    ASN1Decoding (..)
  , ASN1DecodingRepr (..)
  , ASN1Encoding (..)
    -- * Strict bytestring alternatives
  , decodeASN1'
  , decodeASN1Repr'
  , encodeASN1'
  ) where

import           Data.ASN1.Error ( ASN1Error )
import           Data.ASN1.Stream ( ASN1Repr )
import           Data.ASN1.Types ( ASN1 )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

-- | Describe an ASN.1 decoding, that transforms a lazy bytestring into a list
-- of ASN.1 values.
class ASN1Decoding rules where
  -- | Decode a lazy bytestring into a list of ASN.1 values.
  decodeASN1 :: rules -> L.ByteString -> Either ASN1Error [ASN1]

-- | Transition class.
class ASN1DecodingRepr rules where
  -- | Decode a lazy bytestring into a list of 'ASN1Repr' pairs.
  decodeASN1Repr :: rules -> L.ByteString -> Either ASN1Error [ASN1Repr]

-- | Describe an ASN.1 encoding, that transforms a list of ASN.1 values into a
-- lazy bytestring.
class ASN1Encoding rules where
  -- | Encode a list of ASN.1 values into a lazy bytestring.
  encodeASN1 :: rules -> [ASN1] -> L.ByteString

-- | Decode a strict bytestring into a list of ASN.1 values.
decodeASN1' ::
     ASN1Decoding rules
  => rules
  -> B.ByteString
  -> Either ASN1Error [ASN1]
decodeASN1' encoding bs = decodeASN1 encoding $ L.fromChunks [bs]

-- | Decode a strict bytestring into a list of 'ASN1Repr' pairs.
decodeASN1Repr' ::
     ASN1DecodingRepr rules
  => rules
  -> B.ByteString
  -> Either ASN1Error [ASN1Repr]
decodeASN1Repr' encoding bs = decodeASN1Repr encoding $ L.fromChunks [bs]

-- | Encode a list of ASN.1 values into a strict bytestring.
encodeASN1' :: ASN1Encoding rules => rules -> [ASN1] -> B.ByteString
encodeASN1' encoding = B.concat . L.toChunks . encodeASN1 encoding
