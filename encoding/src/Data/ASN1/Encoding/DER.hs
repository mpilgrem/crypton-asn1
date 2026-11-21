{- |
Module      : Data.ASN1.Encoding.DER
License     : BSD-style
Stability   : experimental

Abstract Syntax Notation One (ASN.1) decoding and encoding interface, using
Distinguished Encoding Rules (DER).

This module is intended to be imported qualified, to avoid name clashes with
"Data.ASN1.Encoding" or "Data.ASN1.Encoding.BER" functions. For example:

> import qualified Data.ASN1.Encoding.DER as DER

For example (lazy bytestring):

> module Main ( main ) where
>
> import qualified Data.ASN1.Encoding.DER as DER
> import qualified Data.ByteString.Lazy as B
>
> main :: IO ()
> main = do
>   bs <- B.readFile "octets"
>   case DER.decodeASN1 bs of
>     Right asn1s -> print asn1s
>     Left asn1Error -> putStrLn $ "Error: " <> show asn1Error

or (strict bytestring):

> module Main ( main ) where
>
> import qualified Data.ASN1.Encoding.DER as DER
> import qualified Data.ByteString as B
>
> main :: IO ()
> main = do
>   bs <- B.readFile "octets"
>   case DER.decodeASN1' bs of
>     Right asn1s -> print asn1s
>     Left asn1Error -> putStrLn $ "Error: " <> show asn1Error
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

-- | Decode a lazy bytestring into a list of ASN.1 values, using DER.
decodeASN1 :: L.ByteString -> Either ASN1Error [ASN1]
decodeASN1 = Generic.decodeASN1 DER

-- | Decode a lazy bytestring into a list of 'ASN1Repr' pairs, using DER.
decodeASN1Repr :: L.ByteString -> Either ASN1Error [ASN1Repr]
decodeASN1Repr = Generic.decodeASN1Repr DER

-- | Encode a list of ASN.1 values into a lazy bytestring, using DER.
encodeASN1 :: [ASN1] -> L.ByteString
encodeASN1 = Generic.encodeASN1 DER

-- | Decode a strict bytestring into a list of ASN.1 values, using DER.
decodeASN1' :: B.ByteString -> Either ASN1Error [ASN1]
decodeASN1' = Generic.decodeASN1' DER

-- | Decode a strict bytestring into a list of 'ASN1Repr' pairs, using DER.
decodeASN1Repr' :: B.ByteString -> Either ASN1Error [ASN1Repr]
decodeASN1Repr' = Generic.decodeASN1Repr' DER

-- | Encode a list of ASN.1 values into a strict bytestring, using DER.
encodeASN1' :: [ASN1] -> B.ByteString
encodeASN1' = Generic.encodeASN1' DER
