{- |
Module      : Data.ASN1.Encoding.BER
License     : BSD-style
Stability   : experimental

Abstract Syntax Notation One (ASN.1) decoding interface, using Basic Encoding
Rules (BER).

This module is intended to be imported qualified, to avoid name clashes with
"Data.ASN1.Encoding" or "Data.ASN1.Encoding.DER" functions. For example:

> import qualified Data.ASN1.Encoding.BER as BER

For example (lazy bytestring):

> module Main ( main ) where
>
> import qualified Data.ASN1.Encoding.BER as BER
> import qualified Data.ByteString.Lazy as B
>
> main :: IO ()
> main = do
>   bs <- B.readFile "octets"
>   case BER.decodeASN1 bs of
>     Right asn1s -> print asn1s
>     Left asn1Error -> putStrLn $ "Error: " <> show asn1Error

or (strict bytestring):

> module Main ( main ) where
>
> import qualified Data.ASN1.Encoding.BER as BER
> import qualified Data.ByteString as B
>
> main :: IO ()
> main = do
>   bs <- B.readFile "octets"
>   case BER.decodeASN1' bs of
>     Right asn1s -> print asn1s
>     Left asn1Error -> putStrLn $ "Error: " <> show asn1Error
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

-- | Decode a lazy bytestring into a list of ASN.1 values, using BER.
decodeASN1 :: L.ByteString -> Either ASN1Error [ASN1]
decodeASN1 = Generic.decodeASN1 BER

-- | Decode a lazy bytestring into a list of 'ASN1Repr' pairs, using BER.
decodeASN1Repr :: L.ByteString -> Either ASN1Error [ASN1Repr]
decodeASN1Repr = Generic.decodeASN1Repr BER

-- | Decode a strict bytestring into a list of ASN.1 values, using BER.
decodeASN1' :: B.ByteString -> Either ASN1Error [ASN1]
decodeASN1' = Generic.decodeASN1' BER

-- | Decode a strict bytestring into a list of 'ASN1Repr' pairs, using BER.
decodeASN1Repr' :: B.ByteString -> Either ASN1Error [ASN1Repr]
decodeASN1Repr' = Generic.decodeASN1Repr' BER
