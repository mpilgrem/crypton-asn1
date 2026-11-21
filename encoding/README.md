crypton-asn1-encoding
=====================

Originally forked from
[asn1-encoding-0.9.6](https://hackage.haskell.org/package/asn1-encoding-0.9.6).

A reader and writer of data following the Abstract Syntax Notation One (ASN.1)
standard in raw form or in the high-level forms of Basic Encoding Rules (BER)
and Distinguished Encoding Rules (DER).

Unified API
-----------

The unified API provides the greatest flexibility.

Simple example using DER and lazy bytestrings:

~~~haskell
module Main ( main ) where

import           Data.ASN1.BinaryEncoding ( DER (..) )
import           Data.ASN1.Encoding ( ASN1Decoding (..) )
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
  bs <- B.readFile "octets"
  case decodeASN1 DER bs of
    Right asn1s -> print asn1s
    Left asn1Error -> putStrLn $ "Error: " <> show asn1Error
~~~

Module API
----------

The module API provides modules specialised to BER or DER.

Simple example using DER and lazy bytestrings:

~~~haskell
module Main ( main ) where

import qualified Data.ASN1.BinaryEncoding.DER as DER
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
  bs <- B.readFile "octets"
  case DER.decodeASN1 bs of
    Right asn1s -> print asn1s
    Left asn1Error -> putStrLn $ "Error: " <> show asn1Error
~~~

History
-------

The [`asn1-encoding`](https://hackage.haskell.org/package/asn1-encoding) package
was originated and then maintained by Vincent Hanquez. For published
reasons, he does not intend to develop the package further after version 0.9.6
but he also does not want to introduce other maintainers.
