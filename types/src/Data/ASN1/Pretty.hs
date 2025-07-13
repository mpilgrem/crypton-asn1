{-# LANGUAGE CPP #-}

{- |
Module      : Data.ASN1.Pretty
License     : BSD-style
Copyright   : (c) 2010-2013 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

Types and functions to pretty print ASN.1 elements.
-}

module Data.ASN1.Pretty
  ( pretty
  , PrettyType (..)
  ) where

import           Data.ASN1.BitArray ( bitArrayGetData )
import           Data.ASN1.Types
                   ( ASN1 (..), ASN1CharacterString (..)
                   , ASN1ConstructionType (..), ASN1StringEncoding (..)
                   , ASN1TimeType (..)
                   )
#if MIN_VERSION_base16(1,0,0)
import           Data.Base16.Types ( extractBase16 )
#endif
import           Data.ByteString ( ByteString )
import           Data.ByteString.Base16 ( encodeBase16' )
import           Numeric ( showHex )

-- | A helper function while base16 < 1.0 is supported.
encodeBase16 :: ByteString -> ByteString
#if MIN_VERSION_base16(1,0,0)
encodeBase16 = extractBase16 . encodeBase16'
#else
encodeBase16 = encodeBase16'
#endif

-- | Type representing approaches to formatting.
data PrettyType =
    Multiline Int
    -- ^ Increase indentation following each 'Start' object and decrease
    -- following each 'End' object. The 'Int' is the initial indentation.
  | SingleLine
    -- ^ No indentation.
  deriving (Eq, Show)

-- | Pretty print a list of ASN.1 elements.
pretty ::
     PrettyType -- ^ The approach to formatting.
  -> [ASN1]     -- ^ Stream of ASN.1.
  -> String
pretty (Multiline at) = prettyPrint at
 where
  indent n = replicate n ' '

  prettyPrint _ []                 = ""
  prettyPrint n (x@(Start _) : xs) = indent n     ++ p id x ++ prettyPrint (n+1) xs
  prettyPrint n (x@(End _) : xs)   = indent (n-1) ++ p id x ++ prettyPrint (n-1) xs
  prettyPrint n (x : xs)           = indent n     ++ p id x ++ prettyPrint n xs

pretty SingleLine = prettyPrint
 where
  prettyPrint []                 = ""
  prettyPrint (x@(Start _) : xs) = p id x ++ "," ++ prettyPrint xs
  prettyPrint (x@(End _) : xs)   = p id x ++ "," ++ prettyPrint xs
  prettyPrint (x : xs)           = p id x ++ "," ++ prettyPrint xs

p :: ([Char] -> t) -> ASN1 -> t
p put (Boolean b)                        = put ("bool: " ++ show b)
p put (IntVal i)                         = put ("int: " ++ showHex i "")
p put (BitString bits)                   = put ("bitstring: " ++ hexdump (bitArrayGetData bits))
p put (OctetString bs)                   = put ("octetstring: " ++ hexdump bs)
p put Null                               = put "null"
p put (OID is)                           = put ("OID: " ++ show is)
p put (Real d)                           = put ("real: " ++ show d)
p put (Enumerated _)                     = put "enum"
p put (Start Sequence)                   = put "{"
p put (End Sequence)                     = put "}"
p put (Start Set)                        = put "["
p put (End Set)                          = put "]"
p put (Start (Container x y))            = put ("< " ++ show x ++ " " ++ show y)
p put (End (Container x y))              = put ("> " ++ show x ++ " " ++ show y)
p put (ASN1String cs)                    = putCS put cs
p put (ASN1Time TimeUTC time tz)         = put ("utctime: " ++ show time ++ " " ++ show tz)
p put (ASN1Time TimeGeneralized time tz) = put ("generalizedtime: " ++ show time ++ " " ++ show tz)
p put (Other tc tn x)                    = put ("other(" ++ show tc ++ "," ++ show tn ++ "," ++ show x ++ ")")

putCS :: ([Char] -> t) -> ASN1CharacterString -> t
putCS put (ASN1CharacterString UTF8 t)       = put ("utf8string:" ++ show t)
putCS put (ASN1CharacterString Numeric bs)   = put ("numericstring:" ++ hexdump bs)
putCS put (ASN1CharacterString Printable t)  = put ("printablestring: " ++ show t)
putCS put (ASN1CharacterString T61 bs)       = put ("t61string:" ++ show bs)
putCS put (ASN1CharacterString VideoTex bs)  = put ("videotexstring:" ++ hexdump bs)
putCS put (ASN1CharacterString IA5 bs)       = put ("ia5string:" ++ show bs)
putCS put (ASN1CharacterString Graphic bs)   = put ("graphicstring:" ++ hexdump bs)
putCS put (ASN1CharacterString Visible bs)   = put ("visiblestring:" ++ hexdump bs)
putCS put (ASN1CharacterString General bs)   = put ("generalstring:" ++ hexdump bs)
putCS put (ASN1CharacterString UTF32 t)      = put ("universalstring:" ++ show t)
putCS put (ASN1CharacterString Character bs) = put ("characterstring:" ++ hexdump bs)
putCS put (ASN1CharacterString BMP t)        = put ("bmpstring: " ++ show t)

hexdump :: ByteString -> String
hexdump bs = show (encodeBase16 bs)
