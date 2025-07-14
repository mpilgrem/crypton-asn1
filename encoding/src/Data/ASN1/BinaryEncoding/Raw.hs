{- |
Module      : Data.ASN1.BinaryEncoding.Raw
License     : BSD-style
Copyright   : (c) 2010-2013 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

Raw encoding of binary format (BER/DER/CER)
-}

module Data.ASN1.BinaryEncoding.Raw
  ( -- * types
    ASN1Header (..)
  , ASN1Class (..)
  , ASN1Tag
  , ASN1Length (..)
  , ASN1Event (..)
    -- * parser
  , parseLBS
  , parseBS
    -- * writer
  , toLazyByteString
  , toByteString
    ) where

import           Data.ASN1.BinaryEncoding.Parse ( parseBS, parseLBS )
import           Data.ASN1.BinaryEncoding.Writer
                   ( toByteString, toLazyByteString )
import           Data.ASN1.Types.Lowlevel
                   ( ASN1Class (..), ASN1Event (..), ASN1Header (..)
                   , ASN1Length (..), ASN1Tag
                   )
