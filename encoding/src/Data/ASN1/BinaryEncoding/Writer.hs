{- |
Module      : Data.ASN1.BinaryEncoding.Writer
License     : BSD-style
Copyright   : (c) 2010-2013 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

Serialize events for streaming.
-}

module Data.ASN1.BinaryEncoding.Writer
  ( toByteString
  , toLazyByteString
  ) where

import           Data.ASN1.Serialize ( putHeader )
import           Data.ASN1.Types.Lowlevel
                   ( ASN1Event (..), ASN1Header (..), ASN1Length (..) )
import           Data.ByteString ( ByteString )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

-- | Transform a list of ASN.1 events into a strict bytestring.
toByteString :: [ASN1Event] -> ByteString
toByteString = B.concat . L.toChunks . toLazyByteString

-- | Transform a list of ASN.1 events into a lazy bytestring.
toLazyByteString :: [ASN1Event] -> L.ByteString
toLazyByteString evs = L.fromChunks $ loop [] evs
 where
  loop _ [] = []
  loop acc (x@(Header (ASN1Header _ _ pc len)):xs) =
    toBs x : loop (if pc then (len == LenIndefinite):acc else acc) xs
  loop acc (ConstructionEnd:xs) = case acc of
    []        -> error "malformed stream: end before construction"
    (True:r)  -> toBs ConstructionEnd : loop r xs
    (False:r) -> loop r xs
  loop acc (x:xs) = toBs x : loop acc xs

  toBs (Header hdr)      = putHeader hdr
  toBs (Primitive bs)    = bs
  toBs ConstructionBegin = B.empty
  toBs ConstructionEnd   = B.empty
