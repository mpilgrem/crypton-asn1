{- |
Module      : Data.ASN1.BitArray
License     : BSD-style
Copyright   : (c) 2010-2013 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown
-}

module Data.ASN1.BitArray
  ( BitArray (..)
  , BitArrayOutOfBound (..)
  , bitArrayLength
  , bitArrayGetBit
  , bitArraySetBitValue
  , bitArraySetBit
  , bitArrayClearBit
  , bitArrayGetData
  , toBitArray
  ) where

import           Control.Exception ( Exception, throw )
import           Data.Bits ( clearBit, setBit, testBit )
import           Data.ByteString ( ByteString )
import qualified Data.ByteString as B
import           Data.Maybe ( fromJust )
import           Data.Word ( Word64 )

-- | Thrown in case of out of bounds in the bitarray.
newtype BitArrayOutOfBound = BitArrayOutOfBound Word64
  deriving (Eq, Show)

instance Exception BitArrayOutOfBound

-- | Represent a bitarray / bitmap.
--
-- The memory representation starts at bit 0.
data BitArray = BitArray Word64 ByteString
  deriving (Eq, Show)

-- | Returns the length of bits in this bitarray.
bitArrayLength :: BitArray -> Word64
bitArrayLength (BitArray l _) = l

bitArrayOutOfBound :: Word64 -> a
bitArrayOutOfBound n = throw $ BitArrayOutOfBound n

-- | Get the nth bits.
bitArrayGetBit :: BitArray -> Word64 -> Bool
bitArrayGetBit (BitArray l d) n
  | n >= l    = bitArrayOutOfBound n
  | otherwise =
      flip testBit (7-fromIntegral bitn) $ B.index d (fromIntegral offset)
 where
  (offset, bitn) = n `divMod` 8

-- | Set the nth bit to the value specified.
bitArraySetBitValue :: BitArray -> Word64 -> Bool -> BitArray
bitArraySetBitValue (BitArray l d) n v
  | n >= l    = bitArrayOutOfBound n
  | otherwise =
      let (before, after) = B.splitAt (fromIntegral offset) d
      in
          -- array bound check before prevent fromJust from failing.
          let (w, remaining) = fromJust $ B.uncons after
              remaining' = setter w (7-fromIntegral bitn) `B.cons` remaining
          in  BitArray l (before `B.append` remaining')
 where
  (offset, bitn) = n `divMod` 8
  setter = if v then setBit else clearBit

-- | Set the nth bit.
bitArraySetBit :: BitArray -> Word64 -> BitArray
bitArraySetBit bitarray n = bitArraySetBitValue bitarray n True

-- | Clear the nth bit.
bitArrayClearBit :: BitArray -> Word64 -> BitArray
bitArrayClearBit bitarray n = bitArraySetBitValue bitarray n False

-- | Get padded bytestring from the bitarray.
bitArrayGetData :: BitArray -> ByteString
bitArrayGetData (BitArray _ d) = d

-- | Number of bits to skip at the end (padding).
toBitArray :: ByteString -> Int -> BitArray
toBitArray l toSkip =
  BitArray (fromIntegral (B.length l * 8 - fromIntegral toSkip)) l
