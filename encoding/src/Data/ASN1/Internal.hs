{-# LANGUAGE CPP #-}

{- |
Module      : Data.ASN1.Internal
License     : BSD-style
Copyright   : (c) 2010-2013 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown
-}

module Data.ASN1.Internal
  ( uintOfBytes
  , intOfBytes
  , bytesOfUInt
  , bytesOfInt
  , putVarEncodingIntegral
  ) where

import           Data.Bits
                   ( Bits, (.&.), (.|.), complement, shiftL, shiftR, testBit )
import           Data.ByteString ( ByteString )
import qualified Data.ByteString as B
import           Data.List.NonEmpty ( NonEmpty (..), (<|) )
import qualified Data.List.NonEmpty as NE
import           Data.Word ( Word8 )

-- | Helper function while base < 4.15.0.0 (GHC < 9.0.1) is supported.
singletonNE :: a -> NonEmpty a
#if MIN_VERSION_base(4,15,0)
singletonNE = NE.singleton
#else
singletonNE a = a :| []
#endif

-- | Returns the number of bytes and the unsigned integer represented by the
-- bytes.
uintOfBytes :: ByteString -> (Int, Integer)
uintOfBytes b =
  (B.length b, B.foldl (\acc n -> (acc `shiftL` 8) + fromIntegral n) 0 b)

--bytesOfUInt i = B.unfoldr (\x -> if x == 0 then Nothing else Just (fromIntegral (x .&. 0xff), x `shiftR` 8)) i
bytesOfUInt :: Integer -> NonEmpty Word8
bytesOfUInt x = NE.reverse (list x)
 where
  list i = if i <= 0xff
    then singletonNE (fromIntegral i)
    else (fromIntegral i .&. 0xff) <| list (i `shiftR` 8)

-- | Returns the number of bytes in the list and the represented integer by a
-- two's completement list of bytes.
intOfBytes :: ByteString -> (Int, Integer)
intOfBytes b
  | B.length b == 0   = (0, 0)
  | otherwise         = (len, if isNeg then -(maxIntLen - v + 1) else v)
 where
  (len, v)  = uintOfBytes b
  maxIntLen = 2 ^ (8 * len) - 1
  isNeg     = testBit (B.head b) 7

-- | Convert an integer into a two's completemented list of bytes.
bytesOfInt :: Integer -> NonEmpty Word8
bytesOfInt i
  | i > 0     = if testBit (NE.head uints) 7 then 0 <| uints else uints
  | i == 0    = singletonNE 0
  | otherwise = if testBit (NE.head nints) 7 then nints else 0xff <| nints
 where
  uints = bytesOfUInt (abs i)
  nints = NE.reverse $ plusOne $ NE.reverse $ NE.map complement uints
  plusOne (0xff :| []) = 0 :| [1]
  plusOne (0xff :| (x : xs)) = 0 <| plusOne (x :| xs)
  plusOne (x :| xs) = (x + 1) :| xs

{- ASN1 often uses a particular kind of 7-bit encoding of integers like
   in the case of long tags or encoding of integer component of OID's.
   Use this function for such an encoding. Assumes a positive integer.

   Here is the description of the algorithm of the above encoding:

   1. The integer is chunked up into 7-bit groups. Each of these 7bit
      chunks are encoded as a single octet.

   2. All the octets except the last one has its 8th bit set.
-}
putVarEncodingIntegral :: (Bits i, Integral i) => i -> ByteString
putVarEncodingIntegral i = B.reverse $ B.unfoldr genOctets (i, True)
 where
  genOctets (x, first)
    | x > 0     =
        let out = fromIntegral (x .&. 0x7F) .|. (if first then 0 else 0x80)
        in  Just (out, (shiftR x 7, False))
    | otherwise = Nothing
