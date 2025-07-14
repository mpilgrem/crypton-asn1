{-# OPTIONS_GHC -Wno-orphans #-}

import           Control.Monad ( liftM4, replicateM )
import           Data.ASN1.BinaryEncoding ( DER (..) )
import           Data.ASN1.BinaryEncoding.Parse ( parseLBS )
import           Data.ASN1.BinaryEncoding.Writer ( toLazyByteString )
import           Data.ASN1.BitArray ( BitArray, toBitArray )
import           Data.ASN1.Encoding ( ASN1Decoding (..), ASN1Encoding (..) )
import           Data.ASN1.Get ( Result (..), runGet )
import           Data.ASN1.Prim ( mkSmallestLength )
import           Data.ASN1.Serialize ( getHeader, putHeader )
import           Data.ASN1.Types
                   ( ASN1 (..), ASN1CharacterString (..)
                   , ASN1ConstructionType (..), ASN1StringEncoding (..)
                   , ASN1TimeType (..), OID, asn1CharacterString
                   )
import           Data.ASN1.Types.Lowlevel
                   ( ASN1Class (..), ASN1Event (..), ASN1Header (..)
                   , ASN1Length (..), ASN1Tag
                   )
import qualified Data.ByteString as B
import           Data.Hourglass
                   ( Date (..), DateTime (..), Elapsed (..), Hours (..)
                   , Minutes (..), Month (..), Seconds (..), TimeOfDay (..)
                   , TimezoneOffset (..), timeConvert, timezone_UTC
                   )
import           Test.Tasty ( TestTree, defaultMain, testGroup )
import           Test.Tasty.QuickCheck
                   ( Arbitrary (..), Gen, choose, elements, listOf1, oneof
                   , resize, suchThat, testProperty
                   )

instance Arbitrary ASN1Class where
  arbitrary = elements [ Universal, Application, Context, Private ]

instance Arbitrary ASN1Length where
  arbitrary = do
    c <- choose (0, 2) :: Gen Int
    case c of
      0 -> fmap LenShort (choose (0, 0x79))
      1 -> do
        nb <- choose (0x80, 0x1000)
        return $ mkSmallestLength nb
      _ -> return LenIndefinite

arbitraryDefiniteLength :: Gen ASN1Length
arbitraryDefiniteLength = arbitrary `suchThat` (/= LenIndefinite)

arbitraryTag :: Gen ASN1Tag
arbitraryTag = choose(1,10000)

instance Arbitrary ASN1Header where
  arbitrary = liftM4 ASN1Header arbitrary arbitraryTag arbitrary arbitrary

arbitraryEvents :: Gen ASN1Events
arbitraryEvents = do
  hdr@(ASN1Header _ _ _ len) <- liftM4 ASN1Header arbitrary arbitraryTag (return False) arbitraryDefiniteLength
  let blen = case len of
        LenLong _ x -> x
        LenShort x  -> x
        _           -> 0
  pr <- fmap Primitive (arbitraryBSsized blen)
  return (ASN1Events [Header hdr, pr])

newtype ASN1Events = ASN1Events [ASN1Event]

instance Show ASN1Events where
  show (ASN1Events x) = show x

instance Arbitrary ASN1Events where
  arbitrary = arbitraryEvents

arbitraryOID :: Gen OID
arbitraryOID = do
  i1  <- choose (0, 2) :: Gen Integer
  i2  <- choose (0, 39) :: Gen Integer
  ran <- choose (0, 30) :: Gen Int
  l   <- replicateM ran (suchThat arbitrary (> 0))
  return (i1:i2:l)

arbitraryBSsized :: Int -> Gen B.ByteString
arbitraryBSsized len = do
  ws <- replicateM len (choose (0, 255) :: Gen Int)
  return $ B.pack $ map fromIntegral ws

instance Arbitrary B.ByteString where
  arbitrary = do
    len <- choose (0, 529) :: Gen Int
    arbitraryBSsized len

instance Arbitrary BitArray where
  arbitrary = do
    bs <- arbitrary
    w  <- choose (0, 7) :: Gen Int
    return $ toBitArray bs w

instance Arbitrary Date where
  arbitrary = do
    y <- choose (1951, 2050)
    m <- elements [ January .. December]
    d <- choose (1, 30)
    return $ normalizeDate $ Date y m d

normalizeDate :: Date -> Date
normalizeDate origDate
  | y < 1951  = normalizeDate (Date (y + 50) m d)
  | otherwise = normalizedDate
 where
  normalizedDate@(Date y m d) = timeConvert (timeConvert origDate :: Elapsed)

instance Arbitrary TimeOfDay where
  arbitrary = do
    h    <- choose (0, 23)
    mi   <- choose (0, 59)
    se   <- choose (0, 59)
    let nsec = 0
    return $ TimeOfDay (Hours h) (Minutes mi) (Seconds se) nsec

instance Arbitrary DateTime where
  arbitrary = DateTime <$> arbitrary <*> arbitrary

instance Arbitrary TimezoneOffset where
  arbitrary = elements
    [ timezone_UTC
    , TimezoneOffset 60
    , TimezoneOffset 120
    , TimezoneOffset (-360)
    ]

instance Arbitrary Elapsed where
  arbitrary = Elapsed . Seconds <$> arbitrary

instance Arbitrary ASN1TimeType where
  arbitrary = elements [TimeUTC, TimeGeneralized]

instance Arbitrary ASN1StringEncoding where
  arbitrary = elements
    [ UTF8
    , Numeric
    , Printable
    , T61
    , VideoTex
    , IA5
    , Graphic
    , Visible
    , General
    , UTF32
    , BMP
    ]

arbitraryPrintString :: ASN1StringEncoding -> Gen ASN1CharacterString
arbitraryPrintString encoding = do
  let printableString = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ()+,-./:=?"
  asn1CharacterString encoding <$> replicateM 21 (elements printableString)

arbitraryBS :: ASN1StringEncoding -> Gen ASN1CharacterString
arbitraryBS encoding =
  ASN1CharacterString encoding . B.pack <$> replicateM 7 (choose (0, 0xff))

arbitraryIA5String :: Gen ASN1CharacterString
arbitraryIA5String =
  asn1CharacterString IA5 <$> replicateM 21 (choose (toEnum 0, toEnum 127))

arbitraryUCS2 :: Gen ASN1CharacterString
arbitraryUCS2 =
  asn1CharacterString BMP <$> replicateM 12 (choose (toEnum 0, toEnum 0xffff))

arbitraryUnicode :: ASN1StringEncoding -> Gen ASN1CharacterString
arbitraryUnicode e =
  asn1CharacterString e <$> replicateM 35 (choose (toEnum 0, toEnum 0x10ffff))

instance Arbitrary ASN1CharacterString where
  arbitrary = oneof
    [ arbitraryUnicode UTF8
    , arbitraryUnicode UTF32
    , arbitraryUCS2
    , arbitraryPrintString Numeric
    , arbitraryPrintString Printable
    , arbitraryBS T61
    , arbitraryBS VideoTex
    , arbitraryIA5String
    , arbitraryPrintString Graphic
    , arbitraryPrintString Visible
    , arbitraryPrintString General
    ]

instance Arbitrary ASN1 where
  arbitrary = oneof
    [ fmap Boolean arbitrary
    , fmap IntVal arbitrary
    , fmap BitString arbitrary
    , fmap OctetString arbitrary
    , return Null
    , fmap OID arbitraryOID
    , fmap Real arbitrary
      -- , return Enumerated
    , ASN1String <$> arbitrary
    , ASN1Time <$> arbitrary <*> arbitrary <*> arbitrary
    ]

newtype ASN1s = ASN1s [ASN1]

instance Show ASN1s where
  show (ASN1s x) = show x

instance Arbitrary ASN1s where
  arbitrary = do
    x <- choose (0,5) :: Gen Int
    z <- case x of
      4 -> makeList Sequence
      3 -> makeList Set
      _ -> resize 2 $ listOf1 arbitrary
    return $ ASN1s z
   where
    makeList str = do
      (ASN1s l) <- arbitrary
      return ([Start str] ++ l ++ [End str])

prop_header_marshalling_id :: ASN1Header -> Bool
prop_header_marshalling_id v =
  ofDone ( runGet getHeader $ putHeader v) == Right v
 where
  ofDone (Done r _ _) = Right r
  ofDone _            = Left "not done"

prop_event_marshalling_id :: ASN1Events -> Bool
prop_event_marshalling_id (ASN1Events e) =
  parseLBS (toLazyByteString e) == Right e

prop_asn1_der_marshalling_id :: [ASN1] -> Bool
prop_asn1_der_marshalling_id v =
  (decodeASN1 DER . encodeASN1 DER) v `assertEq` Right v
 where
  assertEq got expected
    | got /= expected =
        error ("got: " ++ show got ++ " expected: " ++ show expected)
    | otherwise       = True

prop_real_der_marshalling_id :: Double -> Bool
prop_real_der_marshalling_id v =
  (decodeASN1 DER . encodeASN1 DER) [Real v] `assertEq` Right [Real v]
 where
  assertEq got expected
    | got /= expected =
        error ("got: " ++ show got ++ " expected: " ++ show expected)
    | otherwise       = True

prop_integral_real_der_marshalling_id :: Integer -> Bool
prop_integral_real_der_marshalling_id v =
  (decodeASN1 DER . encodeASN1 DER) [Real (fromInteger v)]
    `assertEq` Right [Real (fromInteger v)]
 where
  assertEq got expected
    | got /= expected =
        error ("got: " ++ show got ++ " expected: " ++ show expected)
    | otherwise       = True

marshallingTests :: TestTree
marshallingTests = testGroup "Marshalling"
  [ testProperty "Header" prop_header_marshalling_id
  , testProperty "Event"  prop_event_marshalling_id
  , testProperty "DER"    prop_asn1_der_marshalling_id
  , testProperty "Real"   prop_real_der_marshalling_id
  , testProperty "Integral Real"   prop_integral_real_der_marshalling_id
  ]

main :: IO ()
main = defaultMain $ testGroup "asn1-encoding" [marshallingTests]
