{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Postgis.Simple.Internal
-- Copyright   :  (c) igor720
-- License     :  MIT
--
-- Maintainer  :  igor720@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Low level operations on Postgis extention of PostgreSQL Database
--
-----------------------------------------------------------------------------

module Postgis.Simple.Internal (
-- * User data classes
  GeoData (..)
, Geometry (..)
-- * Serialization types 
, Putter
, Getter
, Header
-- * Spatial Reference Identifier
, SRID
-- * Default reader and writer for EWKB data
, writeEWKB
, readEWKB
-- * Function which can be used in user specific getters and putters  
, makeHeader
, putHeader
, putMaybe
, putPoint
, putChainLen
, getHeader
, getHeaderPre
, lookGeoType
, mkGeom
, skipHeader
, getPoint
, getChainLen
-- * PostGIS constants  
, pgisPoint
, pgisLinestring
, pgisPolygon
, pgisMultiPoint
, pgisMultiLinestring
, pgisMultiPolygon
) where

import Foreign

import Data.ByteString.Lex.Integral
--import Data.Bits
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import System.Endian
import Data.Maybe (isJust)
import Data.Typeable (Typeable)

import Data.Binary.IEEE754 (wordToDouble, doubleToWord)
--import Control.Applicative ((<$>))

#include <pgisConst.h>

wkbZ :: Word32
wkbZ    = #const WKBZOFFSET
wkbM :: Word32
wkbM    = #const WKBMOFFSET
wkbSRID :: Word32
wkbSRID = #const WKBSRIDFLAG
ewkbTypeOffset :: Word32
ewkbTypeOffset = 0x1fffffff 

pgisPoint :: Word32
pgisPoint = #const POINTTYPE
pgisLinestring :: Word32
pgisLinestring = #const LINETYPE
pgisPolygon :: Word32
pgisPolygon = #const POLYGONTYPE
pgisMultiPoint :: Word32
pgisMultiPoint = #const MULTIPOINTTYPE
pgisMultiLinestring :: Word32
pgisMultiLinestring = #const MULTILINETYPE
pgisMultiPolygon :: Word32
pgisMultiPolygon = #const MULTIPOLYGONTYPE

type Getter = ReaderT Header Get 
type Putter a = a -> Put

-- | Type class for valid geometry data
-- 
-- geoType function must return valid constant for geometry data
-- 
class GeoData a where
    hasM    :: a -> Bool
    hasZ    :: a -> Bool
    geoType :: a -> Word32 

-- | Type class which defined base DB operations
class Typeable a => Geometry a where
    putGeometry :: Putter a
    getGeometry :: Get a

writeEWKB :: Geometry a => a -> BS.ByteString
writeEWKB = BL.toStrict . runPut . putGeometry 

readEWKB :: Geometry a => BS.ByteString -> a
readEWKB bs = runGet getGeometry (BL.fromStrict bs)

newtype ByteOrder = ByteOrder Endianness deriving Show 

instance Binary ByteOrder where
    get = fromHex <$> getLazyByteString 2 
    put = putLazyByteString . toHex 

type SRID = Maybe Int 

-- | Header record
data Header = Header {
    _byteOrder  :: ByteOrder
  , _geoType    :: Word32
  , _srid       :: SRID
} deriving Show

instance Binary Header where
    get = getHeader 
    put (Header bo gt s) = put bo >> (putInt . fromIntegral) gt  >> putMaybe s putInt

makeHeader :: GeoData a => SRID -> a -> Header
makeHeader s geo =
    let gt = geoType geo
        wOr acc (p, h) = if p then h .|. acc else acc
        _gt = foldl wOr gt [(hasM geo, wkbM), (hasZ geo, wkbZ), (isJust s, wkbSRID)]   
    in Header (ByteOrder getSystemEndianness) _gt s 

lookGeoType :: Header -> Word32
lookGeoType h = _geoType h .&. ewkbTypeOffset

mkGeom :: Header -> (SRID -> a -> b) -> Getter a -> Get b
mkGeom h gt p = gt (_srid h) <$> runReaderT p h

--- Hexable class

class Hexable a where
    toHex :: a -> BL.ByteString
    fromHex :: BL.ByteString -> a

instance Hexable ByteOrder where
    toHex (ByteOrder BigEndian) = "00" :: BL.ByteString  
    toHex (ByteOrder LittleEndian) = "01" :: BL.ByteString  
    fromHex bo = case fromHex' bo :: Word8 of
        0 -> ByteOrder BigEndian
        1 -> ByteOrder LittleEndian
        _ -> error $ "Incorrect ByteOrder" ++ show bo

instance Hexable Word32  where
    toHex = toHex' 8
    fromHex = fromHex'

instance Hexable Word64 where
    toHex = toHex' 16
    fromHex = fromHex'

toHex' :: Integral a => Int -> a -> BL.ByteString
toHex' l w = case bsRes of
    Just s -> BL.fromChunks [pad l s]
    Nothing -> error "toHex: cannot convert word" 
    where
        bsRes = packHexadecimal w
        pad l' bs' = BS.append (BC.replicate (l' - BS.length bs') '0') bs'

fromHex' :: Integral a => BL.ByteString -> a
fromHex' bs = case hexRes of
    Just (v, _) -> v
    Nothing -> error "fromHex: cannot parse hexadecimal"
    where
        hexRes = readHexadecimal $ BS.concat . BL.toChunks $ bs 

--- Putters

byteSwapFn :: ByteOrder -> (a -> a) -> a -> a
byteSwapFn bo f = case bo of 
    ByteOrder BigEndian     -> id
    ByteOrder LittleEndian  -> f

putDouble :: Putter Double
putDouble = putLazyByteString . toHex .
    byteSwapFn (ByteOrder getSystemEndianness) byteSwap64 . doubleToWord

putInt :: Putter Int
putInt = putLazyByteString . toHex .
    byteSwapFn (ByteOrder getSystemEndianness) byteSwap32 . fromIntegral

putChainLen :: Putter Int
putChainLen = putInt

putMaybe :: Maybe a -> Putter a -> Put
putMaybe mi = case mi of
    Just i -> ($ i) 
    Nothing -> \_ -> return ()

putHeader :: GeoData a => SRID -> a -> Put
putHeader s p = put $ makeHeader s p

putPoint :: Putter (Double, Double, Maybe Double, Maybe Double)
putPoint (x, y, Nothing, Nothing)   = putDouble x >> putDouble y
putPoint (x, y, Just z, Nothing)    = putDouble x >> putDouble y >> putDouble z
putPoint (x, y, Nothing, Just m)    = putDouble x >> putDouble y >> putDouble m
putPoint (x, y, Just z, Just m)     = putDouble x >> putDouble y >> putDouble z >> putDouble m

-- Getters 

getHeader :: Get Header
getHeader = do
    bo <- get
    t <- fromIntegral <$>  getInt' bo
    s <- if t .&. wkbSRID > 0 then (Just . fromIntegral) <$> getInt' bo  else return Nothing 
    return $ Header bo t s

skipHeader :: Getter ()
skipHeader = lift getHeader >> return ()

getHeaderPre :: Get Header
getHeaderPre = lookAhead get

getPoint :: Getter (Double, Double, Maybe Double, Maybe Double)
getPoint = do
    gt <- asks _geoType 
    let hasM' = (gt .&. wkbM) > 0 
        hasZ' = (gt .&. wkbZ) > 0
    x <- getDouble
    y <- getDouble
    zMb <- if hasZ' then Just <$> getDouble else return Nothing
    mMb <- if hasM' then Just <$> getDouble else return Nothing
    return (x, y, zMb, mMb)

getNumber :: (Hexable a) => (a -> a) -> Int64 -> ByteOrder -> Get a
getNumber f l bo  = do
  bs <- getLazyByteString l
  case bo of
    ByteOrder BigEndian     -> return $ fromHex bs
    ByteOrder LittleEndian  -> return . f . fromHex $ bs 

getInt' :: ByteOrder -> Get Int
getInt' = fmap fromIntegral . getNumber byteSwap32 8

getInt :: Getter Int 
getInt = (getInt' <$> asks _byteOrder) >>= lift

getChainLen :: Getter Int
getChainLen = getInt

getDouble' :: ByteOrder -> Get Double 
getDouble' = fmap wordToDouble . getNumber byteSwap64 16

getDouble :: Getter Double
getDouble = (getDouble' <$> asks _byteOrder) >>= lift

