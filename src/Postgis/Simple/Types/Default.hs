{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Postgis.Simple.Types.Default
-- Copyright   :  (c) igor720
-- License     :  MIT
--
-- Maintainer  :  igor720@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Default types for Postgis data
--
-----------------------------------------------------------------------------

module Postgis.Simple.Types.Default (
  PointND (..)
, LineString (..)
, Polygon (..)
, MultiPoint (..)
, MultiLineString (..)
, MultiPolygon (..)
, GObject (..)
, Chain 
) where

import qualified Data.Vector as V
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField

import Postgis.Simple.Internal
import Postgis.Simple.Field.Default

-- | Data type for points
data PointND =
    Point2D  {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  | Point3DZ {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  | Point3DM {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  | Point4D  {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double
    deriving (Show, Eq)

instance GeoData PointND where
  hasM (Point2D _ _)    = False
  hasM Point3DZ {}      = False
  hasM Point3DM {}      = True
  hasM Point4D {}       = True
  hasZ (Point2D _ _)    = False
  hasZ Point3DZ {}      = True 
  hasZ Point3DM {}      = False 
  hasZ Point4D {}       = True
  geoType _ = pgisPoint

-- | Data type for point chain of Linestrings and Polygons 
type Chain = V.Vector PointND

-- | Data type for linestrings
data LineString = LineString Chain deriving (Show, Eq)

instance GeoData LineString where
  hasM (LineString ps) = hasM . V.head $ ps
  hasZ (LineString ps) = hasZ . V.head $ ps
  geoType _ = pgisLinestring

-- | Data type for polygons
data Polygon = Polygon (V.Vector Chain) deriving (Show, Eq)

hasMLinearRing :: Chain -> Bool
hasMLinearRing = hasM . V.head 

hasZLinearRing :: Chain -> Bool
hasZLinearRing = hasZ . V.head 

instance GeoData Polygon where
  hasM (Polygon ps) = hasMLinearRing . V.head $ ps
  hasZ (Polygon ps) = hasZLinearRing . V.head $ ps
  geoType _ = pgisPolygon

-- | Data type for multi points
data MultiPoint = MultiPoint (V.Vector PointND) deriving (Show, Eq)

instance GeoData MultiPoint where
  hasM (MultiPoint ps) = hasM . V.head $ ps
  hasZ (MultiPoint ps) = hasZ . V.head $ ps
  geoType _ = pgisMultiPoint

-- | Data type for multi linestrings
data MultiLineString = MultiLineString (V.Vector LineString) deriving (Show, Eq)

instance GeoData MultiLineString where
  hasM (MultiLineString ps) = hasM . V.head $ ps
  hasZ (MultiLineString ps) = hasZ . V.head $ ps
  geoType _ = pgisMultiLinestring

-- | Data type for multi polygons
data MultiPolygon = MultiPolygon (V.Vector Polygon) deriving (Show, Eq)

instance GeoData MultiPolygon where
  hasM (MultiPolygon ps) = hasM . V.head $ ps
  hasZ (MultiPolygon ps) = hasZ . V.head $ ps
  geoType _ = pgisMultiPolygon

-- | Data type for fully specified geometry objects
data GObject =
    GPoint SRID PointND
  | GLineString SRID LineString
  | GPolygon SRID Polygon
  | GMultiPoint SRID MultiPoint
  | GMultiLineString SRID MultiLineString
  | GMultiPolygon SRID MultiPolygon
    deriving (Show, Eq)

instance Geometry GObject where
    putGeometry (GPoint s p@(Point2D _ _)) = do
        putHeader s p
        putPointND p        
    putGeometry (GPoint s p@Point3DZ {}) = do
        putHeader s p
        putPointND p        
    putGeometry (GPoint s p@Point3DM {}) = do
        putHeader s p
        putPointND p        
    putGeometry (GPoint s p@Point4D {}) = do
        putHeader s p
        putPointND p        
    putGeometry (GLineString s ls@(LineString v)) = do
        putHeader s ls
        putRing v
    putGeometry (GPolygon s pg@(Polygon rs)) = do
        putHeader s pg
        putChainLen $ V.length rs 
        V.mapM_ putRing rs
    putGeometry (GMultiPoint s mp@(MultiPoint ps)) = do
        putHeader s mp
        putChainLen $ V.length ps 
        V.mapM_ (putGeometry . GPoint s)  ps
    putGeometry (GMultiLineString s mls@(MultiLineString ls)) = do
        putHeader s mls
        putChainLen $ V.length ls 
        V.mapM_ (putGeometry . GLineString s) ls
    putGeometry (GMultiPolygon s mpg@(MultiPolygon ps)) = do
        putHeader s mpg
        putChainLen $ V.length ps 
        V.mapM_ (putGeometry . GPolygon s) ps
    getGeometry = do
        h <- getHeaderPre
        let geo gt
              | gt==pgisPoint           = mkGeom h GPoint getGPoint
              | gt==pgisLinestring      = mkGeom h GLineString getLineString 
              | gt==pgisPolygon         = mkGeom h GPolygon getPolygon 
              | gt==pgisMultiPoint      = mkGeom h GMultiPoint getMultiPoint 
              | gt==pgisMultiLinestring = mkGeom h GMultiLineString getMultiLineString
              | gt==pgisMultiPolygon    = mkGeom h GMultiPolygon getMultiPolygon 
              | otherwise = error $ "geometry type is not yet implemented: "++show gt
        geo $ lookGeoType h

putPointND :: Putter PointND
putPointND (Point2D x y)      = putPoint (x, y, Nothing, Nothing)
putPointND (Point3DZ x y z)   = putPoint (x, y, Just z, Nothing)
putPointND (Point3DM x y m)   = putPoint (x, y, Just m, Nothing)
putPointND (Point4D x y z m)  = putPoint (x, y, Just z, Just m)

putRing :: Putter Chain
putRing v = do
  putChainLen $ V.length $ v  
  V.mapM_ putPointND v
 
getPointND :: Getter PointND
getPointND = do
    let defPoint x y Nothing Nothing    = Point2D x y
        defPoint x y (Just z) Nothing   = Point3DZ x y z
        defPoint x y Nothing (Just m)   = Point3DM x y m
        defPoint x y (Just z) (Just m)  = Point4D x y z m
    (x, y, zMb, mMb) <- getPoint
    return $ defPoint x y zMb mMb

getGPoint :: Getter PointND
getGPoint = skipHeader >> getPointND

getChain :: Getter Chain
getChain = getChainLen >>= (`V.replicateM` getPointND) 

getLineString :: Getter LineString 
getLineString = skipHeader >> LineString <$> getChain
 
getPolygon :: Getter Polygon 
getPolygon = skipHeader >> Polygon <$> (getChainLen >>= (`V.replicateM` getChain))

getMultiPoint :: Getter MultiPoint 
getMultiPoint = do
  skipHeader
  n <- getChainLen
  ps <- V.replicateM n getGPoint
  return $ MultiPoint ps

getMultiLineString :: Getter MultiLineString 
getMultiLineString = do
  skipHeader
  n <- getChainLen
  ls <- V.replicateM n getLineString 
  return $ MultiLineString ls

getMultiPolygon :: Getter MultiPolygon 
getMultiPolygon = do
  skipHeader
  n <- getChainLen
  ps <- V.replicateM n getPolygon
  return $ MultiPolygon ps

instance ToField GObject where
    toField = toFieldDefault

instance FromField GObject where
    fromField = fromFieldDefault 

