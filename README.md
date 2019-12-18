# postgis-simple

Haskell Postgis DB Driver

This is a almost fully rewritten fork of 'haskell-postgis' package by Peter
(https://github.com/ewestern/haskell-postgis). 
Provides a method that allows direct use of user-defined
Haskell data with PostGIS databases.

For valid installation don't forget to install libghc-postgresql-libpq-dev

```
apt install libghc-postgresql-libpq-dev
```

## Synopsis

### Default types

Import of Postgis.Simple, Database.PostgreSQL.Simple,
Postgis.Simple.Types.Default modules and use default types.

### Specific types

If you have some beloved old legacy data types and don't want to make type convertions.
Then you should manualy create instances of GeoData and Geometry type
classes similar to those defined in Postgis.Simple.Types.Default.
Here we will give an example of declarations that can cause your
data to work with Postgis. Let's assume you have polygons in nested
lists and your points are latitude-longitude pairs.

```haskell
type MySRID = Int
data LatLon = LatLon Double Double deriving Show
newtype Poly = Poly [[LatLon]] deriving Show -- just use newtype
```

Then we will make additional declarations.

```haskell
import Control.Monad
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField

data MyGPoly = MyGPoly (Maybe MySRID) Poly deriving Show

instance GeoData Poly where
  hasM (Poly _) = False
  hasZ (Poly _) = False
  geoType _ = pgisPolygon

instance Geometry MyGPoly where
    putGeometry (MyGPoly s pg@(Poly rs)) = do
        putHeader s pg
        putChainLen $ length rs
        mapM_ putChain rs
    getGeometry = do
        h <- getHeaderPre
        let geo gt
              | gt==pgisPolygon         = mkGeom h MyGPoly getPolygon 
              | otherwise = error $ "geometry type is not yet implemented: "++show gt
        geo $ lookGeoType h

putChain :: Putter [LatLon]
putChain r = do
    putChainLen $ length r
    mapM_ (\(LatLon y x) -> putPoint (x, y, Nothing, Nothing)) r

getChain :: Getter [LatLon]
getChain = getChainLen >>= (`replicateM` getPoint) >>= mapM (\(x, y, _, _) -> return $ LatLon y x) 

getPolygon :: Getter Poly 
getPolygon = skipHeader >> Poly <$> (getChainLen >>= (`replicateM` getChain))

instance ToField MyGPoly where
    toField = toFieldDefault

instance FromField MyGPoly where
    fromField = fromFieldDefault
```

Your app module may look like following.

```haskell
import Database.PostgreSQL.Simple
import Postgis.Simple

{..above declarations here or in a separate module..}

main :: IO ()
main = do
    conn <- connectPostgreSQL "..."
    let srid = Just 4326
        ring = [LatLon 1 2, LatLon 1.5 2.5, LatLon 2.5 3, LatLon 1 2]
        pl = MyGPoly srid (Poly [ring, ring])
    _ <- execute conn "INSERT INTO t1 (name, geom) VALUES (?, ?)" ("polygon"::String, pl)
    a <- query_ conn "select * from t1 LIMIT 1"     :: IO [(String, MyGPoly)]
    print a
```
