{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Postgis.Simple.Field.Default
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

module Postgis.Simple.Field.Default where

import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField

import Postgis.Simple.Internal

-- | Default toField function
toFieldDefault :: Geometry a => a -> Action
toFieldDefault = Escape . writeEWKB

-- | Default fromField function
fromFieldDefault :: Geometry a => FieldParser a
fromFieldDefault f m = do
        typ <- typename f
        if typ /= "geometry"
            then returnError Incompatible f (show typ)
            else case m of
                Nothing  -> returnError UnexpectedNull f ""
                Just bs -> return $ readEWKB bs

