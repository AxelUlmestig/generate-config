{-# LANGUAGE OverloadedStrings #-}

module Flatten (
    flatten
) where

import Data.Text                (Text(..), unpack)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Vector              (Vector (..), toList)
import Data.Map                 (Map, singleton, empty, mapWithKey, foldrWithKey, foldr, union, fromList)
import qualified Data.HashMap.Strict as HashMap
import Data.Aeson
import Data.Maybe               (fromMaybe)

flatten :: String -> Map String String
flatten =
    trimKeys
    . flattenInternal ""
    . fromMaybe Null
    . decode
    . BS.pack

flattenInternal :: String -> Value -> Map String String
flattenInternal key Null            = empty
flattenInternal key (String str)    = singleton key (unpack str)
flattenInternal key (Bool bool)     = singleton key (show bool)
flattenInternal key (Number nbr)    = singleton key (show nbr)
flattenInternal key (Array v)       = flattenArray key v
flattenInternal key (Object obj)    = flattenObject key (fromList . HashMap.toList $ obj)

flattenObject :: String -> Map Text Value -> Map String String
flattenObject key =
    Data.Map.foldr union empty
    . mapWithKey (flattenInternal . (key++) . ("."++) . unpack)

flattenArray :: String -> Vector Value -> Map String String
flattenArray key =
    mconcat
    . zipWith flattenInternal indices
    . toList
    where   indices = map ((key++) . ("["++) . (++"]") . show) [0..]

trimLeadingDots :: String -> String
trimLeadingDots ('.':str)  = str
trimLeadingDots str        = str

trimKeys :: Map String String -> Map String String
trimKeys = foldrWithKey f empty
    where   f k = mappend . singleton (trimLeadingDots k)
