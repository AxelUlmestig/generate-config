
module Modify (
    modify
) where

import Data.Map             (Map, mapKeys, foldrWithKey)
import Data.String.Utils    (replace)

modify :: Map String String -> String -> String
modify config = insertValues (addBrackets config)

addBrackets :: Map String String -> Map String String
addBrackets = mapKeys (("{"++) . (++"}"))

insertValues :: Map String String -> String -> String
insertValues = flip $ foldrWithKey replace
