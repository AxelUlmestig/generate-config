
module Modify (
    modify
) where

import Data.Map             (Map, mapKeys, foldrWithKey, empty, singleton, union)
import Data.String.Utils    (replace)
import Text.Regex.TDFA      ((=~))
import Data.String.Utils    (strip)

modify :: Map String String -> String -> String
modify origConfig origTemplate = insertValues (addBrackets config) template
    where   config      = union origConfig (getDefaults origTemplate)
            template    = overwriteDefaults origTemplate

addBrackets :: Map String String -> Map String String
addBrackets = mapKeys (("{"++) . (++"}"))

insertValues :: Map String String -> String -> String
insertValues = flip $ foldrWithKey replace

getDefaults :: String -> Map String String
getDefaults template = mconcat
    . map toMap
    $ template =~ regex
    where   toMap (_:key:defaultValue:_)    = singleton (strip key) defaultValue
            toMap _                         = empty

overwriteDefaults :: String -> String
overwriteDefaults template = foldr ($) template
    . map removeDefault
    $ template =~ regex
    where   removeDefault (expr:key:_)  = replace expr ("{" ++ (strip key) ++ "}")
            removeDefault _             = id

--The regex engine can't handle \S for some reason,
--So I have to use trim from String.Utils instead...
regex = "{(.*)\\| *(.*)}" :: String
--regex = "{(.*\\S)\\|\\s*(.*)}" :: String
