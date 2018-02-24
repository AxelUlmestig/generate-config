
module Main (
    main
) where

import System.Environment   (getArgs)
import Flatten              (flatten)
import Modify               (modify)

main :: IO ()
main = do
    args <- getArgs
    case args of
        (manifestPath:templatePath:outputPath:_) ->
            generateConfigs manifestPath templatePath outputPath
        _ ->
            putStrLn "usage: generate-config manifest template outputPath"

generateConfigs :: String -> String -> String -> IO ()
generateConfigs manifestPath templatePath outputPath = do
    manifest <- readFile manifestPath
    template <- readFile templatePath
    let output = modify (flatten manifest) template
    writeFile outputPath output
