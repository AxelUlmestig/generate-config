
module Main (
    main
) where

import System.Environment       (getArgs)
import System.Directory         (doesFileExist, doesDirectoryExist, listDirectory)
import System.FilePath.Posix    (isExtensionOf, takeFileName, pathSeparator)
import Text.Regex.TDFA          ((=~))

import Flatten                  (flatten)
import Modify                   (modify)
import Generate                 (File(..), generate)

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
    manifests <- getFiles ".*\\.json$" manifestPath
    templates <- getFiles "\\.template" templatePath
    saveFiles outputPath $ generate templates manifests

getFiles :: String -> FilePath -> IO [File]
getFiles regex path = do
    fileExists <- doesFileExist path
    dirExists <- doesDirectoryExist path
    case (dirExists, fileExists) of
        (False, False) -> pure []
        (_, True) -> do
            fileContent <- readFile path
            pure [File {
                content = fileContent,
                name = takeFileName path
            }]
        (True, _) -> do
            files <- map (appendToPath path) . filter (=~ regex) <$> listDirectory path
            concat <$> mapM (getFiles regex) files

saveFiles :: FilePath -> [File] -> IO ()
saveFiles _ [] = putStrLn "no files matched requirements"
saveFiles outputPath [File fContent fName] = do
    dirExists <- doesDirectoryExist outputPath
    if dirExists
        then writeFile (appendToPath outputPath fName) fContent
        else writeFile outputPath fContent
saveFiles outputPath files = do
    dirExists <- doesDirectoryExist outputPath
    if dirExists
        then mapM_ (\(File fContent fName) -> writeFile (outputPath ++ [pathSeparator] ++ fName) fContent) files
        else putStrLn "Error: Can't give the same name to multiple output files"

appendToPath :: FilePath -> FilePath -> FilePath
appendToPath dirPath fileName = dirPath ++ [pathSeparator] ++ fileName
