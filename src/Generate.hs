
module Generate (
    generate,
    File(..),
    Manifest,
    Template,
    ManifestPath,
    TemplatePath
) where

import Data.List.Utils          (replace)
import System.FilePath.Posix    (dropExtension)

import Flatten                  (flatten)
import Modify                   (modify)

data File = File {
    content :: String,
    name :: FilePath
} deriving (Show, Eq)

type Manifest = File
type Template = File

type ManifestPath = FilePath
type TemplatePath = FilePath

generate :: [Template] -> [Manifest] -> [File]
generate templates manifests = generateSingle <$> templates <*> manifests

generateSingle :: Template -> Manifest -> File
generateSingle (File templateContent templateName) (File manifestContent manifestName) =
    File {
        content = modify (flatten manifestContent) templateContent,
        name = generateName templateName manifestName
    }

generateName :: TemplatePath -> ManifestPath -> FilePath
generateName template manifest = replace ".template." ("." ++ dropExtension manifest ++ ".") template
