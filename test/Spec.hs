
module Main (main) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Generate (File(..), generate)

main :: IO ()
main = defaultMain
    . hUnitTestToTests
    . TestList
    . zipWith TestLabel labels
    . map TestCase
    $ assertions

labels = ["generate " ++ show n | n <- [1..]]

assertions = [
        assertion1,
        assertion2,
        assertion3,
        assertion4,
        assertion5,
        assertion6
    ]

assertion1 = assertEqual "no manifests" expected actual
    where   expected    = []
            actual      = generate [template1, template2] []

assertion2 = assertEqual "no templates" expected actual
    where   expected    = []
            actual      = generate [] [manifest1, manifest2]

assertion3 = assertEqual "one manifest, one template" expected actual
    where   expected    = [File "<h1> hello kex</h1>" "template1.manifest1.html"]
            actual      = generate [template1] [manifest1]

assertion4 = assertEqual "one manifest, two templates" expected actual
    where   expected    = [
                    File "<h1> hello kex</h1>" "template1.manifest1.html",
                    File "<h1> good bye kex</h1>" "template2.manifest1.html"
                ]
            actual      = generate [template1, template2] [manifest1]

assertion5 = assertEqual "two manifests, one template" expected actual
    where   expected    = [
                    File "<h1> hello kex</h1>" "template1.manifest1.html",
                    File "<h1> hello 1.0</h1>" "template1.manifest2.html"
                ]
            actual      = generate [template1] [manifest1, manifest2]

assertion6 = assertEqual "manifest with array" expected actual
    where   expected    = [File "<h1> greetings frukt</h1>" "template3.manifest3.html"]
            actual      = generate [template3] [manifest3]

manifest1 = File "{\"ost\": {\"fisk\": \"kex\"}}" "manifest1" :: File
manifest2 = File "{\"ost\": {\"fisk\": 1}}" "manifest2" :: File
manifest3 = File "{\"fisk\": [\"ost\", \"frukt\"]}" "manifest3" :: File

template1 = File "<h1> hello {ost.fisk}</h1>" "template1.template.html" :: File
template2 = File "<h1> good bye {ost.fisk}</h1>" "template2.template.html" :: File
template3 = File "<h1> greetings {fisk[1]}</h1>" "template3.template.html" :: File
