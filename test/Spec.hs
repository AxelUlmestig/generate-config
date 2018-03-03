
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
        assertion6,
        assertion7,
        assertion8,
        assertion9,
        assertion10
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

assertion7 = assertEqual "template with unnecessary default value" expected actual
    where   expected    = [File "<h1> hello kex</h1>" "template4.manifest1.html"]
            actual      = generate [template4] [manifest1]

assertion8 = assertEqual "template with necessary default value" expected actual
    where   expected    = [File "<h1> hello ostron</h1>" "template4.manifest3.html"]
            actual      = generate [template4] [manifest3]

assertion9 = assertEqual "template with multiple keys" expected actual
    where   expected    = [File "<h1> hello kex</h1>\n 3.0" "template5.manifest1.html"]
            actual      = generate [template5] [manifest1]

assertion10 = assertEqual "template with empty default value" expected actual
    where   expected    = [File "<h1> hello </h1>" "template6.manifest1.html"]
            actual      = generate [template6] [manifest1]

manifest1 = File "{\"ost\": {\"fisk\": \"kex\"}, \"frukt\": 3}" "manifest1" :: File
manifest2 = File "{\"ost\": {\"fisk\": 1}}" "manifest2" :: File
manifest3 = File "{\"fisk\": [\"ost\", \"frukt\"]}" "manifest3" :: File

template1 = File "<h1> hello {ost.fisk}</h1>" "template1.template.html" :: File
template2 = File "<h1> good bye {ost.fisk}</h1>" "template2.template.html" :: File
template3 = File "<h1> greetings {fisk[1]}</h1>" "template3.template.html" :: File
template4 = File "<h1> hello {ost.fisk | ostron}</h1>" "template4.template.html" :: File
template5 = File "<h1> hello {ost.fisk}</h1>\n {frukt}" "template5.template.html" :: File
template6 = File "<h1> hello {ost.hamster |}</h1>" "template6.template.html" :: File
