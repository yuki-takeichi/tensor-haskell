module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest ["-XTypeApplications", "-XDataKinds", "-isrc", "src/Crow.hs"]
--main = doctest ["-isrc", "src/Crow.hs"]