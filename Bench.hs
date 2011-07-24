{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns #-}

module Main where

import Text.Regex.PCRE.Rex
import Test

import Criterion.Main

addEm :: Int -> [Double]
addEm n = map math $
  zipWith (++)
    (zipWith (++) (map show [1..n])
    (concat $ repeat ["+", "/", "-", "*"]))
  (map show [100..])

main = defaultMain [
       bgroup "math" [ bench "10" $ nf addEm 10
                     , bench "1000" $ nf addEm 1000
                     , bench "10000" $ nf addEm 10000
                     ]
                    ]
