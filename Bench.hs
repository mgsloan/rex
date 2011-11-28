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

peanoize n = map (\i -> peano . (++"Z") . concat $ replicate i "S ") [0..n]

testPairs n = [parsePair $ "<" ++ replicate i ' ' ++ "a, pair>" | i <- [0..n]]

testDate n = [parseDate $ show i ++ ".10.20" | i <- [1900 .. 1900 + n]]

--NOTE: benchmark time includes test generation

main = defaultMain 
  [ bgroup "math" [ bench "10"       $ nf addEm     10 ]
  , bgroup "peano" [ bench "100"     $ nf peanoize 100 ]
  , bgroup "parsePair" [ bench "10"  $ nf testPairs 10 ]
  , bgroup "parseDate" [ bench "100" $ nf testDate 100 ]
  ]
