{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns #-}

module Test where

import Text.Regex.PCRE.Rex

import qualified Data.ByteString.Char8 as B
import Data.Maybe (catMaybes, isJust)

math x = mathl x 0

mathl [] x = x
mathl [rex|^  \s*(?{ read -> y }\d+)\s*(?{ s }.*)$|] x = mathl s y
mathl [rex|^\+\s*(?{ read -> y }\d+)\s*(?{ s }.*)$|] x = mathl s $ x + y
mathl [rex|^ -\s*(?{ read -> y }\d+)\s*(?{ s }.*)$|] x = mathl s $ x - y
mathl [rex|^\*\s*(?{ read -> y }\d+)\s*(?{ s }.*)$|] x = mathl s $ x * y
mathl [rex|^ /\s*(?{ read -> y }\d+)\s*(?{ s }.*)$|] x = mathl s $ x / y
mathl str x = error str

peano :: String -> Maybe Int
peano = [rex|^(?{ length . filter (=='S') } \s* (?:S\s+)*Z)\s*$|]

parsePair :: String -> Maybe (String, String)
parsePair = [rex|^<\s* (?{ }[^\s,>]+) \s*,\s* (?{ }[^\s,>]+) \s*>$|]

-- From http://www.regular-expressions.info/dates.html
parseDate :: String -> Maybe (Int, Int, Int)
parseDate [rex|^(?{ read -> y }(?:19|20)\d\d)[- /.]
                (?{ read -> m }0[1-9]|1[012])[- /.]
                (?{ read -> d }0[1-9]|[12][0-9]|3[01])$|]
  |  (d > 30 && (m `elem` [4, 6, 9, 11]))
  || (m == 2 &&
       (d ==29 && not (mod y 4 == 0 && (mod y 100 /= 0 || mod y 400 == 0)))
    || (d > 29)) = Nothing
  | otherwise = Just (y, m, d)
parseDate _ = Nothing

onNull a f [] = a
onNull _ f xs = f xs

nonNull = onNull Nothing

disjunct [rex| ^(?:(?{nonNull $ Just . head -> a} .)
             | (?{nonNull $ Just . head -> b} ..)
             | (?{nonNull $ Just . last -> c} ...))$|] =
  head $ catMaybes [a, b, c]

nocap = isJust $ [rex|jus \s nothin|] "jus nothin"
nocapat [rex|jus \s nothin|] = "matched!"
nocapat _ = "no match."

-- Ensure that the other variants compile:
nctest = [ncrex|(?{}hello)|] "hello" == Just "hello"
btest = [brex|(?{}hello)|] (B.pack "hello") == Just (B.pack "hello")
ncbtest = [ncbrex|(?{}hello)|] (B.pack "hello") == Just (B.pack "hello")
