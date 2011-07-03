{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns #-}

import Text.Regex.PCRE.Rex
import Data.Maybe (catMaybes)

math x = mathl x 0

mathl [rex|^  \s*(?{ Just y }\d+)\s*(?{ id -> s }.*)|]  x = mathl s y
mathl [rex|^\+\s*(?{ Just y }\d+)\s*(?{ id -> s }.*)$|] x = mathl s $ x + y
mathl [rex|^ -\s*(?{ Just y }\d+)\s*(?{ id -> s }.*)$|] x = mathl s $ x - y
mathl [rex|^\*\s*(?{ Just y }\d+)\s*(?{ id -> s }.*)$|] x = mathl s $ x * y
mathl [rex|^ /\s*(?{ Just y }\d+)\s*(?{ id -> s }.*)$|] x = mathl s $ x / y
mathl [] x = x

peano :: String -> Int
peano = [rex|^(?{ length . filter (=='S') } \s* (?:S\s+)*Z)\s*$|]

vect2d :: String -> (Maybe Int, Maybe Int)
vect2d = [rex|^<\s* (?{}\d+) \s*,\s* (?{}\d+) \s*>$|]

-- From http://www.regular-expressions.info/dates.html
parseDate :: String -> Maybe (Int, Int, Int)
parseDate [rex|^(?{ Just y }(?:19|20)\d\d)[- /.]
                (?{ Just m }0[1-9]|1[012])[- /.]
                (?{ Just d }0[1-9]|[12][0-9]|3[01])$|]
  |  (d > 30 && (m `elem` [4, 6, 9, 11]))
  || (m == 2 &&
       (d ==29 && not (mod y 4 == 0 && (mod y 100 /= 0 || mod y 400 == 0)))
    || (d > 29)) = Nothing

  | otherwise = Just (y, m, d)

onNull a f [] = a
onNull _ f xs = f xs

nonNull  = onNull Nothing

disjunct [rex| ^(?:(?{nonNull $ Just . head -> a} .)
             | (?{nonNull $ Just . head -> b} ..)
             | (?{nonNull $ Just . last -> c} ...))$|] =
  head $ catMaybes [a, b, c]


