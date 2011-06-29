{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns #-}

import Text.Regex.PCRE.QQT
import Debug.Trace

-- mathr ([$reg|({ -> Just x}\d+)\s*\+\s*({id -> y}.+)|]) = x + mathr y

debug x = trace (show x) x

math ([reg|^(\d+)\s*({id}.*)|]       -> (Just y, s)) x = math (debug s) y
math ([reg|^\+\s*(\d+)\s*({id}.*)$|] -> (Just y, s)) x = math (debug s) $ x + y
math ([reg|^-\s*(\d+)\s*({id}.*)$|]  -> (Just y, s)) x = math (debug s) $ x - y
math ([reg|^\*\s*(\d+)\s*({id}.*)$|] -> (Just y, s)) x = math (debug s) $ x * y
math ([reg|^/\s*(\d+)\s*({id}.*)$|]  -> (Just y, s)) x = math (debug s) $ x / y
math [] x = x

{-
*Main> math "2 + 2" 0
4.0
*Main> math "2 + 2 * 5" 0
20.0
*Main> math "2 + 20 * 5" 0
110.0
*Main> math "2 + 20 * 5 / 3" 0
36.666666666666664
-}
