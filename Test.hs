{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns #-}

import Text.Regex.PCRE.QQT

math x = mathl x 0

mathl [reg|^({ Just y }\d+)\s*({ id -> s }.*)|] x = mathl s y
mathl [reg|^\+\s*({ Just y }\d+)\s*({ id -> s }.*)$|] x = mathl s $ x + y
mathl [reg|^-\s*({ Just y }\d+)\s*({ id -> s }.*)$|]  x = mathl s $ x - y
mathl [reg|^\*\s*({ Just y }\d+)\s*({ id -> s}.*)$|]  x = mathl s $ x * y
mathl [reg|^/\s*({ Just y }\d+)\s*({ id -> s}.*)$|]   x = mathl s $ x / y
mathl [] x = x

{-
*Main> math "2 + 2"
4.0
*Main> math "2 + 2 * 5"
20.0
*Main> math "2 + 20 * 5"
110.0
*Main> math "2 + 20 * 5 / 3"
36.666666666666664
-}
