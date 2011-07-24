{-# LANGUAGE TemplateHaskell, QuasiQuotes, TupleSections, ViewPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Regex.PCRE.Rex
-- Copyright   :  (c) Michael Sloan 2011
--
-- Maintainer  :  Michael Sloan (mgsloan@gmail.com)
-- Stability   :  unstable
-- Portability :  unportable
-- 
-- This module provides a template Haskell quasiquoter for regular
-- expressions, which provides the following features:
-- 
-- 1) Compile-time checking that the regular expression is valid.
-- 
-- 2) Arity of resulting tuple based on the number of selected capture patterns
-- in the regular expression.
--
-- 3) By default utilizes type inference to determine how to parse capture
-- patterns - uses the "read" function's return-type polymorphism
--
-- 4) Allows for the inline interpolation of a mapping function String -> a.
--
-- 5) Precompiles the regular expression at compile time, by calling into the
-- PCRE library and storing a ByteString literal representation of its state.
--
-- Here's a silly example which parses peano numbers of the form "Z", "S Z",
-- "S S Z", etc.  The \s+ means that it is not sensitive to the quantity or type
-- of seperating whitespace. (these examples can also be found in Test.hs)
--
-- > peano :: String -> Maybe Int
-- > peano = [rex|^(?{ length . filter (=='S') } \s* (?:S\s+)*Z)\s*$|]
--
-- > *Main> peano "Z"
-- > Just 0
-- > *Main> peano "S Z"
-- > Just 1
-- > *Main> peano "S   S Z"
-- > Just 2
-- > *Main> peano "S S S Z"
-- > Just 3
-- > *Main> peano "invalid"
-- > Nothing
--
-- The token \"(?{\" introduces a capture group which has a mapping applied to the
-- result - in this case \"length . filter (=='S')\". If an expression is omitted, 
-- e.g. \"(?{} ... )\", then there is an implicit usage of the read function.
-- 
-- If the ?{ ... } are omitted, then the capture group is not taken as part of
-- the results of the match.
-- 
-- > vect2d :: String -> Maybe (Int, Int)
-- > vect2d = [rex|^<\s* (?{}\d+) \s*,\s* (?{}\d+) \s*>$|]
--
-- The following example is derived from http://www.regular-expressions.info/dates.html
--
-- > parseDate :: String -> Maybe (Int, Int, Int)
-- > parseDate [rex|^(?{ y }(?:19|20)\d\d)[- /.]
-- >                 (?{ m }0[1-9]|1[012])[- /.]
-- >                 (?{ d }0[1-9]|[12][0-9]|3[01])$|]
-- >   |  (d > 30 && (m `elem` [4, 6, 9, 11]))
-- >   || (m == 2 &&
-- >        (d == 29 && not (mod y 4 == 0 && (mod y 100 /= 0 || mod y 400 == 0)))
-- >      || (d > 29)) = Nothing
-- >   | otherwise = Just (y, m, d)
-- > parseDate _ = Nothing
--
-- The above example makes use of the regex quasi-quoter as a pattern matcher.
-- The interpolated Haskell patterns are used to construct an implicit view
-- pattern.  The above pattern is expanded to the equivalent:
--
-- > parseDate ([rex|^(?{}(?:19|20)\d\d)[- /.]
-- >                  (?{}0[1-9]|1[012])[- /.]
-- >                  (?{}0[1-9]|[12][0-9]|3[01])$|]
-- >           -> Just (y, m, d))
--
-- In order to provide a capture-mapper along with a pattern, use view-patterns
-- inside the interpolation brackets.
--
-- Caveat: Since haskell-src-exts does not support parsing view-patterns, the
-- above is implemented as a relatively naive split on \"->\".  It presumes that
-- the last \"->\" in the interpolated pattern seperates the pattern from an
-- expression on the left.  This allows for lambdas to be present in the
-- expression, but prevents nesting view patterns.
--
-- There are a few other inelegances:
--
-- 1) PCRE captures, unlike .NET regular expressions, yield the last capture
-- made by a particular pattern.  So, for example, (...)*, will only yield one
-- match for '...'.  Ideally these would be detected and yield an implicit [a].
--
-- 2) Patterns with disjunction between captures ((?{f}a) | (?{g}b)) will
-- provide the empty string to one of f / g.  In the case of pattern
-- expressions, it would be convenient to be able to map multiple captures into
-- a single variable / pattern, preferring the first non-empty option.  The
-- general logic for this is a bit complicated, and postponed for a later
-- release.
--
-- Since pcre-light is a wrapper over a C API, the most efficient interface is
-- ByteStrings, as it does not natively speak Haskell lists.  The [rex| ... ]
-- quasiquoter implicitely packs the input into a bystestring, and unpacks the
-- results to strings before providing them to your mappers.  Use [brex| ... ]
-- to bypass this, and process raw ByteStrings.  In order to preserve the
-- same default behavior, "read . unpack" is the default mapper for brex.
--
-- Inspired by / copy-modified from Matt Morrow's regexqq package:
-- http://hackage.haskell.org/packages/archive/regexqq/latest/doc/html/src/Text-Regex-PCRE-QQ.html
-- And code from Erik Charlebois's interpolatedstring-qq package:
-- http://hackage.haskell.org/packages/archive/interpolatedstring-qq/latest/doc/html/Text-InterpolatedString-QQ.html
--
-----------------------------------------------------------------------------

module Text.Regex.PCRE.Rex (rex, brex, maybeRead, padRight) where

import qualified Text.Regex.PCRE.Light as PCRE
import Text.Regex.PCRE.Precompile

import Control.Arrow (first, second)
import Control.Monad (liftM)

import qualified Data.ByteString.Char8 as B
import Data.Either.Utils (forceEitherMsg)
import Data.List (groupBy, sortBy)
import Data.List.Split (split, onSublist)
import Data.Maybe (catMaybes, listToMaybe, fromJust, isJust)
import Data.Ord (comparing)
import Data.Char (isSpace)

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse

import System.IO.Unsafe (unsafePerformIO)

type ParseChunk = Either String (Int, String)
type ParseChunks = (Int, String, [(Int, String)])

{- TODO: idea - provide a variant which allows splicing in an expression
  that evaluates to regex string.  The tricky thing here is that this
  splice might contain capture groups - thus, the parser here would need to
  be referenced in order to dispatch 
-}

{- TODO: target Text.Regex.Base -}

-- | The regular expression quasiquoter.
rex, brex :: QuasiQuoter
rex = QuasiQuoter
        (checkRegex (makeExp False) . parseIt)
        (checkRegex (makePat False) . parseIt)
        undefined undefined

brex = QuasiQuoter
        (checkRegex (makeExp True) . parseIt)
        (checkRegex (makePat True) . parseIt)
        undefined undefined

-- Default parsers, one for the bytestring case and one for the string.
defaultExp :: Bool -> String
defaultExp True = "read . unpack"
defaultExp False = "read"

-- Gives an error at compile time if the regex string is invalid.
checkRegex :: (ParseChunks -> a) -> ParseChunks -> a
checkRegex f x@(_, pat, _) = precompile (B.pack pat) pcreOpts `seq` f x
{-
  forceEitherMsg ("Error compiling regular expression [reg|"  ++ pat ++ "|]\n")
    (PCRE.compileM (B.pack pat) pcreOpts) `seq` f x
    -}


-- Template Haskell Code Generation
-------------------------------------------------------------------------------

-- Creates the template haskell Exp which corresponds to the parsed interpolated
-- regex.  This particular code mainly just handles making "read" the
-- default for captures which lack a parser definition, and defaulting to making
-- the parser that doesn't exist
makeExp :: Bool -> ParseChunks -> ExpQ
makeExp bs (cnt, pat, exs) = buildExp bs cnt pat
    . map (liftM (processExp bs) . snd . head)
    . groupSortBy (comparing fst)
    $ map (second Just) exs ++ [(i, Nothing) | i <- [0..cnt]]
  
-- Creates the template haskell Pat which corresponds to the parsed interpolated
-- regex. As well as handling the aforementioned defaulting considerations, this
-- turns per-capture view patterns into a single tuple-resulting view pattern.
-- 
-- E.g. [reg| ... (?{e1 -> v1} ...) ... (?{e2 -> v2} ...) ... |] becomes
--      [reg| ... (?{e1} ...) ... (?{e2} ...) ... |] -> (v1, v2)
makePat :: Bool -> ParseChunks -> PatQ
makePat bs (cnt, pat, exs) = do
  viewExp <- buildExp bs cnt pat $ map (liftM fst) ys
  return . ViewP viewExp . ConP (mkName "Just") . (\x -> [x])
         . TupP . map snd $ catMaybes ys
 where
  ys = map (floatSnd . processView . snd . head)
     . groupSortBy (comparing fst)
     $ exs ++ [(i, "") | i <- [0..cnt]]

  processView xs = case splitFromBack 2 ((split . onSublist) "->" xs) of
    (_, [r]) -> onSpace (error $ "blank pattern in: " ++ r)
                        ((processExp bs "",) . processPat) r
    -- View pattern
    (concat -> l, [_, r]) -> (processExp bs l, processPat r)
    -- Included so that Haskell doesn't warn about non-exhaustive patterns
    -- (even though the above are exhaustive in this context)
    _ -> undefined

-- Here's where the main meat of the template haskell is generated.  Given the
-- number of captures, the pattern string, and a list of capture expressions,
-- yields the template Haskell Exp which parses a string into a tuple.
buildExp :: Bool -> Int -> String -> [Maybe Exp] -> ExpQ
buildExp bs cnt pat xs = if bs
  then [| let r = unsafePerformIO (regexFromTable $(pre)) in 
          liftM ( $(return maps) . padRight B.empty pad )
        . (flip $ PCRE.match r) pcreExecOpts |]
  else [| let r = unsafePerformIO (regexFromTable $(pre)) in
          liftM ( $(return maps) . padRight "" pad . map B.unpack )
        . flip (PCRE.match r) pcreExecOpts . B.pack |]
  where pad = cnt + 2
        vs = [mkName $ "v" ++ show i | i <- [0..cnt]]
        --TODO: make sure this takes advantage of bytestring fusion stuff - is
        -- the right pack / unpack. Or use XOverloadedStrings
        pre = [| B.pack $(runIO (precompile (B.pack pat) pcreOpts) >>= 
                        return . LitE . StringL . B.unpack . fromJust) |]
        maps = LamE [ListP . (WildP:) $ map VarP vs]
                    (TupE . map (uncurry AppE)
                    -- filter out all "Nothing" exprs
                    . map (first fromJust) . filter (isJust . fst)
                    -- [(Expr, Variable applied to)]
                    . zip xs $ map VarE vs)

-- Parse a Haskell expression into a template Haskell Exp, yielding the
-- default for strings which just consist of whitespace.
processExp :: Bool -> String -> Exp
processExp bs xs = forceEitherMsg ("Error while parsing capture mapper " ++ xs)
                 . parseExp . onSpace (defaultExp bs) id $ xs

-- Parse a Haskell pattern match into a template Haskell Pat, yielding Nothing for
-- strings which just consist of whitespace.
processPat :: String -> Maybe Pat
processPat xs = onSpace Nothing
  (Just . forceEitherMsg ("Error while parsing capture pattern " ++ xs) . parsePat) xs

-- Parsing
-------------------------------------------------------------------------------

-- Postprocesses the results of the chunk-wise parse output, into the pattern to
-- be pased to the regex engine, and the interpolated 
parseIt :: String -> ParseChunks
parseIt xs = ( cnt, concat [x | Left x <- results]
             , [(i, x) | Right (i, x) <- results])
  where (cnt, results) = parseRegex (filter (`notElem` "\r\n") xs) "" (-1)

-- A pair of mutually-recursive functions, one for processing the quotation
-- and the other for the anti-quotation.

-- TODO: add check for erroneous { }

parseRegex :: String -> String -> Int -> (Int, [ParseChunk])
parseRegex inp s ix = case inp of
  -- Disallow branch-reset capture.
  ('(':'?':'|':_) ->
    error "Branch reset pattern (?| not allowed in fancy quasi-quoted regex."

  -- Ignore non-capturing parens / handle backslash escaping.
  ('\\':'\\'  :xs) -> parseRegex xs ("\\\\" ++ s) ix
  ('\\':'('   :xs) -> parseRegex xs (")\\"  ++ s) ix
  ('\\':')'   :xs) -> parseRegex xs ("(\\"  ++ s) ix
  ('(':'?':':':xs) -> parseRegex xs (":?("  ++ s) ix

  -- Anti-quote for processing a capture group.
  ('(':'?':'{':xs) -> mapSnd ((Left $ reverse ('(':s)) :)
                    $ parseHaskell xs "" (ix + 1)

  -- Keep track of how many capture groups we've seen.
  ('(':xs) -> parseRegex xs ('(':s) (ix + 1)

  -- Consume the regular expression contents.
  (x:xs) -> parseRegex xs (x:s) ix
  [] -> (ix, [Left $ reverse s])

parseHaskell :: String -> String -> Int -> (Int, [ParseChunk])
parseHaskell inp s ix = case inp of
  -- Escape } in the Haskell splice using a backslash.
  ('\\':'}':xs) -> parseHaskell xs ('}':s) ix

  -- Capture accumulated antiquote, and continue parsing regex literal.
  ('}':xs) -> mapSnd ((Right (ix, reverse s)):)
            $ parseRegex xs "" ix

  -- Consume the antiquoute contents, appending to a reverse accumulator.
  (x:xs) -> parseHaskell xs (x:s) ix
  [] -> error "Regular-expression Haskell splice is never terminated with a trailing }"

-- TODO: provide bytestring variant.

-- Utils
-------------------------------------------------------------------------------

-- The following 2 functions are referenced in the generated TH code, as well as
-- this module.

-- | A possibly useful utility function - yields "Just x" when there is a valid
-- parse, and Nothing otherwise.
maybeRead :: (Read a) => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- TODO: allow for a bundle of parameters to be passed in at the beginning
-- of the quasiquote. Would also be a juncture for informing arity /
-- extension information.

pcreOpts :: [PCRE.PCREOption]
pcreOpts =
  [ PCRE.extended
  , PCRE.multiline ]
  -- , dotall, caseless, utf8
  -- , newline_any, PCRE.newline_crlf ]

pcreExecOpts :: [PCRE.PCREExecOption]
pcreExecOpts = []
  -- [ PCRE.exec_newline_crlf
  -- , exec_newline_any, PCRE.exec_notempty
  -- , PCRE.exec_notbol, PCRE.exec_noteol ]

groupSortBy :: (a -> a -> Ordering) -> [a] -> [[a]]
groupSortBy f = groupBy (\x -> (==EQ) . f x) . sortBy f

splitFromBack :: Int -> [a] -> ([a], [a])
splitFromBack i xs = (reverse b, reverse a)
  where (a, b) = splitAt i $ reverse xs

onSpace :: a -> (String -> a) -> String -> a
onSpace x f s | all isSpace s = x
              | otherwise = f s

-- | Given a desired list-length, if the passed list is too short, it is padded
-- with the given element.  Otherwise, it trims.
padRight :: a -> Int -> [a] -> [a]
padRight _ 0 xs = xs
padRight v i [] = replicate i v
padRight v i (x:xs) = x : padRight v (i-1) xs

mapLeft :: (a -> a') -> Either a b -> Either a' b
mapLeft f = either (Left . f) Right

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

floatSnd :: (Functor f) => (a, f b) -> f (a, b)
floatSnd (x, y) = fmap (x,) y

{- for completeness:

mapRight :: (b -> b') -> Either a b -> Either a b'
mapRight f = either Left (Right . f)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

floatFst :: (Functor f) => (f a, b) -> f (a, b)
floatFst (x, y) = fmap (,y) x

floatBoth :: (Monad m) => (m a, m b) -> m (a, b)
floatBoth (x, y) = do
  x' <- x
  y' <- y
  return (x', y')

-}
