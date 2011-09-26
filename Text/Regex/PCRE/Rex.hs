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
-- 3) Allows for the inline interpolation of mapping functions :: String -> a.
--
-- 4) Precompiles the regular expression at compile time, by calling into the
-- PCRE library and storing a ByteString literal representation of its state.
--
-- 5) Compile-time configurable to use different PCRE options, turn off
-- precompilation, use ByteStrings, or set a default mapping expression.
--
-- Since this is a quasiquoter library that generates code using view patterns,
-- the following extensions are required:
--
-- > {-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns #-}
--
-- Here's a silly example which parses peano numbers of the form Z, S Z,
-- S S Z, etc.  The \s+ means that it is not sensitive to the quantity or type
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
-- The token \"(?{\" introduces a capture group which has a mapping applied to
-- the -- result - in this case \"length . filter (=='S')\".  If the ?{ ... }
-- are omitted, then the capture group is not taken as part of the results of
-- the match.  If the contents of the ?{ ... } is omitted, then "id" is assumed:
--
-- parsePair :: String -> Maybe (String, String)
-- parsePair = [rex|^<\s* (?{ }[^\s,>]+) \s*,\s* (?{ }[^\s,>]+) \s*>$|]
--
-- The following example is derived from
-- http://www.regular-expressions.info/dates.html
--
-- > parseDate :: String -> Maybe (Int, Int, Int)
-- > parseDate [rex|^(?{ read -> y }(?:19|20)\d\d)[- /.]
-- >                 (?{ read -> m }0[1-9]|1[012])[- /.]
-- >                 (?{ read -> d }0[1-9]|[12][0-9]|3[01])$|]
-- >   |  (d > 30 && (m `elem` [4, 6, 9, 11]))
-- >   || (m == 2 &&
-- >       (d == 29 && not (mod y 4 == 0 && (mod y 100 /= 0 || mod y 400 == 0)))
-- >      || (d > 29)) = Nothing
-- >   | otherwise = Just (y, m, d)
-- > parseDate _ = Nothing
--
-- The above example makes use of the regex quasi-quoter as a pattern matcher.
-- The interpolated Haskell patterns are used to construct an implicit view
-- pattern out of the inlined ones.  The above pattern is expanded to the
-- equivalent:
--
-- > parseDate ([rex|^(?{ read }(?:19|20)\d\d)[- /.]
-- >                  (?{ read }0[1-9]|1[012])[- /.]
-- >                  (?{ read }0[1-9]|[12][0-9]|3[01])$|]
-- >           -> Just (y, m, d))
--
--
-- Caveat: Since haskell-src-exts does not support parsing view-patterns, the
-- above is implemented as a relatively naive split on \"->\".  It presumes that
-- the last \"->\" in the interpolated pattern seperates the pattern from an
-- expression on the left.  This allows for lambdas to be present in the
-- expression, but prevents nesting view patterns.
--
-- There are also a few other inelegances:
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
-- 3) While this error is believed to no longer exist, the following error
-- 
-- >  <interactive>: out of memory (requested 17584491593728 bytes)
--
-- Used to occur when evaluating in GHCi, due to a bug in the way precompilation
-- worked.  If this happens, please report it, and as a temporary work around,
-- make your own quasiquoter using \"rexConf _ False _ _\" to disable
-- pre-compilation.
--
-- Since pcre-light is a wrapper over a C API, the most efficient interface is
-- ByteStrings, as it does not natively speak Haskell lists.  The [rex| ... ]
-- quasiquoter implicitely packs the input into a bystestring, and unpacks the
-- results to strings before providing them to your mappers.  The \"brex\"
-- QuasiQuoter is provided for this purpose.  You can also define your own
-- QuasiQuoter - the definitions of the default configurations are as follows:
--
-- > rex  = rexConf False True "id" rexPCREOpts rexPCREExecOpts
-- > brex = rexConf True  True "id" rexPCREOpts rexPCREExecOpts
--
-- As mentioned, the other Bool determines whether precompilation is used.  The
-- string following is the default mapping expression, used when omitted.
-- Due to GHC staging restrictions, your configuration will need to be in a
-- different module than its usage.
--
-- Inspired by / copy-modified from Matt Morrow's regexqq package:
-- <http://hackage.haskell.org/packages/archive/regexqq/latest/doc/html/src/Text-Regex-PCRE-QQ.html>
--
-- And code from Erik Charlebois's interpolatedstring-qq package:
-- <http://hackage.haskell.org/packages/archive/interpolatedstring-qq/latest/doc/html/Text-InterpolatedString-QQ.html>
--
-----------------------------------------------------------------------------

module Text.Regex.PCRE.Rex (
  rex, brex, maybeRead, padRight, rexConf, rexPCREOpts, rexPCREExecOpts) where

import qualified Text.Regex.PCRE.Light as PCRE
import qualified Text.Regex.PCRE.Light.Base as PCRE
import Text.Regex.PCRE.Precompile

import Control.Arrow (first)
import Control.Monad (liftM)

import qualified Data.ByteString.Char8 as B
import Data.Either.Utils (forceEitherMsg)
import Data.List (find)
import Data.List.Split (split, onSublist)
import Data.Maybe (catMaybes, listToMaybe, fromJust, isJust)
import Data.Maybe.Utils (forceMaybeMsg)
import Data.Char (isSpace)

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse

import System.IO.Unsafe (unsafePerformIO)

import Debug.Trace

debug x = trace (show x) x

{- TODO:
  * Fix mentioned caveats
  * Target Text.Regex.Base ? 
  * Provide a variant which allows splicing in an expression that evaluates to
    regex string.
  * Add unit tests
  * Consider allowing passing arity lists in to configuration
-}

type ParseChunk = Either String (Int, String)
type ParseChunks = (Int, String, [(Int, String)])
type Config = (Bool, Bool, String, [PCRE.PCREOption], [PCRE.PCREExecOption])

-- | Default regular expression quasiquoter for Strings, and ByteStrings.
rex, brex :: QuasiQuoter
rex  = rexConf False True "id" rexPCREOpts rexPCREExecOpts
brex = rexConf True  True "id" rexPCREOpts rexPCREExecOpts


rexPCREOpts :: [PCRE.PCREOption]
rexPCREOpts =
  [ PCRE.extended
  , PCRE.multiline ]
  -- , dotall, caseless, utf8
  -- , newline_any, PCRE.newline_crlf ]

rexPCREExecOpts :: [PCRE.PCREExecOption]
rexPCREExecOpts = []
  -- [ PCRE.exec_newline_crlf
  -- , exec_newline_any, PCRE.exec_notempty
  -- , PCRE.exec_notbol, PCRE.exec_noteol ]

-- | A configureable regular-expression QuasiQuoter.  Takes the options to pass
-- to the PCRE engine, along with Bools to flag ByteString usage and
-- non-compilation respecively.
rexConf :: Bool -> Bool -> String -> [PCRE.PCREOption] -> [PCRE.PCREExecOption]
        -> QuasiQuoter
rexConf bs pc d os eos = QuasiQuoter
        (makeExp conf . parseIt)
        (makePat conf . parseIt)
        undefined undefined
 where
  conf = (bs, pc, d, [PCRE.combineOptions os], [PCRE.combineExecOptions eos])

-- Template Haskell Code Generation
-------------------------------------------------------------------------------

-- Creates the template haskell Exp which corresponds to the parsed interpolated
-- regex.  This particular code mainly just handles making "read" the
-- default for captures which lack a parser definition, and defaulting to making
-- the parser that doesn't exist
makeExp :: Config -> ParseChunks -> ExpQ
makeExp conf (cnt, pat, exs) = buildExp conf cnt pat exs'
 where
  exs' = map (\ix -> liftM (processExp conf . snd) $ find ((==ix).fst) exs) [0..cnt]
  
-- Creates the template haskell Pat which corresponds to the parsed interpolated
-- regex. As well as handling the aforementioned defaulting considerations, this
-- turns per-capture view patterns into a single tuple-resulting view pattern.
-- 
-- E.g. [reg| ... (?{e1 -> v1} ...) ... (?{e2 -> v2} ...) ... |] becomes
--      [reg| ... (?{e1} ...) ... (?{e2} ...) ... |] -> (v1, v2)
makePat :: Config -> ParseChunks -> PatQ
makePat conf (cnt, pat, exs) = do
  viewExp <- buildExp conf cnt pat $ map (liftM fst) views
  return . ViewP viewExp
         . (\xs -> ConP (mkName "Just") [TupP xs])
         . map snd $ catMaybes views
 where
  views :: [Maybe (Exp, Pat)]
  views = map (\ix -> liftM (processView . snd) $ find ((==ix).fst) exs) [0..cnt]

  processView :: String -> (Exp, Pat)
  processView xs = case splitFromBack 2 ((split . onSublist) "->" xs) of
    (_, [r]) -> onSpace r (error $ "blank pattern in view: " ++ r)
                          ((processExp conf "",) . processPat)
    -- View pattern
    (l, [_, r]) -> (processExp conf $ concat l, processPat r)
    -- Included so that Haskell doesn't warn about non-exhaustive patterns
    -- (even though the above are exhaustive in this context)
    _ -> undefined

-- Here's where the main meat of the template haskell is generated.  Given the
-- number of captures, the pattern string, and a list of capture expressions,
-- yields the template Haskell Exp which parses a string into a tuple.
buildExp :: Config -> Int -> String -> [Maybe Exp] -> ExpQ
buildExp (bs, nc, _, pcreOpts, pcreExecOpts) cnt pat xs =
  [| let r = $(get_regex) in
     $(process) . (flip $ PCRE.match r) $(liftRS pcreExecOpts)
   . $(if bs then [| id |] else [| B.pack |]) |]
 where
  liftRS x = [| read shown |]
   where shown = show x

  --TODO: make sure this takes advantage of bytestring fusion stuff - is
  -- the right pack / unpack. Or use XOverloadedStrings
  get_regex = if nc
    then [| unsafePerformIO (regexFromTable $! $(table_bytes)) |]
    else [| PCRE.compile (B.pack pat) $(liftRS pcreOpts)|]
  table_bytes = [| B.pack $(runIO table_string) |]
  table_string = precompile (B.pack pat) pcreOpts >>= 
    return . LitE . StringL . B.unpack . 
    forceMaybeMsg "Error while getting PCRE compiled representation\n"

  process = case (null vs, bs) of
    (True, _)  -> [| liftM ( const () ) |]
    (_, False) -> [| liftM (($(return maps)) . padRight "" pad . map B.unpack) |]
    (_, True)  -> [| liftM (($(return maps)) . padRight B.empty pad) |]
  pad = cnt + 2
  maps = LamE [ListP . (WildP:) $ map VarP vs]
       . TupE . map (uncurry AppE)
       -- filter out all "Nothing" exprs
       . map (first fromJust) . filter (isJust . fst)
       -- [(Expr, Variable applied to)]
       . zip xs $ map VarE vs
  vs = [mkName $ "v" ++ show i | i <- [0..cnt]]

-- Parse a Haskell expression into a template Haskell Exp
processExp :: Config -> String -> Exp
processExp (_, _, d, _, _) xs
  = forceEitherMsg ("Error while parsing capture mapper `" ++ xs ++ "'")
  . parseExp $ onSpace xs d id

-- Parse a Haskell pattern match into a template Haskell Pat, yielding Nothing
-- for patterns which consist of just whitespace.
processPat :: String -> Pat
processPat xs
  = forceEitherMsg ("Error while parsing capture pattern `" ++ xs ++ "'")
  $ parsePat xs

-- Parsing
-------------------------------------------------------------------------------

-- Postprocesses the results of the chunk-wise parse output, into the pattern to
-- be pased to the regex engine, and the interpolated 
parseIt :: String -> ParseChunks
parseIt xs = ( cnt, concat [x | Left x <- results]
             , [(i, x) | Right (i, x) <- results])
 where
  (cnt, results) = debug $ parseRegex (filter (`notElem` "\r\n") xs) "" (-1)

-- A pair of mutually-recursive functions, one for processing the quotation
-- and the other for the anti-quotation.

-- TODO: add check for erroneous { }

parseRegex :: String -> String -> Int -> (Int, [ParseChunk])
parseRegex inp s ix = case inp of
  -- Disallow branch-reset capture.
  ('(':'?':'|':_) ->
    error "Branch reset pattern (?| not allowed in quasi-quoted regex."

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

-- Utils
-------------------------------------------------------------------------------

-- | A possibly useful utility function - yields "Just x" when there is a valid
-- parse, and Nothing otherwise.
maybeRead :: (Read a) => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

splitFromBack :: Int -> [a] -> ([a], [a])
splitFromBack i xs = (reverse b, reverse a)
  where (a, b) = splitAt i $ reverse xs

onSpace :: String -> a -> (String -> a) -> a
onSpace s x f | all isSpace s = x
              | otherwise = f s

-- | Given a desired list-length, if the passed list is too short, it is padded
-- with the given element.  Otherwise, it trims.
padRight :: a -> Int -> [a] -> [a]
padRight _ 0 xs = xs
padRight v i [] = replicate i v
padRight v i (x:xs) = x : padRight v (i-1) xs

mapSnd :: (t -> t2) -> (t1, t) -> (t1, t2)
mapSnd f (x, y) = (x, f y)
