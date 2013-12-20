{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

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
-- PCRE library and storing a 'ByteString' literal representation of its state.
--
-- 5) Compile-time configurable to use different PCRE options, turn off
-- precompilation, use 'ByteString's, or set a default mapping expression.
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
-- the match.  If the contents of the ?{ ... } is omitted, then 'id' is assumed:
--
-- > parsePair :: String -> Maybe (String, String)
-- > parsePair = [rex|^<\s* (?{ }[^\s,>]+) \s*,\s* (?{ }[^\s,>]+) \s*>$|]
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
-- Since pcre-light is a wrapper over a C API, the most efficient interface is
-- ByteStrings, as it does not natively speak Haskell lists.  The [rex| ... ]
-- quasiquoter implicitely packs the input into a bystestring, and unpacks the
-- results to strings before providing them to your mappers.  The 'brex'
-- 'QuasiQuoter' is provided for this purpose.  You can also define your own
-- 'QuasiQuoter' - the definitions of the default configurations are as follows:
--
-- > rex  = rexWithConf $ defaultRexConf
-- > brex = rexWithConf $ defaultRexConf { rexByteString = True }
-- >
-- > defaultRexConf = RexConf False True "id" [PCRE.extended] []
--
-- The first @False@ specifies to use @String@ rather than 'ByteString'.  The
-- @True@ argument specifies to use precompilation.  --  The
-- string following is the default mapping expression, used when omitted.
-- Due to GHC staging restrictions, your configuration will need to be in a
-- different module than its usage.
--
-- Inspired by Matt Morrow's regexqq package:
-- <http://hackage.haskell.org/packages/archive/regexqq/latest/doc/html/src/Text-Regex-PCRE-QQ.html>
--
-- And code from Erik Charlebois's interpolatedstring-qq package:
-- <http://hackage.haskell.org/packages/archive/interpolatedstring-qq/latest/doc/html/Text-InterpolatedString-QQ.html>
--
-----------------------------------------------------------------------------

module Text.Regex.PCRE.Rex
  (
  -- * Quasiquoters
    rex, brex
  -- * Configurable QuasiQuoter
  , rexWithConf, RexConf(..), defaultRexConf
  -- * Utility
  , makeQuasiMultiline
  -- * Used by Generated Code
  , maybeRead, padRight
  ) where

import Text.Regex.PCRE.Precompile

import qualified Text.Regex.PCRE.Light as PCRE

import Control.Applicative   ( (<$>) )
import Control.Arrow         ( first )
import Control.Monad         ( liftM )
import Data.ByteString.Char8 ( pack, unpack, empty )
import Data.Maybe            ( catMaybes, listToMaybe, fromJust, isJust )
import Data.Char             ( isSpace )
import System.IO.Unsafe      ( unsafePerformIO )

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta (toExp,toPat)
import Language.Haskell.Exts.Extension (Extension(..), KnownExtension(..))
import Language.Haskell.Exts (parseExpWithMode, parsePatWithMode,
                              ParseMode, defaultParseMode, extensions,
                              ParseResult(..))

{- TODO:
  * Target Text.Regex.Base ?
  * Add unit tests
-}

data RexConf = RexConf
  { rexByteString :: Bool
  , rexCompiled :: Bool
  , rexView :: String
  , rexPCREOpts :: [PCRE.PCREOption]
  , rexPCREExecOpts :: [PCRE.PCREExecOption]
  }

-- | Default regular expression quasiquoter for 'String's and 'ByteString's,
-- respectively.
rex, brex :: QuasiQuoter
rex  = rexWithConf $ defaultRexConf
brex = rexWithConf $ defaultRexConf { rexByteString = True }

-- | This is a 'QuasiQuoter' transformer, which allows for a whitespace-
--   sensitive quasi-quoter to be broken over multiple lines.  The default 'rex'
--   and 'brex' functions do not need this as they are already whitespace
--   insensitive. However, if you create your own configuration, which omits the
--   'PCRE.extended' parameter, then this could be useful. The leading space of
--   each line is ignored, and all newlines removed.
makeQuasiMultiline :: QuasiQuoter -> QuasiQuoter
makeQuasiMultiline (QuasiQuoter a b c d) =
    QuasiQuoter (a . pre) (b . pre) (c . pre) (d . pre)
  where
    pre = concat . (\(x:xs) -> x : map (dropWhile isSpace) xs) . lines

-- | Default rex configuration, which specifies that the regexes operate on
--   strings, don't postprocess the matched patterns, and use 'PCRE.extended'.
--   This setting causes whitespace to be nonsemantic, and ignores # comments.
defaultRexConf :: RexConf
defaultRexConf = RexConf False False "id" [PCRE.extended] []

-- | A configureable regular-expression QuasiQuoter.  Takes the options to pass
--   to the PCRE engine, along with 'Bool's to flag 'ByteString' usage and
--   non-compilation respecively.  The provided 'String' indicates which mapping
--   function to use, when one is omitted - \"(?{} ...)\".
rexWithConf :: RexConf -> QuasiQuoter
rexWithConf conf =
  QuasiQuoter
    (makeExp conf . parseIt)
    (makePat conf . parseIt)
    undefined
    undefined

-- Template Haskell Code Generation
-------------------------------------------------------------------------------

-- Creates the template haskell Exp which corresponds to the parsed interpolated
-- regex.  This particular code mainly just handles making "read" the
-- default for captures which lack a parser definition, and defaulting to making
-- the parser that doesn't exist
makeExp :: RexConf -> ParseChunks -> ExpQ
makeExp conf (cnt, pat, exs) = buildExp conf cnt pat exs'
 where
  exs' = map (\ix -> liftM (forceEitherMsg "makeExp" . processExp conf) $ lookup ix exs) [0..cnt]

-- Creates the template haskell Pat which corresponds to the parsed interpolated
-- regex. As well as handling the aforementioned defaulting considerations, this
-- turns per-capture view patterns into a single tuple-resulting view pattern.
--
-- E.g. [reg| ... (?{e1 -> v1} ...) ... (?{e2 -> v2} ...) ... |] becomes
--      [reg| ... (?{e1} ...) ... (?{e2} ...) ... |] -> (v1, v2)
makePat :: RexConf -> ParseChunks -> PatQ
makePat conf (cnt, pat, exs) = do
  viewExp <- buildExp conf cnt pat $ map (liftM fst) views
  return . ViewP viewExp
         . (\xs -> ConP 'Just [TupP xs])
         . map snd $ catMaybes views
 where
  views :: [Maybe (Exp, Pat)]
  views = map (\ix -> liftM processView $ lookup ix exs) [0..cnt]

  processView :: String -> (Exp, Pat)
  processView xs = case processPat ("("++xs++")") of
    ParseOk (ParensP (ViewP e p)) -> (e,p)
    ParseOk p -> (forceEitherMsg "impossible" (processExp conf ""), p)
    ParseFailed _ b -> error b

-- Here's where the main meat of the template haskell is generated.  Given the
-- number of captures, the pattern string, and a list of capture expressions,
-- yields the template Haskell Exp which parses a string into a tuple.
buildExp :: RexConf -> Int -> String -> [Maybe Exp] -> ExpQ
buildExp RexConf{..} cnt pat xs =
    [| let r = $(get_regex) in
       $(process) . (flip $ PCRE.match r) $(liftRS rexPCREExecOpts)
     . $(if rexByteString then [| id |] else [| pack |]) |]
  where
    liftRS x = [| read shown |] where shown = show x

    --TODO: make sure this takes advantage of bytestring fusion stuff - is
    -- the right pack / unpack. Or use XOverloadedStrings
    get_regex
      | rexCompiled = [| unsafePerformIO (regexFromTable $! $(table_bytes)) |]
      | otherwise = [| PCRE.compile (pack pat) $(liftRS pcreOpts) |]
    table_bytes = [| pack $(LitE . StringL . unpack <$> runIO table_string) |]
    table_string =
      forceMaybeMsg "Error while getting PCRE compiled representation\n" <$>
      precompile (pack pat) pcreOpts
    pcreOpts = rexPCREOpts

    process = case (null vs, rexByteString) of
      (True, _)  -> [| liftM ( const () ) |]
      (_, False) -> [| liftM ($(return maps) . padRight "" pad . map unpack) |]
      (_, True)  -> [| liftM ($(return maps) . padRight empty pad) |]
    pad = cnt + 2
    maps = LamE [ListP . (WildP:) $ map VarP vs]
         . TupE . map (uncurry AppE)
         -- filter out all "Nothing" exprs
         . map (first fromJust) . filter (isJust . fst)
         -- [(Expr, Variable applied to)]
         . zip xs $ map VarE vs
    vs = [mkName $ "v" ++ show i | i <- [0..cnt]]

-- Parse a Haskell expression into a template Haskell Exp
processExp :: RexConf -> String -> ParseResult Exp
processExp conf xs
  = fmap toExp
  . parseExpWithMode rexParseMode
  $ onSpace xs (rexView conf) id

-- probably the quasiquote should have access to the pragmas in the current
-- file, but for now just enable some common extensions that do not steal
-- much syntax
rexParseMode :: ParseMode
rexParseMode = defaultParseMode{ extensions = map EnableExtension
    [ViewPatterns,
     ImplicitParams,
     RecordPuns, RecordWildCards,
     ScopedTypeVariables,
     TupleSections,
     TypeFamilies,
     TypeOperators ]}

-- Parse a Haskell pattern match into a template Haskell Pat, yielding Nothing
-- for patterns which consist of just whitespace.
processPat :: String -> ParseResult Pat
processPat xs = fmap toPat $ parsePatWithMode rexParseMode xs

-- Parsing
-------------------------------------------------------------------------------

type ParseChunk = Either String (Int, String)
type ParseChunks = (Int, String, [(Int, String)])

-- Postprocesses the results of the chunk-wise parse output, into the pattern to
-- be pased to the regex engine, with the interpolated patterns / expressions.
parseIt :: String -> ParseChunks
parseIt xs =
    ( cnt, concat [x | Left x <- results]
    , [(i, x) | Right (i, x) <- results]
    )
  where
    (cnt, results) = parseRegex (filter (`notElem` "\r\n") xs) "" (-1)

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
  [] -> error "Rex haskell splice terminator, }, never found"

-- Utils
-------------------------------------------------------------------------------

-- | A possibly useful utility function - yields 'Just' x when there is a
-- valid parse, and 'Nothing' otherwise.
maybeRead :: (Read a) => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

onSpace :: String -> a -> (String -> a) -> a
onSpace s x f | all isSpace s = x
              | otherwise = f s

-- | Given a desired list-length, if the passed list is too short, it is padded
-- with the given element.  Otherwise, it trims.
padRight :: a -> Int -> [a] -> [a]
padRight _ 0 _ = []
padRight v i [] = replicate i v
padRight v i (x:xs) = x : padRight v (i-1) xs

mapSnd :: (t -> t2) -> (t1, t) -> (t1, t2)
mapSnd f (x, y) = (x, f y)

-- From MissingH

{- | Like 'forceMaybe', but lets you customize the error message raised if
Nothing is supplied. -}
forceMaybeMsg :: String -> Maybe a -> a
forceMaybeMsg msg Nothing = error msg
forceMaybeMsg _ (Just x) = x

{- | Like 'forceEither', but can raise a specific message with the error. -}
forceEitherMsg :: String -> ParseResult a -> a
forceEitherMsg msg (ParseFailed x _) = error $ msg ++ ": " ++ show x
forceEitherMsg _ (ParseOk x) = x
