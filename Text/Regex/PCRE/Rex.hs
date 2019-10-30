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
-- This module provides a template Haskell quasiquoter for regular expressions,
-- which provides the following features:
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
-- Inspired by Matt Morrow's regexqq package:
-- <http://hackage.haskell.org/package/regexqq/docs/Text-Regex-PCRE-QQ.html>.
--
-- And some code from Erik Charlebois's interpolatedstring-qq package:
-- <http://hackage.haskell.org/package/interpolatedstring-qq/>.
--
-----------------------------------------------------------------------------

module Text.Regex.PCRE.Rex
  (

-- * Language Extensions
-- |
-- Since this is a quasiquoter library that generates code using view patterns,
-- the following extensions are required:
--
-- > {-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns #-}

-- * First Example
-- |
-- Here's an example which parses peano numbers of the form Z, S Z, S S Z, etc.
-- The \s+ means that it is not sensitive to the quantity or type of separating
-- whitespace.  These examples can also be found in Test.hs.
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
-- the result. In this case, it's @length . filter (=='S')@.  If the ?{ ... }
-- are omitted, then the capture group is not taken as part of the results of
-- the match.  If the contents of the ?{ ... } is omitted, then a call to
-- 'rexView' is assumed:
--
-- > parsePair :: String -> Maybe (String, String)
-- > parsePair = [rex|^<\s* (?{ }[^\s,>]+) \s*,\s* (?{ }[^\s,>]+) \s*>$|]
--
-- The 'rexView' exported by this module is just equal to 'id', so by default
-- no preprocessing is done.  However, we can shadow this locally:
--
-- > parsePair' :: String -> Maybe (Int, Int)
-- > parsePair' = [rex|^<\s* (?{ }[^\s,>]+) \s*,\s* (?{ }[^\s,>]+) \s*>$|]
-- >   where
-- >     rexView = read
--
-- Additional shorthands can be added by using 'rexWithConf' and specifying
-- custom values for 'rexPreprocessExp' or 'rexPreprocessPat'.

-- * Second Example
-- |
-- This example is derived from
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

-- * ByteStrings vs Strings
-- |
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

-- * Future Work
-- |
-- There are a few things that could potentially be improved:
--
-- 1) PCRE captures, unlike .NET regular expressions, yield the last capture
-- made by a particular pattern.  So, for example, (...)*, will only yield one
-- match for '...'.  Ideally these would be detected and yield an implicit [a].
--
-- 2) Patterns with disjunction between captures ((?{f}a) | (?{g}b)) will
-- provide the empty string to one of f / g.  In the case of pattern
-- expressions, it would be convenient to be able to map multiple captures into
-- a single variable / pattern, preferring the first non-empty option.

-- * Quasiquoters
    rex, brex
-- * Configurable QuasiQuoter
  , rexWithConf, RexConf(..), defaultRexConf
-- * Utilities
  , makeQuasiMultiline
  , eitherToParseResult
  , parseExp
  , parsePat
  , rexParseMode
-- * Used by the generated code
  , rexView
  ) where

import Text.Regex.PCRE.Precompile

import qualified Text.Regex.PCRE.Light as PCRE

import Control.Applicative   ( (<$>) )
import Control.Arrow         ( first )
import Data.ByteString.Char8 ( ByteString, pack, unpack, empty )
import Data.Either           ( partitionEithers )
import Data.Maybe            ( catMaybes )
import Data.Char             ( isSpace )
import System.IO.Unsafe      ( unsafePerformIO )

import Language.Haskell.TH (Body(..), Dec(..), Exp(..), ExpQ, Pat(..), PatQ, Lit(..),
                            mkName, newName, runIO)
import Language.Haskell.TH.Quote
import Language.Haskell.Meta (toExp,toPat)
import Language.Haskell.Exts.Extension (Extension(..), KnownExtension(..))
import Language.Haskell.Exts (parseExpWithMode, parsePatWithMode,
                              ParseMode, defaultParseMode, extensions,
                              ParseResult(..))
import Language.Haskell.Exts.SrcLoc (noLoc)

{- TODO:
  * Target Text.Regex.Base ?
  * Add unit tests
-}

data RexConf = RexConf {
  -- | When @True@, the input type is a ByteString, otherwise, it's a String.
  rexByteString :: Bool,
  -- | When @True@, the regex is precompiled.
  rexCompiled :: Bool,
  -- | Preprocess the string used in expression antiquotes.  'defaultRexConf'
  --   just passes through the string unaltered, unless it just consists of
  --   whitespace.  When it's all whitespace, @"rexView"@ is used.
  rexPreprocessExp :: String -> String,
  -- | Preprocess the string used in pattern antiquotes. 'defaultRexConf'
  --   adds parenthesis around the string, so that view patterns will parse
  --   without requiring parenthesis around them.
  rexPreprocessPat :: String -> String,
  -- | When a pattern match doesn't have a view pattern, this expression is
  --   used to preprocess it before matching.  When 'defaultRexConf' is used,
  --   perhaps via 'rex' or 'brex', a reference to @rexView@ is used.
  --
  --   The 'rexView' exported by this module is 'id', so by default no
  --   preprocessing is done before
  rexViewExp :: Exp,
  -- | Options used when compiling PCRE regular expressions.
  rexPCREOpts :: [PCRE.PCREOption],
  -- | Options used when executing PCRE regular expressions.
  rexPCREExecOpts :: [PCRE.PCREExecOption]
  }

-- | Default rex configuration, which specifies that the regexes operate on
--   strings, don't post-process the matched patterns, and use 'PCRE.extended'.
--   This setting causes whitespace to be non-semantic, and ignores # comments.
defaultRexConf :: RexConf
defaultRexConf = RexConf
  { rexByteString = False
  , rexCompiled = True
  , rexPreprocessExp = \s -> if all isSpace s then "rexView" else s
  , rexPreprocessPat = \s -> "(" ++ s ++ ")"
  , rexViewExp = VarE (mkName "rexView")
  , rexPCREOpts = [PCRE.extended]
  , rexPCREExecOpts = []
  }

-- | Rex quasiquoter which takes 'String' as input, and uses 'defaultRexConf'
--   for its configuration.  Can be used in expressions and patterns.
rex :: QuasiQuoter
rex  = rexWithConf defaultRexConf

-- | Rex quasiquoter which takes 'ByteString' as input, and otherwise uses
--  'defaultRexConf' for its configuration.  Can be used in expressions and
--  patterns.
brex :: QuasiQuoter
brex = rexWithConf defaultRexConf { rexByteString = True }

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
    pre = removeLineSpaces . lines
    removeLineSpaces [] = []
    removeLineSpaces (x:xs) = x ++ concatMap (dropWhile isSpace) xs

-- | A configureable regular-expression QuasiQuoter.  Takes the options to pass
--   to the PCRE engine, along with 'Bool's to flag 'ByteString' usage and
--   non-compilation respecively.  The provided 'String' indicates which mapping
--   function to use, when one is omitted - \"(?{} ...)\".
rexWithConf :: RexConf -> QuasiQuoter
rexWithConf conf =
  QuasiQuoter
    (makeExp conf . parseRex)
    (makePat conf . parseRex)
    undefined
    undefined

-- Template Haskell Code Generation
--------------------------------------------------------------------------------

-- Creates the template haskell Exp which corresponds to the parsed interpolated
-- regex.  This particular code mainly just handles making "read" the
-- default for captures which lack a parser definition, and defaulting to making
-- the parser that doesn't exist
makeExp :: RexConf -> ParseChunks -> ExpQ
makeExp conf (cnt, pat, exs) =
  buildExp conf cnt pat $ flip map exs $ fmap $
    fromParseOk "While parsing expression antiquote"
    . parseExp
    . rexPreprocessExp conf

-- Creates the template haskell Pat which corresponds to the parsed interpolated
-- regex. As well as handling the aforementioned defaulting considerations, this
-- turns per-capture view patterns into a single tuple-resulting view pattern.
--
-- E.g. [reg| ... (?{e1 -> v1} ...) ... (?{e2 -> v2} ...) ... |] becomes
--      [reg| ... (?{e1} ...) ... (?{e2} ...) ... |] -> (v1, v2)
makePat :: RexConf -> ParseChunks -> PatQ
makePat conf (cnt, pat, exs) = do
  viewExp <- buildExp conf cnt pat $ map (fmap fst) views
  return . ViewP viewExp
         . (\xs -> ConP 'Just [TupP xs])
         . map snd $ catMaybes views
 where
  views :: [Maybe (Exp, Pat)]
  views = map (fmap processView) exs

  processView :: String -> (Exp, Pat)
  processView xs = case parsePat (rexPreprocessPat conf xs) of
    ParseOk (ParensP (ViewP e p)) -> (e,p)
    ParseOk p -> (rexViewExp conf, p)
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

    get_regex
      | rexCompiled = [| unsafePerformIO (regexFromTable $! $(table_bytes)) |]
      | otherwise = [| PCRE.compile (pack pat) $(liftRS pcreOpts) |]
    table_bytes = [| pack $(LitE . StringL . unpack <$> runIO table_string) |]
    table_string =
      fromJust' "Error while getting PCRE compiled representation\n" <$>
      precompile (pack pat) pcreOpts
    pcreOpts = rexPCREOpts

    process = case (null vs, rexByteString) of
      (True, _)  -> [| fmap ( const () ) |]
      (_, False) -> [| fmap ($(maps 'unconsStr)) |]
      (_, True)  -> [| fmap ($(maps 'unconsByte)) |]
    maps def = do
      vsName <- newName "vs"
      lets <- makeLets vsName . (WildP:) $ map VarP vs
      return $ LamE [VarP vsName] . LetE lets
         . TupE
         -- filter out all "Nothing" exprs
         -- [(Expr, Variable applied to)]
         $ [AppE x (VarE v) | (Just x, v) <- zip xs vs]
      where
        makeLets _ [] = return []
        makeLets vsName (y:ys)
          | null ys = return [makeLet WildP] -- special case so we don't create a variable we don't use
          | otherwise = do
            innerVsName <- newName "vs"
            let yLet = makeLet (VarP innerVsName)
            yLets <- makeLets innerVsName ys
            return $ yLet:yLets
          where
            makeLet innerVs = ValD (TupP [y,innerVs]) (NormalB (AppE (VarE def) (VarE vsName))) []
    vs = [mkName $ "v" ++ show i | i <- [0..cnt]]

-- | Converts @Left@ to @'ParseFailed' 'noLoc'@, and a @Right@ to @'ParseOk'@.
eitherToParseResult :: Either String a -> ParseResult a
eitherToParseResult (Left err) = ParseFailed noLoc err
eitherToParseResult (Right x) = ParseOk x

-- | Parse a Haskell expression into a Template Haskell Exp.
parseExp :: String -> ParseResult Exp
parseExp = fmap toExp . parseExpWithMode rexParseMode

-- | Parse a Haskell pattern match into a Template Haskell Pat.
parsePat :: String -> ParseResult Pat
parsePat = fmap toPat . parsePatWithMode rexParseMode

-- | Parse mode used by 'parseExp' and 'parsePat'.
rexParseMode :: ParseMode
rexParseMode = defaultParseMode { extensions = map EnableExtension exts }
  where
    -- probably the quasiquote should have access to the pragmas in the current
    -- file, but for now just enable some common extensions that do not steal
    -- much syntax
    exts =
      [ ViewPatterns
      , ImplicitParams
      , RecordPuns
      , RecordWildCards
      , ScopedTypeVariables
      , TupleSections
      , TypeFamilies
      , TypeOperators
      ]

-- Parsing
--------------------------------------------------------------------------------

type ParseChunk = Either String (Maybe String)
type ParseChunks = (Int, String, [Maybe String])

-- Postprocesses the results of the chunk-wise parse output, into the pattern to
-- be pased to the regex engine, with the interpolated patterns / expressions.
parseRex :: String -> ParseChunks
parseRex xs = (cnt, concat chunks, quotes)
  where
    (chunks, quotes) = partitionEithers results
    (cnt, results) = parseRegex (filter (`notElem` "\r\n") xs) "" (-1)

-- A pair of mutually-recursive functions, one for processing the quotation
-- and the other for the anti-quotation.

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
                    $ parseAntiquote xs "" (ix + 1)

  -- Keep track of how many capture groups we've seen.
  ('(':xs) -> mapSnd (Right Nothing :)
            $ parseRegex xs ('(':s) (ix + 1)

  -- Consume the regular expression contents.
  (x:xs) -> parseRegex xs (x:s) ix
  [] -> (ix, [Left $ reverse s])

parseAntiquote :: String -> String -> Int -> (Int, [ParseChunk])
parseAntiquote inp s ix = case inp of
  -- Escape } in the Haskell splice using a backslash.
  ('\\':'}':xs) -> parseAntiquote xs ('}':s) ix

  -- Capture accumulated antiquote, and continue parsing regex literal.
  ('}':xs) -> mapSnd ((Right (Just (reverse s))):)
            $ parseRegex xs "" ix

  -- Consume the antiquoute contents, appending to a reverse accumulator.
  (x:xs) -> parseAntiquote xs (x:s) ix
  [] -> error "Rex haskell splice terminator, }, never found"

-- Utils
--------------------------------------------------------------------------------

unconsStr :: [ByteString] -> (String,[ByteString])
unconsStr [] = ("",[])
unconsStr (x:xs) = (unpack x,xs)

unconsByte :: [ByteString] -> (ByteString,[ByteString])
unconsByte [] = (empty,[])
unconsByte (x:xs) = (x,xs)

-- | A default view function used when expression antiquotes are empty, or when
--   pattern antiquotes omit a view pattern.  See the documentation for
--   'rexPreprocessPat' and 'rexPreprocessExp' for more details.
--
--   You can locally shadow this 'rexView' with your own version, if you wish.
--   One good option is readMay from the safe package:
--   <http://hackage.haskell.org/package/safe/docs/Safe.html#v:readMay>.
--
--   The type of this identity rexView is fully polymorphic so that it can be
--   used with either 'String' or 'ByteString'.
rexView :: a -> a
rexView = id

mapSnd :: (t -> t2) -> (t1, t) -> (t1, t2)
mapSnd f (x, y) = (x, f y)

fromJust' :: String -> Maybe a -> a
fromJust' msg Nothing = error msg
fromJust' _ (Just x) = x

fromParseOk :: Show a => String -> ParseResult a -> a
fromParseOk _ (ParseOk x) = x
fromParseOk msg err = error $ msg ++ ": " ++ show err
