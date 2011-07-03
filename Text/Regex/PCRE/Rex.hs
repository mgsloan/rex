{-# LANGUAGE TemplateHaskell, QuasiQuotes, TupleSections, ParallelListComp, ViewPatterns #-}

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
-- patterns - uses "maybeRead" function, which yields a "(Read a) => Just a"
-- value on successful parse.
--
-- 4) Allows for the inline interpolation of a mapping function String -> a.
--
-- Here's a silly example which parses peano numbers of the form "Z", "S Z",
-- "S S Z", etc.  The \s+ means that it is not sensitive to the quantity or type
-- of seperating whitespace. (these examples can also be found in Test.hs)
--
-- > peano :: String -> Int
-- > peano = [rex|^(?{ length . filter (=='S') } \s* (?:S\s+)*Z)\s*$|]
--
-- > *Main> peano "Z"
-- > 0
-- > *Main> peano "S Z"
-- > 1
-- > *Main> peano "S   S Z"
-- > 2
-- > *Main> peano "S S S Z"
-- > 3
-- > *Main> peano "invalid"
-- > 0
--
-- The token \"(?{\" introduces a capture group which has a mapping applied to the
-- result - in this case \"length . filter (=='S')\".  In the case that the match
-- fails, \"\" is provided as input.  \"}\" ends the Haskell mapping expression.
--
-- If an expression is omitted, \"(?{} ... )\", then there is an implicit usage of
-- the \"maybeRead :: (Read a) => String -> Maybe a\" function provided by this
-- module.  If the ?{ ... } is omitted, then the capture group is not taken as
-- part of the results of the match.
-- 
-- > vect2d :: String -> (Maybe Int, Maybe Int)
-- > vect2d = [rex|^<\s* (?{}\d+) \s*,\s* (?{}\d+) \s*>$|]
--
-- The following example is derived from http://www.regular-expressions.info/dates.html
--
-- > parseDate :: String -> Maybe (Int, Int, Int)
-- > parseDate [rex|^(?{ Just y }(?:19|20)\d\d)[- /.]
-- >                 (?{ Just m }0[1-9]|1[012])[- /.]
-- >                 (?{ Just d }0[1-9]|[12][0-9]|3[01])$|]
-- >   |  (d > 30 && (m `elem` [4, 6, 9, 11]))
-- >   || (m == 2 &&
-- >        (d ==29 && not (mod y 4 == 0 && (mod y 100 /= 0 || mod y 400 == 0)))
-- >     || (d > 29)) = Nothing
-- > 
-- >   | otherwise = Just (y, m, d)
--
-- The above example makes use of the regex quasi-quoter as a pattern matcher.
-- The interpolated Haskell patterns are used to construct an implicit view
-- pattern.  The above pattern is expanded to the equivalent:
--
-- > parseDate ([rex|^(?{}(?:19|20)\d\d)[- /.]
-- >                  (?{}0[1-9]|1[012])[- /.]
-- >                  (?{}0[1-9]|[12][0-9]|3[01])$|]
-- >           -> (Just y, Just m, Just d))
--
-- Since this is how the patterns desugar, if you wish for the pattern to fall
-- through to the next case on failed regex match, then at least one of the 
-- captures needs to fail on the empty string.
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
-- Inspired by / copy-modified from Matt Morrow's regexqq package:
-- http://hackage.haskell.org/packages/archive/regexqq/latest/doc/html/src/Text-Regex-PCRE-QQ.html
-- And code from Erik Charlebois's interpolatedstring-qq package:
-- http://hackage.haskell.org/packages/archive/interpolatedstring-qq/latest/doc/html/Text-InterpolatedString-QQ.html
--
-----------------------------------------------------------------------------

module Text.Regex.PCRE.Rex (rex, makeExpr, maybeRead, pack, unpack) where

import Text.Regex.PCRE.Light (Regex,PCREOption,PCREExecOption)
import qualified Text.Regex.PCRE.Light as PCRE

import Control.Arrow (second)
import Control.Monad (liftM)

import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w,w2c)
import Data.Either.Utils (forceEitherMsg)
import Data.List (groupBy, inits, sortBy)
import Data.List.Split (split, onSublist)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ord (comparing)
import Data.Char (isSpace)

import Debug.Trace

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse

type ParseChunk = Either String (Int, String)
type ParseChunks = (Int, String, [(Int, String)])

{- TODO: idea - provide a variant which allows splicing in an expression
  that evaluates to regex string.  The tricky thing here is that this
  splice might contain capture groups - thus, the parser here would need to
  be referenced in order to dispatch 
-}

{- TODO: target Text.Regex.Base -}

-- | The regular expression quasiquoter.
rex :: QuasiQuoter
rex = QuasiQuoter
        (checkRegex makeExpr . parseIt)
        (checkRegex makePat . parseIt)
        undefined undefined

-- Gives an error at compile time if the regex string is invalid.
checkRegex :: (ParseChunks -> a) -> ParseChunks -> a
checkRegex f x@(_, pat, _) = 
  forceEitherMsg ("Error compiling regular expression [reg|"  ++ pat ++ "|]\n")
    (PCRE.compileM (pack pat) pcreOpts) `seq` f x


-- Template Haskell Code Generation
-------------------------------------------------------------------------------

-- Creates the template haskell Exp which corresponds to the parsed interpolated
-- regex.  This particular code mainly just handles making "maybeRead" the
-- default for captures which lack a parser definition, and defaulting to making
-- the parser that doesn't exist
makeExpr :: ParseChunks -> ExpQ
makeExpr (cnt, pat, exs) = buildExpr cnt pat
    . map ((>>= processExpr) . snd . head)
    . groupSortBy (comparing fst)
    $ map (second Just) exs ++ [(i, Nothing) | i <- [0..cnt]]
  
-- Creates the template haskell Pat which corresponds to the parsed interpolated
-- regex. As well as handling the aforementioned defaulting considerations, this
-- turns per-capture view patterns into a single tuple-resulting view pattern.
-- 
-- E.g. [reg| ... (?{e1 -> v1} ...) ... (?{e2 -> v2} ...) ... |] becomes
--      [reg| ... (?{e1} ...) ... (?{e2} ...) ... |] -> (v1, v2)
makePat :: ParseChunks -> PatQ
makePat (cnt, pat, exs) = do
  viewExp <- buildExpr cnt pat $ map (liftM fst) ys
  return . ViewP viewExp . TupP . map snd $ catMaybes ys
 where
  ys = map ((>>= floatBoth) . processView . snd . head)
     . groupSortBy (comparing fst)
     $ exs ++ [(i, "") | i <- [0..cnt]]

  processView "" = Nothing
  processView xs = case splitFromBack 2 ((split . onSublist) "->" xs) of
    (_, [r]) -> onSpace Nothing (Just . (processExpr "maybeRead",) . processPat) r
    (concat -> l, [_, r]) -> Just (processExpr (onSpace "maybeRead" id $ l), processPat r)
    (_, _) -> Nothing

-- Here's where the main meat of the template haskell is generated.  Given the
-- number of captures, the pattern string, and a list of capture expressions,
-- yields the template Haskell Exp which parses a string into a tuple.
buildExpr :: Int -> String -> [Maybe Exp] -> ExpQ
buildExpr cnt pat hexps = do
  vx <- newName "x"
  emptyE <- [|""|]
  caseExp <-  [| fmap (map unpack)
               $ PCRE.match (regex pat) (pack $(return $ VarE vx)) pcreExecOpts |]
  let mkMatch rcnt xs = Match
        (if null xs then ConP (mkName "Nothing") []
                    else ConP (mkName "Just") . single . ListP . (WildP:) $ map VarP xs)
        (NormalB . TupE . map (uncurry AppE) . catMaybes
                 . zipWith (curry floatFst) hexps 
                 $ (map VarE xs) ++ replicate rcnt emptyE)
        []
  return . LamE [VarP vx] . CaseE caseExp . zipWith mkMatch [cnt+1,cnt..]
         $ inits [mkName $ "r" ++ show i | i <- [0..cnt]]

-- Parse a Haskell expression into a template Haskell Exp, yielding Nothing for
-- strings which just consist of whitespace.
processExpr :: String -> Maybe Exp
processExpr xs = Just . forceEitherMsg ("Error while parsing capture mapper " ++ xs)
               . parseExp . onSpace "maybeRead" id $ xs

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

pack :: String -> B.ByteString
pack = B.pack . fmap c2w

unpack :: B.ByteString -> String
unpack = fmap w2c . B.unpack

-- Compiles a regular expression with the default options.
regex :: String -> Regex
regex = flip PCRE.compile pcreOpts . pack

-- | The default read definition - yields "Just x" when there is a valid parse,
--   and Nothing otherwise.
maybeRead :: (Read a) => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads


-- TODO: allow for a bundle of parameters to be passed in at the beginning
-- of the quasiquote. Would also be a juncture for informing arity /
-- extension information.

pcreOpts :: [PCREOption]
pcreOpts =
  [ PCRE.extended
  , PCRE.multiline ]
  -- , dotall, caseless, utf8
  -- , newline_any, PCRE.newline_crlf ]

pcreExecOpts :: [PCREExecOption]
pcreExecOpts = []
  -- [ PCRE.exec_newline_crlf
  -- , exec_newline_any, PCRE.exec_notempty
  -- , PCRE.exec_notbol, PCRE.exec_noteol ]

single :: a -> [a]
single x = [x]

groupSortBy :: (a -> a -> Ordering) -> [a] -> [[a]]
groupSortBy f = groupBy (\x -> (==EQ) . f x) . sortBy f

debug :: (Show a) => a -> a
debug x = trace (show x) x

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

floatFst :: (Functor f) => (f a, b) -> f (a, b)
floatFst (x, y) = fmap (,y) x

floatSnd :: (Functor f) => (a, f b) -> f (a, b)
floatSnd (x, y) = fmap (x,) y

floatBoth :: (Monad m) => (m a, m b) -> m (a, b)
floatBoth (x, y) = do
  x' <- x
  y' <- y
  return (x', y')

splitFromBack :: Int -> [a] -> ([a], [a])
splitFromBack i xs = (reverse b, reverse a)
  where (a, b) = splitAt i $ reverse xs

dropAllBut :: Int -> [a] -> [a]
dropAllBut i = reverse . take i . reverse

onSpace :: a -> (String -> a) -> String -> a
onSpace x f s | all isSpace s = x
              | otherwise = f s

mapLeft :: (a -> a') -> Either a b -> Either a' b
mapLeft f = either (Left . f) Right

mapRight :: (b -> b') -> Either a b -> Either a b'
mapRight f = either Left (Right . f)

{-
--TODO: use something like this to cache compiled regex.
-- Even better would be to do the compilation step compile time :)
-- http://stackoverflow.com/questions/141650/how-do-you-make-a-generic-memoize-function-in-haskell
memoize :: Ord a => (a -> b) -> (a -> b)
memoize f = unsafePerformIO $ do 
    r <- newIORef Map.empty
    return $ \ x -> unsafePerformIO $ do 
        m <- readIORef r
        case Map.lookup x m of
            Just y  -> return y
            Nothing -> do 
                    let y = f x
                    writeIORef r (Map.insert x y m)
                    return y
-}
