{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Regex.PCRE.Rex
-- Copyright   :  (c) Michael Sloan 2011
--
-- Maintainer  :  Michael Sloan (mgsloan@gmail.com)
-- Stability   :  unstable
-- Portability :  unportable
-- 
-- This module provides support for accessing the compiled representation of
-- PCRE regular expressions.  This byte array encodes a lookup-table based
-- representation for the regular expression, and can be safely written
-- out and read back by compatible PCRE versions.
--
-----------------------------------------------------------------------------

module Text.Regex.PCRE.Precompile where

import Control.Monad (liftM)

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Unsafe (unsafePackCStringLen, unsafeUseAsCString)

import Foreign.ForeignPtr (withForeignPtr, newForeignPtr_)
import Foreign.Ptr (nullPtr, castPtr)
import Foreign.Marshal (alloca)
import Foreign.Storable (peek)
import Foreign.C.Types (CInt)

import Text.Regex.PCRE.Light
import Text.Regex.PCRE.Light.Base

-- | Compiles the given regular expression, and assuming nothing bad
-- happens, yields the bytestring filled with PCRE's compiled
-- representation.
precompile :: B.ByteString -> [PCREOption] -> IO (Maybe B.ByteString)
precompile pat opts = regexToTable $ compile pat opts

-- | Takes a compiled regular expression adn yields its bytestring
-- representation.
regexToTable :: Regex -> IO (Maybe B.ByteString)
regexToTable (Regex p _) =
  withForeignPtr p $ \pcre -> alloca $ \res -> do
    success <- c_pcre_fullinfo pcre nullPtr info_size res
    len <- return . fromIntegral =<< (peek res :: IO CInt)
    if success >= 0 
      then withForeignPtr p (liftM Just  . unsafePackCStringLen . (, len) . castPtr)
      else return Nothing

-- | Creates a regular expression
regexFromTable :: B.ByteString -> IO Regex
regexFromTable bytes = unsafeUseAsCString bytes $ \cstr -> do
   ptr <- newForeignPtr_ (castPtr cstr)
   return $ Regex ptr bytes
