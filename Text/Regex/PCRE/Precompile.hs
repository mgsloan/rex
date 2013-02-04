{-# LANGUAGE MagicHash, BangPatterns #-}
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

import Control.Monad          (liftM)
import Data.ByteString.Char8  (ByteString, packCStringLen)
import Data.ByteString.Internal (toForeignPtr)
import Foreign.ForeignPtr     (withForeignPtr)
import Foreign.Ptr            (nullPtr, castPtr)
import Foreign.Marshal        (alloca)
import Foreign.Storable       (peek)
import Foreign.C.Types        (CSize)
import GHC.Exts               (Int(..), plusAddr#)
import GHC.ForeignPtr         (ForeignPtr(..))
import Text.Regex.PCRE.Light
import Text.Regex.PCRE.Light.Base

-- | A synonym indicating which ByteStrings represent PCRE-format compiled data.
type CompiledBytes = ByteString

-- | Compiles the given regular expression, and assuming nothing bad happens,
-- yields the bytestring filled with PCRE's compiled representation.
precompile :: ByteString -> [PCREOption] -> IO (Maybe CompiledBytes)
precompile pat opts = regexToTable $ compile pat opts

-- | Takes a compiled regular expression, and if successful, yields the
-- compiled representation.
regexToTable :: Regex -> IO (Maybe CompiledBytes)
regexToTable (Regex p _) =
  withForeignPtr p $ \pcre -> alloca $ \res -> do
    success <- c_pcre_fullinfo pcre nullPtr info_size res
    len <- return . fromIntegral =<< (peek res :: IO CSize)
    if success >= 0 
      then liftM Just $ packCStringLen (castPtr pcre, len)
      else return Nothing

-- | Creates a regular expression from the compiled representation.
regexFromTable :: CompiledBytes  -> IO Regex
regexFromTable bytes =
  return $ Regex (ForeignPtr (plusAddr# addr offset) content) bytes
 where
  !(ForeignPtr addr content, I# offset, _) = toForeignPtr bytes
