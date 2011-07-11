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
-- the PCRE
--
-----------------------------------------------------------------------------

module Text.Regex.PCRE.Precompile where

import Control.Monad (liftM)

import qualified Data.ByteString as B
import Data.ByteString.Unsafe (unsafePackCStringLen, unsafeUseAsCString)
import Data.IORef
import qualified Data.Map as M

import Foreign.ForeignPtr (withForeignPtr, newForeignPtr_)
import Foreign.Ptr (nullPtr, castPtr)
import Foreign.Marshal (alloca)
import Foreign.Storable (peek)
import Foreign.C.Types (CInt)

import Text.Regex.PCRE.Light
import Text.Regex.PCRE.Light.Base

import System.IO.Unsafe (unsafePerformIO)

precompile :: B.ByteString -> [PCREOption] -> IO (Maybe B.ByteString)
precompile pat opts = regexToTable $ compile pat opts

regexToTable :: Regex -> IO (Maybe B.ByteString)
regexToTable (Regex p _) =
  withForeignPtr p $ \pcre -> alloca $ \res -> do
    success <- c_pcre_fullinfo pcre nullPtr info_size res
    len <- return . fromIntegral =<< (peek res :: IO CInt)
    if success >= 0 
      then withForeignPtr p (liftM Just . unsafePackCStringLen . (, len) . castPtr)
      else return Nothing

regexFromTable :: B.ByteString -> IO Regex
regexFromTable = unsafePerformIO $ do
  return $ \bytes -> unsafeUseAsCString bytes
         $ \cstr -> do
           ptr <- newForeignPtr_ (castPtr cstr)
           return $ Regex ptr bytes

-- http://stackoverflow.com/questions/141650/how-do-you-make-a-generic-memoize-function-in-haskell
memoizeRegex :: String -> B.ByteString -> Regex
memoizeRegex = unsafePerformIO $ do
--  print "creating new memoize map"
  r <- newIORef M.empty
  --r2 <- newIORef [] -- Used to hold onto references to the bytestrings
  return $ \k bytes -> unsafePerformIO $ do
    m <- readIORef r
    case M.lookup k m of
      Just y -> do
--        tab <- regexToTable y
--        print ("used cache for " ++ k ++ "\nyielding " ++ show tab) 
        return y
      Nothing -> do
        result <- regexFromTable bytes
        writeIORef r (M.insert k result m)
   --     modifyIORef r2 (bytes:)
--        print ("stored cache for " ++ k ++ ":\n" ++ show bytes)
        return result
