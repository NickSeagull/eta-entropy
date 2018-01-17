{-# LANGUAGE CPP, ForeignFunctionInterface, BangPatterns, ScopedTypeVariables, MagicHash #-}
{-|
 Maintainer: Thomas.DuBuisson@gmail.com
 Stability: beta
 Portability: portable

 Obtain entropy from system sources or x86 RDRAND when available.

-}

module System.EntropyJVM
        ( CryptHandle
        , openHandle
        , hGetEntropy
        , closeHandle
        , hardwareRandom
        ) where

import Control.Monad (liftM, when)
import Data.ByteString as B
import System.IO.Error (mkIOError, eofErrorType, ioeSetErrorString)
import Data.Bits (xor)

import Foreign.Ptr
import Foreign.C.Types

import Java
import Data.ByteString.Internal as B

foreign import java unsafe "@static Utils.get_rand_bytes"
  java_get_rand_bytes :: Ptr CUChar -> CSize -> IO CInt

-- import System.Posix (openFd, closeFd, fdReadBuf, OpenMode(..), defaultFileFlags, Fd)

-- |Handle for manual resource management
data CryptHandle
    = CH SecureRandom

data SecureRandom = SecureRandom @java.security.SecureRandom
  deriving Class

foreign import java unsafe "@new" newSecureRandom :: IO SecureRandom


-- | Get random values from the hardward RNG or return Nothing if no
-- supported hardware RNG is available.
--
-- Supported hardware:
--      * RDRAND
--      * Patches welcome
hardwareRandom :: Int -> IO (Maybe B.ByteString)
hardwareRandom n = do
  random <- newSecureRandom
  Just <$> hGetEntropy (CH random) n

-- |Open a `CryptHandle`
openHandle :: IO CryptHandle
openHandle = do
  CH `fmap` newSecureRandom

-- |Close the `CryptHandle`
closeHandle :: CryptHandle -> IO ()
closeHandle (CH _) = return ()

-- |Read random data from a `CryptHandle`
hGetEntropy :: CryptHandle -> Int -> IO B.ByteString
hGetEntropy (CH _) n =
  B.create n filler
 where
  filler ptr = do
    r <- java_get_rand_bytes (castPtr ptr) (fromIntegral n)
    when (r/= 0) (fail "entropy library: Something bad occurred on the Java side")

