{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Lib
    ( call
    , hash_via_c
    , hash_via_haskell
    ) where

import GHC.Prim
import GHC.Types
import Foreign.C.Types
import Data.Text.Array (Array (..))
import Data.Text.Internal (Text (..))

defaultSalt :: Int
defaultSalt = -2578643520546668380  -- 0xdc36d1615b7400a4

call f (Text (Array arr) off len) = f arr off len defaultSalt

-- | Compute a hash value for the content of this 'ByteArray#', using
-- an initial salt.
--
-- This function can for example be used to hash non-contiguous
-- segments of memory as if they were one contiguous segment, by using
-- the output of one hash as the salt for the next.
hash_via_c
    :: ByteArray#  -- ^ data to hash
    -> Int         -- ^ offset, in bytes
    -> Int         -- ^ length, in bytes
    -> Int         -- ^ salt
    -> Int         -- ^ hash value
hash_via_c ba !off !len !h =
    fromIntegral $ c_hashByteArray ba (fromIntegral off) (fromIntegral len)
    (fromIntegral h)

foreign import ccall unsafe "hashable_fnv_hash_offset" c_hashByteArray
    :: ByteArray# -> CLong -> CLong -> CLong -> CLong

hash_via_haskell
    :: ByteArray#  -- ^ data to hash
    -> Int         -- ^ offset, in bytes
    -> Int         -- ^ length, in bytes
    -> Int         -- ^ salt
    -> Int         -- ^ hash value
hash_via_haskell arr (I# off) (I# len) (I# salt) =
    let ptr = byteArrayContents# arr `plusAddr#` off
        loop (# idx, hash #) =
          case int2Word# idx `xor#` int2Word# len of
            0## -> I# (word2Int# hash)
            _ ->
              let hash' = timesWord# hash 16777619## `xor#`
                          indexWord8OffAddr# ptr idx
                  idx' = idx +# 1#
               in loop (# idx', hash' #)
     in loop (# 0#,  (int2Word# salt) #)
