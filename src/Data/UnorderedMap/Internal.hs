{-# LANGUAGE ForeignFunctionInterface #-}

module Data.UnorderedMap.Internal where

import Prelude hiding (lookup)
import Foreign hiding (unsafeForeignPtrToPtr)
import Foreign.C.Types

data UMO  -- unordered_map object
-- type UnorderedMap_ = ForeignPtr UMO

foreign import ccall unsafe "hashmap.h hashmap_create"
  hashmap_create :: IO (Ptr UMO)

foreign import ccall unsafe "hashmap.h hashmap_create_sized"
  hashmap_create_sized :: CSize -> IO (Ptr UMO)

foreign import ccall unsafe "hashmap.h &hashmap_destroy"
  hashmap_destroy :: FinalizerPtr UMO

foreign import ccall unsafe "hashmap.h hashmap_insert"
  hashmap_insert :: (Ptr UMO) -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> IO ()

foreign import ccall unsafe "hashmap.h hashmap_lookup"
  hashmap_lookup :: (Ptr UMO) -> Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

foreign import ccall unsafe "hashmap.h hashmap_delete"
  hashmap_delete :: (Ptr UMO) -> Ptr Word8 -> CSize -> IO ()

foreign import ccall unsafe "hashmap.h hashmap_size"
  hashmap_size :: (Ptr UMO) -> IO CSize

data UMIO -- unordered_map::iterator object

foreign import ccall unsafe "hashmap.h iter_create"
  iter_create :: (Ptr UMO) -> IO (Ptr UMIO)

foreign import ccall unsafe "hashmap.h iter_destroy"
  iter_destroy :: (Ptr UMIO) -> IO ()

foreign import ccall unsafe "hashmap.h iter_hasNext"
  iter_hasNext :: (Ptr UMO) -> (Ptr UMIO) -> IO Bool

foreign import ccall unsafe "hashmap.h iter_next"
  iter_next :: (Ptr UMO) -> (Ptr UMIO) -> Ptr (Ptr Word8) -> Ptr CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
