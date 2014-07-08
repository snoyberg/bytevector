{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Data.ByteVector
    ( MByteVector
    , ByteVector
    , fromByteVector
    , toByteVector
    ) where

import           Control.Monad.Primitive     (unsafePrimToPrim)
import qualified Data.ByteString             as S
import           Data.ByteString.Internal    (mallocByteString)
import           Data.ByteString.Internal    (ByteString (PS))
import qualified Data.ByteString.Unsafe      as BU
import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Generic.Mutable as VGM
import           Data.Word8                  (Word8)
import           Foreign.ForeignPtr          (ForeignPtr)
import           Foreign.ForeignPtr.Unsafe   (unsafeForeignPtrToPtr)
import           Foreign.Ptr                 (Ptr)
import           Foreign.Storable            (peekByteOff, pokeByteOff)

data MByteVector s a = MByteVector
    { mbsFptr   :: !(ForeignPtr Word8)
    , mbsOffset :: {-# UNPACK #-} !Int
    , mbsLen    :: {-# UNPACK #-} !Int
    }

mbsPtr :: MByteVector s a -> Ptr Word8
mbsPtr = unsafeForeignPtrToPtr . mbsFptr
{-# INLINE mbsPtr #-}

instance a ~ Word8 => VGM.MVector MByteVector a where
    basicLength = mbsLen
    {-# INLINE basicLength #-}

    basicUnsafeSlice start2 len (MByteVector fptr start1 _) =
        MByteVector fptr (start1 + start2) len
    {-# INLINE basicUnsafeSlice #-}

    basicOverlaps x y = mbsPtr x == mbsPtr y
    {-# INLINE basicOverlaps #-}

    basicUnsafeNew len = do
        fptr <- unsafePrimToPrim $ mallocByteString len
        return $! MByteVector fptr 0 len
    {-# INLINE basicUnsafeNew #-}

    basicUnsafeRead mbs idx = unsafePrimToPrim $ peekByteOff (mbsPtr mbs) (idx + mbsOffset mbs)
    {-# INLINE basicUnsafeRead #-}

    basicUnsafeWrite mbs idx w = unsafePrimToPrim $ pokeByteOff (mbsPtr mbs) (idx + mbsOffset mbs) w
    {-# INLINE basicUnsafeWrite #-}

newtype ByteVector a = ByteVector S.ByteString

toByteVector :: S.ByteString -> ByteVector Word8
toByteVector = ByteVector
{-# INLINE toByteVector #-}

fromByteVector :: ByteVector Word8 -> S.ByteString
fromByteVector (ByteVector bs) = bs
{-# iNLINE fromByteVector #-}

instance a ~ Word8 => VG.Vector ByteVector a where
    basicUnsafeFreeze (MByteVector fptr off len) = return $! ByteVector $! PS fptr off len
    {-# INLINE basicUnsafeFreeze #-}

    basicUnsafeThaw (ByteVector (PS fptr off len)) =
        return $! MByteVector fptr off len
    {-# INLINE basicUnsafeThaw #-}

    basicLength = S.length . fromByteVector
    {-# INLINE basicLength #-}

    basicUnsafeSlice start len (ByteVector bs) = ByteVector $! BU.unsafeTake len $! BU.unsafeDrop start bs
    {-# INLINE basicUnsafeSlice #-}

    basicUnsafeIndexM (ByteVector bs) idx = return $! BU.unsafeIndex bs idx
    {-# INLINE basicUnsafeIndexM #-}

type instance VG.Mutable ByteVector = MByteVector
