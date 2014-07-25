module Copy.PeekPoke where

import qualified Data.ByteString               as S
import           Data.ByteString.Internal      (ByteString (PS),
                                                mallocByteString)
import           Data.ByteString.Lazy.Internal (defaultChunkSize)
import           Data.MonoTraversable          (ofoldlM)
import           Data.Word                     (Word8)
import           Foreign.ForeignPtr            (ForeignPtr)
import           Foreign.ForeignPtr.Unsafe     (unsafeForeignPtrToPtr)
import           Foreign.Storable              (pokeByteOff)
import           System.IO                     (Handle)

run :: Handle -> Handle -> IO ()
run inH outH = do
    let loop s = do
            bs <- S.hGetSome inH defaultChunkSize
            if S.null bs
                then do
                    let S idx fptr = s
                    S.hPut outH $ PS fptr 0 idx
                else ofoldlM go s bs >>= loop
        go (S idx fptr) w = do
            pokeByteOff (unsafeForeignPtrToPtr fptr) idx w
            let idx' = succ idx
            if idx' >= defaultChunkSize
                then do
                    S.hPut outH $ PS fptr 0 defaultChunkSize
                    newS
                else return $! S idx' fptr
    newS >>= loop

data S = S
    {-# UNPACK #-} !Int -- index
    {-# UNPACK #-} !(ForeignPtr Word8)

newS :: IO S
newS = do
    fptr <- mallocByteString defaultChunkSize
    return $! S 0 fptr
