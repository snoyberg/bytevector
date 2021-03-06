module Copy.Raw where

import qualified Data.ByteString               as S
import           Data.ByteString.Lazy.Internal (defaultChunkSize)
import           Data.ByteVector
import           Data.MonoTraversable          (ofoldlM)
import           Data.Vector.Generic           (unsafeFreeze)
import qualified Data.Vector.Generic.Mutable   as V
import           Data.Word                     (Word8)
import           GHC.Prim                      (RealWorld)
import           System.IO                     (Handle)

run :: Handle -> Handle -> IO ()
run inH outH = do
    let loop pair = do
            bs <- S.hGetSome inH defaultChunkSize
            if S.null bs
                then do
                    let Pair idx mbv = pair
                    bv <- unsafeFreeze mbv
                    S.hPut outH $ S.take idx $ fromByteVector bv
                else ofoldlM go pair bs >>= loop
        go (Pair idx mbv) w = do
            V.write mbv idx w
            let idx' = succ idx
            if idx' >= defaultChunkSize
                then do
                    bv <- unsafeFreeze mbv
                    S.hPut outH $ fromByteVector bv
                    newPair
                else return $! Pair idx' mbv
    newPair >>= loop

data Pair a = Pair {-# UNPACK #-} !Int !a

newPair :: IO (Pair (MByteVector RealWorld Word8))
newPair = do
    mbv <- V.new defaultChunkSize
    return $! Pair 0 mbv
