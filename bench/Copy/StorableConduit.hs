{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
module Copy.StorableConduit where

import           Conduit
import qualified Data.ByteString               as S
import           Data.ByteString.Internal (ByteString (PS))
import           Data.ByteString.Lazy.Internal (defaultChunkSize)
import           Data.Conduit.Combinators      (vectorBuilder)
import           System.IO                     (Handle)
import qualified Data.Vector.Storable as V
import Data.Word (Word8)

fromStorableVector :: V.Vector Word8 -> S.ByteString
fromStorableVector v =
    PS fptr offset idx
  where
    (fptr, offset, idx) = V.unsafeToForeignPtr v

run :: Handle -> Handle -> IO ()
run inH outH =
    (sourceHandle inH :: Producer IO S.ByteString)
        $$ vectorBuilder defaultChunkSize mapM_CE
        =$ mapC fromStorableVector
        =$ sinkHandle outH

