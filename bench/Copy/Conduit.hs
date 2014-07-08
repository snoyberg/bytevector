{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
module Copy.Conduit where

import           Conduit
import qualified Data.ByteString               as S
import           Data.ByteString.Lazy.Internal (defaultChunkSize)
import           Data.ByteVector               (fromByteVector)
import           Data.Conduit.VectorBuilder    (vectorBuilder)
import           System.IO                     (Handle)

run :: Handle -> Handle -> IO ()
run inH outH =
    (sourceHandle inH :: Producer IO S.ByteString)
        $$ vectorBuilder defaultChunkSize mapM_CE
        =$ mapC fromByteVector
        =$ sinkHandle outH

