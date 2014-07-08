{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
module Main where

import           Conduit
import           Control.Monad                 (liftM)
import           Control.Monad.Primitive       (PrimMonad, PrimState)
import qualified Data.ByteString               as S
import           Data.ByteString.Lazy.Internal (defaultChunkSize)
import           Data.ByteVector               (fromByteVector)
import           Data.Conduit.Internal         (ConduitM (..), Pipe (..))
import           Data.Primitive.MutVar         (MutVar, newMutVar, readMutVar,
                                                writeMutVar)
import           Data.Vector.Generic           (Mutable, Vector, unsafeFreeze)
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable   as V
import qualified System.IO                     as IO
import           System.IO.Temp                (withSystemTempFile)
import           Util

main :: IO ()
main =
    withRandomFile $ \inFP ->
    withSystemTempFile "output.bin" $ \_ outH ->
    IO.withBinaryFile inFP IO.ReadMode $ \inH -> timed $
        ((sourceHandle inH :: Producer IO S.ByteString)
            $$ vectorBuilder defaultChunkSize mapM_CE
            =$ mapC fromByteVector
            =$ sinkHandle outH :: IO ())

vectorBuilder :: (PrimMonad m, Vector v e)
              => Int -- ^ size
              -> ((e -> m ()) -> ConduitM i (v e) m r)
              -> ConduitM i (v e) m r
vectorBuilder size inner = do
    ref <- lift $ do
        mv <- V.new size
        newMutVar $! S 0 mv id
    res <- onAwait (yieldS ref) (inner (addE ref))
    vs <- lift $ do
        S idx mv front <- readMutVar ref
        end <-
            if idx == 0
                then return []
                else do
                    v <- unsafeFreeze mv
                    return [Data.Vector.Generic.unsafeTake idx v]
        return $ front end
    yieldMany vs
    return res

data S s v e = S
    {-# UNPACK #-} !Int -- ^ index
    {-# UNPACK #-} !(Mutable v s e)
    ([v e] -> [v e])

onAwait :: Monad m => ConduitM i o m () -> ConduitM i o m r -> ConduitM i o m r
onAwait (ConduitM callback) =
    ConduitM . go . unConduitM
  where
    go (Done r) = Done r
    go (HaveOutput f g o) = HaveOutput (go f) g o
    go (NeedInput f g) = callback >> NeedInput (go . f) (go . g)
    go (PipeM mp) = PipeM (liftM go mp)
    go (Leftover f i) = Leftover (go f) i

yieldS :: PrimMonad m => MutVar (PrimState m) (S (PrimState m) v e) -> Producer m (v e)
yieldS ref = do
    S idx mv front <- lift $ readMutVar ref
    yieldMany (front [])
    lift $ writeMutVar ref $! S idx mv id

addE :: (PrimMonad m, Vector v e)
     => MutVar (PrimState m) (S (PrimState m) v e)
     -> e
     -> m ()
addE ref e = do
    S idx mv front <- readMutVar ref
    V.write mv idx e
    let idx' = succ idx
        size = V.length mv
    if idx' >= size
        then do
            v <- unsafeFreeze mv
            let front' = front . (v:)
            mv' <- V.new size
            writeMutVar ref $! S 0 mv' front'
        else writeMutVar ref $! S idx' mv front
