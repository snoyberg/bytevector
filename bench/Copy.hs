import           Conduit
import           Control.Monad                 (unless)
import           Control.Monad                 (when)
import           Control.Monad.Catch           (MonadMask)
import qualified Copy.Conduit
import qualified Copy.PeekPoke
import qualified Copy.Raw
import           Criterion.Main
import qualified Data.ByteString               as S
import           Data.ByteString.Lazy.Internal (defaultChunkSize)
import           Data.ByteVector               (fromByteVector)
import           System.IO                     (Handle, hClose)
import qualified System.IO                     as IO
import           System.IO.Temp                (withSystemTempFile)
import qualified System.Random.MWC             as MWC

runTest :: FilePath -- ^ source file
        -> String
        -> (Handle -> Handle -> IO ())
        -> Benchmark
runTest inFP name f = bench name $
    IO.withBinaryFile inFP IO.ReadMode $ \inH ->
    withSystemTempFile "run-test-out.bin" $ \outFP outH -> do
        f inH outH
        hClose inH
        hClose outH
        when testOutput $ do
            bs1 <- S.readFile inFP
            bs2 <- S.readFile outFP
            unless (bs1 == bs2) $ putStrLn "Buggy implementation!"

testOutput :: Bool
testOutput = False

main :: IO ()
main = withRandomFile $ \input -> defaultMain
    [ runTest input "Raw" Copy.Raw.run
    , runTest input "PeekPoke" Copy.PeekPoke.run
    , runTest input "conduit" Copy.Conduit.run
    ]

withRandomFile :: (MonadIO m, MonadMask m) => (FilePath -> m a) -> m a
withRandomFile f = withSystemTempFile "random-file.bin" $ \fp h -> do
    liftIO $ do
        gen <- MWC.createSystemRandom
        replicateMC
            (4000000 `div` defaultChunkSize)
            (MWC.uniformVector gen defaultChunkSize)
            $$ mapC fromByteVector
            =$ sinkHandle h
        hClose h
    f fp
