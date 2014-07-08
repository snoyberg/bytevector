module Copy where

--import Criterion.Main
import           Conduit
import           Control.Monad                 (unless)
import           Control.Monad.Catch           (MonadMask)
import qualified Copy.Conduit
import qualified Copy.Raw
import qualified Data.ByteString               as S
import           Data.ByteString.Lazy.Internal (defaultChunkSize)
import           Data.ByteVector               (fromByteVector)
import           Data.Time                     (diffUTCTime, getCurrentTime)
import           System.IO                     (Handle, hClose)
import qualified System.IO                     as IO
import           System.IO.Temp                (withSystemTempFile)
import qualified System.Random.MWC             as MWC

runTest :: FilePath -- ^ source file
        -> String
        -> (Handle -> Handle -> IO ())
        -> IO ()
runTest inFP name f =
    IO.withBinaryFile inFP IO.ReadMode $ \inH ->
    withSystemTempFile "run-test-out.bin" $ \outFP outH -> do
        putStrLn name
        timed $ f inH outH
        hClose inH
        hClose outH
        bs1 <- S.readFile inFP
        bs2 <- S.readFile outFP
        unless (bs1 == bs2) $ putStrLn "Buggy implementation!"

main :: IO ()
main = withRandomFile $ \input -> do
    runTest input "Raw" Copy.Raw.run
    runTest input "conduit" Copy.Conduit.run

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

timed :: MonadIO m => m a -> m a
timed f = do
    start <- liftIO $ do
        start <- getCurrentTime
        putStrLn $ "Start: " ++ show start
        return start
    res <- f
    liftIO $ do
        end <- getCurrentTime
        putStrLn $ "End  : " ++ show end
        print $ diffUTCTime end start
    return res
