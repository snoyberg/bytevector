module Util where

import qualified System.Random.MWC as MWC
import Data.ByteVector (fromByteVector)
import System.IO.Temp (withSystemTempFile)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import System.IO (hClose)
import Conduit
import Control.Monad.Catch (MonadMask)
import Data.Time (getCurrentTime, diffUTCTime)

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

timed f = do
    start <- liftIO getCurrentTime
    putStrLn $ "Start: " ++ show start
    f
    end <- liftIO getCurrentTime
    putStrLn $ "End  : " ++ show end
    print $ diffUTCTime end start