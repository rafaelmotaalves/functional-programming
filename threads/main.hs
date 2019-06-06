import Control.Concurrent
import Control.Monad
import System.IO

main = do
  m <- newEmptyMVar
  forkIO $ cronometer 100 m
  handleCountDown m

handleCountDown :: (MVar Int) -> IO ()
handleCountDown mvar = do
  r <- takeMVar mvar
  putStr ((show r) ++ "\n")
  when (r > 0) (handleCountDown mvar)

counter :: Int -> (MVar Int) -> IO ()
counter x mvar = do
    putMVar mvar (succ x)
    threadDelay 1000000
    counter (succ x) mvar

cronometer :: Int -> (MVar Int) -> IO ()
cronometer x mvar = do
  putMVar mvar (x)
  threadDelay 1000000
  when (x > 0) (cronometer (pred x) mvar)
