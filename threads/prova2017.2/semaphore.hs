import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent
import Control.Monad
import System.IO

type Semaphore = TVar Bool

p :: Semaphore -> STM ()

p sem = do
    value <- readTVar sem
    if (value) 
    then writeTVar sem False
    else (p sem)

v :: Semaphore -> STM ()
v sem = do writeTVar sem True
        

main = do 
    sem <- atomically (newTVar True)
    atomically (p sem)
    value <- atomically (readTVar sem)
    putStr (show value)
    