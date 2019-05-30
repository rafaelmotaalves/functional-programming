import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
 
type Repository = TVar Int
type BreadRepository = TVar Int
type MeatRepository = TVar Int
type TomatoRepository = TVar Int
type Knife = TVar Bool
 
main = do
    knife <- atomically (newTVar True)
    bread <- atomically (newTVar 30)
    meat <- atomically (newTVar 30)
    tomato <- atomically (newTVar 30)
    forkIO (foodWorker knife bread meat tomato)
    forkIO (foodWorker knife bread meat tomato)
    forever $ do
        atomically (supplyIngredients bread meat tomato)
        putStr "Ingredients supplied\n"
        threadDelay (10000000)
        
supplyIngredients :: BreadRepository -> MeatRepository -> TomatoRepository -> STM ()
         
supplyIngredients br mr tr = do
    (writeTVar br 30)
    (writeTVar mr 30)
    (writeTVar tr 30)

foodWorker :: Knife -> BreadRepository -> MeatRepository -> TomatoRepository -> IO ()
foodWorker kn br mr tr = forever $ do
    (prepareFood kn br mr tr)
    putStr ("Hamburger prepared\n")
    threadDelay (1000000)

prepareFood :: Knife -> BreadRepository -> MeatRepository -> TomatoRepository -> IO ()
prepareFood kn br mr tr = do
    putStr "trying to take knife\n"
    atomically (takeKnife kn)
    putStr "Took knife\n"
    atomically (takeIngredient br)
    atomically (takeIngredient mr)
    atomically (takeIngredient tr)
    atomically (returnKnife kn)
    putStr "Returned knife\n"

takeKnife :: Knife -> STM ()
takeKnife kn = do
    isAvailable <- (readTVar kn)
    if (isAvailable)
    then (writeTVar kn False)
    else retry

returnKnife :: Knife -> STM ()
returnKnife kn = do (writeTVar kn True)
    
takeIngredient :: Repository -> STM ()
takeIngredient rep = do
    ingredient <- readTVar rep
    if (hasIngredient ingredient)
    then (writeTVar rep (ingredient - 1))
    else retry

hasIngredient :: Int -> Bool
hasIngredient q = q > 0