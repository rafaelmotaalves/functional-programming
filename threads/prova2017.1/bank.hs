import Control.Concurrent.STM

type Conta = TVar Int

main = do
    conta <- atomically(newTVar 0)
    atomically (deposito conta 10)
    atomically (saque2 conta 5)
    atomically (saque2 conta 10)
    saldo <- atomically(readTVar conta)

    (putStr (show saldo))

saque :: Conta -> Int -> STM ()
saque conta valor = do
    saldo <- readTVar conta
    writeTVar conta (saldo - valor)
    
deposito :: Conta -> Int -> STM ()
deposito conta valor = (saque conta (-valor))

saque2 :: Conta -> Int -> STM (Bool)
saque2 conta valor = do
    saldo <- readTVar conta
    if (saldo - valor < 0)
    then return (False)
    else do
        writeTVar conta (saldo - valor)
        return (True)

saque3 :: Conta -> Conta -> Int -> STM (Bool)
saque3 contaA contaB valor = do
    success <- (saque2 contaA valor)
    if (success)
    then return(True)
    else (saque2 contaB valor)