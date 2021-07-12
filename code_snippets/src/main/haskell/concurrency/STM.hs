-----------------------------------------------------------------------------------
-- From `Haskell for Imperative Programmers`
-- https://www.youtube.com/watch?v=Vgu82wiiZ90&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV
-- https://www.youtube.com/watch?v=2lll2VbX8Vc&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV&index=31
-----------------------------------------------------------------------------------

-----------------------------------------------------------------------------------
-- Documentations
-----------------------------------------------------------------------------------
-- https://hackage.haskell.org/package/stm-2.5.0.0/docs/Control-Concurrent-STM.html

-----------------------------------------------------------------------------------
-- Further Reading
-----------------------------------------------------------------------------------
-- Composable Memory Transactions
--    https://www.microsoft.com/en-us/research/wp-content/uploads/2005/01/2005-ppopp-composable.pdf?from=http%3A%2F%2Fresearch.microsoft.com%2Fen-us%2Fum%2Fpeople%2Fsimonpj%2Fpapers%2Fstm%2Fstm.pdf

-----------------------------------------------------------------------------------
-- Compile with -threaded
-----------------------------------------------------------------------------------
-- $ ghc -threaded STM.hs
--
-- OR compiling via stack --> https://docs.haskellstack.org/en/stable/GUIDE/#ghcrunghc
-- $ stack ghc -- -threaded STM.hs

-----------------------------------------------------------------------------------
-- Running the code `STM`
-----------------------------------------------------------------------------------
-- $ ./STM +RTS -N

-----------------------------------------------------------------------------------

import System.IO
import Control.Concurrent
import Control.Concurrent.STM

-- (<Sum>, <Number of finished transactions>)
type Result = TVar (Int, Int)

-----------------------------------------------------------------------------------

-- Adds x to result and increments the number of finished transactions
addToResult :: Result -> Int -> STM ()
addToResult result x = do
  (sum, endCtr) <- readTVar result
  writeTVar result (sum+x, endCtr+1)

-----------------------------------------------------------------------------------

-- Waits for the number of finished transactions to reach a limit
-- Then returns the sum of the result
waitForCounter :: Result -> Int -> STM Int
waitForCounter result limit = do
  (sum, endCtr) <- readTVar result
  if endCtr < limit then retry else return sum

-----------------------------------------------------------------------------------

main :: IO ()
main = do
  -- Number of threads to spawn
  let n = 100
  -- Set up TVar
  result <- atomically $ newTVar (0, 0)
  -- Spawn threads
  mapM_ (\x -> forkIO $ atomically $ addToResult result x) [1..n]
  -- Wait for threads to finish and get sum
  sum <- atomically $ waitForCounter result n
  -- Print sum
  putStrLn $ "Sum [1..n] = " ++ show sum

-----------------------------------------------------------------------------------
