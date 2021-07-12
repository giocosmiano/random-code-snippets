-----------------------------------------------------------------------------------
-- From `Haskell for Imperative Programmers`
-- https://www.youtube.com/watch?v=Vgu82wiiZ90&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV
-- https://www.youtube.com/watch?v=x3GwVccWcqs&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV&index=29
-----------------------------------------------------------------------------------

-----------------------------------------------------------------------------------
-- Documentations
-----------------------------------------------------------------------------------
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent-QSem.html
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent-QSemN.html

-----------------------------------------------------------------------------------
-- Further Reading
-----------------------------------------------------------------------------------
-- https://wiki.haskell.org/Concurrency
-- https://en.wikipedia.org/wiki/Concurrent_Haskell
-- https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/mark.pdf
-- https://wiki.haskell.org/Parallelism_vs._Concurrency

-----------------------------------------------------------------------------------
-- Mutexes and Locks
-----------------------------------------------------------------------------------
-- https://en.wikipedia.org/wiki/Lock_(computer_science)
-- https://en.wikipedia.org/wiki/Mutual_exclusion

-----------------------------------------------------------------------------------
-- Compile with -threaded
-----------------------------------------------------------------------------------
-- $ ghc -threaded Semaphores.hs
--
-- OR compiling via stack --> https://docs.haskellstack.org/en/stable/GUIDE/#ghcrunghc
-- $ stack ghc -- -threaded Semaphores.hs

-----------------------------------------------------------------------------------
-- Running the code `Semaphores`
-----------------------------------------------------------------------------------
-- $ ./Semaphores +RTS -N

-----------------------------------------------------------------------------------

import System.IO
import Control.Concurrent

-----------------------------------------------------------------------------------

getGreeting :: IO String
getGreeting = do
  -- Get id and convert to string
  tid <- myThreadId
  let greeting = "Hello from " ++ show tid
  -- Force evaluation of greeting and return
  return $! greeting

-----------------------------------------------------------------------------------

threadHello :: QSem -> QSemN -> IO ()
threadHello mutex endFlags = do
  -- Compute greeting (finished before getting mutex)
  greeting <- getGreeting
  -- Get mutex (acquires lock for output)
  waitQSem mutex
  -- Say hello
  putStrLn greeting
  -- Release mutex (give up lock, another thread can take over)
  signalQSem mutex
  -- Signal end of thread
  signalQSemN endFlags 1

-----------------------------------------------------------------------------------

main :: IO ()
main = do
  -- Disable buffering on stdout
  hSetBuffering stdout NoBuffering
  -- Number of threads to spawn
  let n = 10
  -- Init mutex and FIFO for end flags
  mutex <- newQSem 0
  endFlags <- newQSemN 0
  -- Spawn threads (threads are waiting for mutex before printing)
  mapM_ (const $ forkIO $ threadHello mutex endFlags) [1..n]
  -- Give mutex its value (threads start acquiring mutex here)
  signalQSem mutex
  -- Read n end flags (blocks until all threads have sent their end signal)
  waitQSemN endFlags n

-----------------------------------------------------------------------------------
