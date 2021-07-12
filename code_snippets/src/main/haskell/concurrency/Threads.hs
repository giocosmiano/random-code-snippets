-----------------------------------------------------------------------------------
-- From `Haskell for Imperative Programmers`
-- https://www.youtube.com/watch?v=Vgu82wiiZ90&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV
-- https://www.youtube.com/watch?v=cuHD2qTXxL4&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV&index=28
-----------------------------------------------------------------------------------

-----------------------------------------------------------------------------------
-- Documentations
-----------------------------------------------------------------------------------
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent.html
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent-MVar.html
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent-Chan.html
-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-concurrent.html

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
-- $ ghc -threaded Threads.hs
--
-- OR compiling via stack --> https://docs.haskellstack.org/en/stable/GUIDE/#ghcrunghc
-- $ stack ghc -- -threaded Threads.hs

-----------------------------------------------------------------------------------
-- Running the code `Threads`
-----------------------------------------------------------------------------------
-- $ ./Threads +RTS -N

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

threadHello :: MVar () -> Chan () -> IO ()
threadHello mutex endFlags = do
  -- Compute greeting (finished before getting mutex)
  greeting <- getGreeting
  -- Get mutex (acquires lock for output)
  takeMVar mutex
  -- Say hello
  putStrLn greeting
  -- Release mutex (give up lock, another thread can take over)
  putMVar mutex ()
  -- Signal end of thread
  writeChan endFlags ()

-----------------------------------------------------------------------------------

main :: IO ()
main = do
  -- Disable buffering on stdout
  hSetBuffering stdout NoBuffering
  -- Number of threads to spawn
  let n = 10
  -- Init mutex and FIFO for end flags
  mutex <- newEmptyMVar
  endFlags <- newChan
  -- Spawn threads (threads are waiting for mutex before printing)
  mapM_ (const $ forkIO $ threadHello mutex endFlags) [1..n]
  -- Give mutex its value (threads start acquiring mutex here)
  putMVar mutex ()
  -- Read n end flags (blocks until all threads have sent their end signal)
  mapM_ (const $ readChan endFlags) [1..n]

-----------------------------------------------------------------------------------
