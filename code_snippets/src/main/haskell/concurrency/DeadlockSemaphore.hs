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
-- $ ghc -threaded DeadlockSemaphore.hs
--
-- OR compiling via stack --> https://docs.haskellstack.org/en/stable/GUIDE/#ghcrunghc
-- $ stack ghc -- -threaded DeadlockSemaphore.hs

-----------------------------------------------------------------------------------
-- Running the code `DeadlockSemaphore`
-----------------------------------------------------------------------------------
-- $ ./DeadlockSemaphore +RTS -N
-- DeadlockSemaphore: thread blocked indefinitely in an MVar operation

-----------------------------------------------------------------------------------

import System.IO
import Control.Concurrent

-----------------------------------------------------------------------------------

threadFunc :: QSem -> IO ()
threadFunc sem = do
  waitQSem sem

-----------------------------------------------------------------------------------

main :: IO ()
main = do
  -- Init mutex and FIFO for end flags
  sem <- newQSem 0
  forkIO $ threadFunc sem
  -- Read n end flags (blocks until all threads have sent their end signal)
  waitQSem sem

-----------------------------------------------------------------------------------
