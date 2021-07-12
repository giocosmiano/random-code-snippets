-----------------------------------------------------------------------------------
-- From `Haskell for Imperative Programmers`
-- https://www.youtube.com/watch?v=Vgu82wiiZ90&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV
-- https://www.youtube.com/watch?v=eAcNGbnuTYs&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV&index=33
-----------------------------------------------------------------------------------

-----------------------------------------------------------------------------------
-- Documentations
-----------------------------------------------------------------------------------
-- https://downloads.haskell.org/~ghc/7.10.1/docs/html/libraries/Control-DeepSeq.html

module Evaluate where

import Control.DeepSeq
import Control.Exception (evaluate)

-----------------------------------------------------------------------------------

something :: Int -> [Int]
something x = map (*2) [1..x]

-----------------------------------------------------------------------------------

somethingIO :: Int -> IO [Int]
somethingIO x = do
  return $!! map (*2) [1..x]

-----------------------------------------------------------------------------------

main :: IO ()
main = do
  let x = 1000000
  result1 <- evaluate $ force $ something x
  -- result1 is fully evaluated here
  result2 <- somethingIO x
  -- result2 is fully evaluated here. this is much cleaner than using `evaluate and force` above
  return ()


-----------------------------------------------------------------------------------
-- Compile
-----------------------------------------------------------------------------------
-- $ stack ghci
-- Prelude> :l Tree
--
-- OR compiling via stack --> https://docs.haskellstack.org/en/stable/GUIDE/#ghcrunghc
-- $ stack ghc Tree.hs
--
-- a = something 10
-- :sprint a
-- a = _
--
-- b = force a
-- :sprint a
-- a = _
-- :sprint b
-- b = _
--
-- result <- evaluate b
-- :sprint b
-- b = [2,4,6,8,10,12,14,16,18,20]
-- :sprint a
-- a = [2,4,6,8,10,12,14,16,18,20]
-- :sprint result
-- result = [2,4,6,8,10,12,14,16,18,20]
--
-- result2 <- somethingIO 15
-- :sprint result2
-- result2 = [2,4,6,8,10,12,14,16,18,20,22,24,26,28,30]


