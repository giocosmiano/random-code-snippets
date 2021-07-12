-----------------------------------------------------------------------------------
-- From `Haskell for Imperative Programmers`
-- https://www.youtube.com/watch?v=Vgu82wiiZ90&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV
-- https://www.youtube.com/watch?v=eAcNGbnuTYs&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV&index=33
-----------------------------------------------------------------------------------

-----------------------------------------------------------------------------------
-- Documentations
-----------------------------------------------------------------------------------
-- https://downloads.haskell.org/~ghc/7.10.1/docs/html/libraries/Control-DeepSeq.html

module Tree where

import Control.DeepSeq

-----------------------------------------------------------------------------------

data Tree a = Node (Tree a) a (Tree a) | Leaf deriving Show

insert :: (Ord a) => Tree a -> a -> Tree a
insert Leaf x = Node Leaf x Leaf
insert (Node l v r) x
  | x <= v    = Node (insert l x) v r
  | otherwise = Node l v (insert r x)

t = foldl insert Leaf [1..5]

-- Example from:
-- Chapter 2 of "Parallel and Concurrent Programming in Haskell"
-- by Simon marlow
instance NFData a => NFData (Tree a) where
  rnf Leaf = ()
  rnf (Node l v r) = rnf l `seq` rnf v `seq` rnf r

-----------------------------------------------------------------------------------
-- Compile
-----------------------------------------------------------------------------------
-- $ stack ghci
-- Prelude> :l Tree
--
-- OR compiling via stack --> https://docs.haskellstack.org/en/stable/GUIDE/#ghcrunghc
-- $ stack ghc Tree.hs
--
-- :sprint t
-- t = _
--
-- deepseq t ()
-- ()
--
-- :sprint t
-- t = Node Leaf 1 (Node Leaf 2 (Node Leaf 3 (Node Leaf 4 (Node Leaf 5 Leaf))))

