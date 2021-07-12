-----------------------------------------------------------------------------------
-- From `Haskell for Imperative Programmers`
-- https://www.youtube.com/watch?v=Vgu82wiiZ90&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV
-- https://www.youtube.com/watch?v=eAcNGbnuTYs&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV&index=33
-----------------------------------------------------------------------------------

-----------------------------------------------------------------------------------
-- Documentations
-----------------------------------------------------------------------------------
-- https://downloads.haskell.org/~ghc/7.10.1/docs/html/libraries/Control-DeepSeq.html

module Peano where

import Control.DeepSeq

-----------------------------------------------------------------------------------

data Peano = Succ Peano | Zero deriving Show

add :: Peano -> Peano -> Peano
add Zero a = a
add (Succ a) b = add a (Succ b)

five = Succ $ Succ $ Succ $ Succ $ Succ Zero

instance NFData Peano where
  rnf Zero = ()
  rnf (Succ a) = rnf a

-----------------------------------------------------------------------------------
-- Compile
-----------------------------------------------------------------------------------
-- $ stack ghci
-- Prelude> :l Peano
--
-- OR compiling via stack --> https://docs.haskellstack.org/en/stable/GUIDE/#ghcrunghc
-- $ stack ghc Peano.hs
--
-- :sprint five
-- five = _
--
-- deepseq five ()
-- ()
--
-- :sprint five
-- five = Succ (Succ (Succ (Succ (Succ Zero))))

