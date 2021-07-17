-----------------------------------------------------------------------------------
-- From `Haskell for Imperative Programmers`
-- https://www.youtube.com/watch?v=Vgu82wiiZ90&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV
-- https://www.youtube.com/watch?v=eAcNGbnuTYs&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV&index=24
-----------------------------------------------------------------------------------

-----------------------------------------------------------------------------------
-- Documentations
-----------------------------------------------------------------------------------
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/System-Environment.html
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/System-Exit.html

module Greeting where

import System.Exit
import System.Environment
import Data.Maybe

-----------------------------------------------------------------------------------

printHelp = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ "[ [-h | --help | -v | --version ] <greeting> "

-----------------------------------------------------------------------------------

printVersion = putStrLn "v1"

-----------------------------------------------------------------------------------

mainAct [] = do
  putStrLn "Needs a greeting!"
  printHelp
  exitFailure
mainAct args = do
  let greeting = unwords args
  name <- lookupEnv "USER"
  putStrLn $ maybe "No user to greet!" (\name -> greeting ++ " " ++ name) name

-----------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  if "-h" `elem` args || "--help" `elem` args then
    printHelp >> exitSuccess
  else if "-v" `elem` args || "--version" `elem` args then
    printVersion >> exitSuccess
  else mainAct args

-----------------------------------------------------------------------------------
-- Compile
-----------------------------------------------------------------------------------
-- $ stack ghci
-- Prelude> :l Greeting
--
-- For compiling options --> https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html
-- OR compiling via stack --> https://docs.haskellstack.org/en/stable/GUIDE/#ghcrunghc
-- $ stack ghc -- -main-is Greeting Greeting.hs
-- $ ./Greeting hello
-- hello gio
--
-- $ ./Greeting --help
-- hello gio
--
-- OR compile renaming the executable to `greet`
-- $ stack ghc -- -main-is Greeting Greeting.hs -o greet
-- $ ./greet hello
-- hello gio
--
--
-- GHC compiles to .o and .hi but no executable
-- https://stackoverflow.com/questions/62625481/ghc-compiles-to-o-and-hi-but-no-executable
--
--
--
