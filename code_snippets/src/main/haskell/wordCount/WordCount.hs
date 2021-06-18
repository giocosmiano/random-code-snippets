{-# LANGUAGE InstanceSigs #-}

module WordCount where

import Data.Char
import Data.List

-----------------------------------------------------------------------------------

countWords :: String -> [(String,Int)]
countWords = map (\w -> (head w, length w)) . group . sort . words . filter (\x -> isAlphaNum x || isSpace x) . map toLower

-----------------------------------------------------------------------------------

main = do
  let text = "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."
  print $ "input     --> " ++ (show $ text)
  print $ "wordCount --> " ++ (show $ countWords text)
