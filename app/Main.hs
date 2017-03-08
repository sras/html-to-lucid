{-# LANGUAGE TemplateHaskell #-}

module Main where

import Lib

main :: IO ()
main = do
  src <- readFile "src.html"
  putStrLn $ convertToLucid $ makeDocFromHtml src

