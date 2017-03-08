{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.IO
import Lib

main :: IO ()
main = do
  src <- hGetContents stdin
  putStrLn $ convertToLucid $ makeDocFromHtml src

