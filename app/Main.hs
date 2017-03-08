{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.IO
import Lib

main :: IO ()
main = hGetContents stdin >>= putStr.makeTemplate
