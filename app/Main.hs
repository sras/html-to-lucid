{-# LANGUAGE TemplateHaskell #-}

module Main where

import Lib
import Lucid

main :: IO ()
main = convert "text"
