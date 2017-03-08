{-# LANGUAGE TemplateHaskell #-}

module Main where

import Lib
import Lucid

convertToLucid $ makeDocFromHtml "<html><body><table><tr><td>Columns1</td><td>Columns2</td></tr></table></body></html>"

main :: IO ()
main = convert "text"
