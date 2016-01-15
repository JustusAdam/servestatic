{-# LANGUAGE OverloadedStrings #-}
module Main where

import Snap.Snaplet.DocumentServer
import Snap
import System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  serveSnaplet defaultConfig (initDocumentServer "" (head args))
