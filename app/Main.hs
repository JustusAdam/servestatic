{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Snap
import           Snap.Snaplet.DocumentServer
import           System.Environment          (getArgs)


main :: IO ()
main = do
  args <- getArgs
  serveSnaplet defaultConfig (initDocumentServer "" (head args))
