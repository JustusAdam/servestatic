{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Snap
import           Snap.Snaplet.DocumentServer
import           System.Environment          (getArgs)


main :: IO ()
main = do
  args <- getArgs
  let dir = case args of
              [] -> "."
              [directory] -> directory
              _ -> error "Incorrect number of arguments provided (expected 1)"
  serveSnaplet defaultConfig $ initDocumentServer "" dir
