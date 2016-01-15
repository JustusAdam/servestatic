{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Snap.Snaplet.ResourceServer where


import           Control.Arrow          (second)
import           Control.Lens
import           Control.Monad          (filterM, unless)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as B
import           Data.Monoid            ((<>))
import           Snap
import           System.Directory
import           System.FilePath
import           System.IO              (hPutStrLn, stderr)


data ResourceConf = ResourceConf
  { _paths :: [(B.ByteString, FilePath)]
  }


makeLenses ''ResourceConf


printError :: String -> IO ()
printError = hPutStrLn stderr


initDocumentServer :: [(B.ByteString, FilePath)] -> SnapletInit ResourceConf ResourceConf
initDocumentServer servePaths =
  makeSnaplet "assets" "Serves static files like css and javascript." Nothing $ do
    installable <- flip filterM servePaths $ \(_, systemPath) -> do
      exists <- liftIO $ doesDirectoryExist systemPath
      unless exists $ liftIO $ printError $ "Directory " <> systemPath <> " does not exist. Handler not installed."
      return exists

    addRoutes $ map (second fileHandler) installable
    return ResourceConf { _paths = installable }


fileHandler :: FilePath -> Handler v b ()
fileHandler path = return ()
