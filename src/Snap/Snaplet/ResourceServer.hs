{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Snap.Snaplet.ResourceServer where


import           Control.Arrow          (second)
import           Control.Lens
import           Control.Lens.TH
import           Control.Monad          (filterM, unless, void)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as B
import           Data.Maybe             (catMaybes)
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           Data.Traversable       (for)
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
initDocumentServer paths =
  makeSnaplet "assets" "Serves static files like css and javascript." Nothing $ do
    installable <- flip filterM paths $ \(mount, path) -> do
      exists <- liftIO $ doesDirectoryExist path
      unless exists $ liftIO $ printError $ "Directory " <> path <> " does not exist. Handler not installed."
      return exists

    addRoutes $ map (second fileHandler) installable
    return ResourceConf { _paths = installable }


fileHandler :: FilePath -> Handler v b ()
fileHandler path = return ()
