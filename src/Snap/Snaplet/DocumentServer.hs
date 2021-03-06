{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Snap.Snaplet.DocumentServer where


import           Control.Applicative
import           Control.Arrow                       ((&&&))
import           Control.Lens
import           Control.Monad                       (unless, void)
import           Control.Monad.IO.Class              (liftIO)
import qualified Data.ByteString.Char8               as B
import           Data.List.Extra                     (intersperse, splitOn)
import           Data.Monoid                         ((<>))
import qualified Data.Text                           as T
import           Data.Text.Encoding                  (encodeUtf8)
import           Snap
import           Snap.Snaplet.DocumentServer.Compile (CompileResult (..),
                                                      compile)
import           Snap.Snaplet.DocumentServer.Pages
import           System.Directory
import           System.FilePath
import           System.IO                           (stderr)
import           Text.Blaze.Html.Renderer.Utf8       (renderHtmlBuilder)
import qualified Text.Blaze.Html5                    as Html
import qualified Text.Blaze.Html5.Attributes         as Attr


writeError :: B.ByteString -> IO ()
writeError = B.hPutStrLn stderr


defaultID :: T.Text -> T.Text
defaultID = ("DocumentServer:" <>)


data DocumentServer = DocumentServer
  { _directory :: FilePath
  , _serveName :: T.Text
  }


makeLenses ''DocumentServer


initDocumentServer :: T.Text -> FilePath -> SnapletInit DocumentServer DocumentServer
initDocumentServer name fp =
  makeSnaplet (defaultID name) "Serves documents" Nothing $ do
    directoryExists <- liftIO $ doesDirectoryExist fp
    let dirnameBS = encodeUtf8 name
    if directoryExists
      then void $ addRoutes [("", documentHandler)]
      else void $ liftIO $
            writeError $ "Directory " <> dirnameBS <> " does not exist, handler not installed."

    unless (isAbsolute fp) $ liftIO $
      writeError "Running the document server on a relative path is not recommended!"

    return DocumentServer { _directory = fp, _serveName = name }


serveDocument :: Handler b DocumentServer ()
serveDocument = do
  requestedPath <- getDocServerPath
  absolutePath <- absolutize requestedPath
  isFile <- liftIO $ doesFileExist absolutePath
  if isFile
    then do
      compiled <- liftIO $ compile absolutePath
      breadc <- breadcrumbs
      let embed = respHtml . documentPage requestedPath breadc
      case compiled of
        Embeddable h -> embed h
        Servable b -> writeLBS b
        Failed err -> embed (Html.div $ Html.string err)

    else empty


serveDirectory :: Handler b DocumentServer ()
serveDirectory = do
  requestedPath <- getDocServerPath
  absolutePath <- absolutize requestedPath
  isDir <- liftIO $ doesDirectoryExist absolutePath
  if isDir
    then do
      let isRoot = requestedPath == "/" || requestedPath == ""
      contents <-
        filter (\p -> p /= "." && (not isRoot || p /= ".."))
        <$>
        liftIO (getDirectoryContents absolutePath)
      uri <- withRequest $ return . B.unpack . rqURI
      let
          navigate path' = case reverse uri of
            '/':_ -> uri <> path'
            _ -> uri <> ('/':path')
      breadc <- breadcrumbs
      respHtml $
        documentPage
          requestedPath
          breadc
          $ Html.div $
              makeLinkList $ map (navigate &&& id) contents
    else empty


duplicate :: a -> (a, a)
duplicate a = (a, a)


makeLinkList :: [(String, String)] -> Html.Html
makeLinkList = Html.ul . mapM_ (Html.li . makeLink)


joinWith :: Monoid m => m -> m -> m -> m
joinWith c a b = a <> c <> b


breadcrumbs :: Handler b DocumentServer Breadcrumbs
breadcrumbs = do
  requestedPath <- getDocServerPath
  basePath <- withRequest $ return . B.unpack . rqContextPath
  let segments = splitOn "/" requestedPath
      partialPaths = scanl (joinWith "/") "" segments
      absolutizedPaths = map (basePath </>) partialPaths
  return $ zip absolutizedPaths ("🏠":segments)


respHtml :: MonadSnap m => Html.Html -> m ()
respHtml = writeBuilder . renderHtmlBuilder


respBasicPage :: MonadSnap m => Html.Html -> Html.Html -> m ()
respBasicPage = (respHtml .) . basicPage


absolutize :: FilePath -> Handler b DocumentServer FilePath
absolutize p = do
  s <- getSnapletState
  return $ s ^. snapletValue . directory </> p


getDocServerPath :: MonadSnap m => m String
getDocServerPath = withRequest $ return . B.unpack . rqPathInfo


documentNotFound :: Handler DocumentServer DocumentServer ()
documentNotFound = do
  name <- getDocServerPath
  respHtml $ Html.string $ "File " <> name <> "not found."


documentHandler :: Handler DocumentServer DocumentServer ()
documentHandler = serveDocument <|> serveDirectory <|> documentNotFound
