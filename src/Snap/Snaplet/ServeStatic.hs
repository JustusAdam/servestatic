{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Snap.Snaplet.ServeStatic where


import           Control.Lens
import           Control.Lens.TH
import           Control.Monad          (unless, void, when)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as B
import           Data.Maybe
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           Data.Text.Encoding     (encodeUtf8)
import           Snap
import           Snap.Snaplet.Heist
import           System.Directory
import           System.FilePath
import           System.IO              (stderr, hPutStrLn)
import Control.Applicative
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)
import qualified Text.Blaze.Html5 as Html
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Data.Default (def)
import Text.Pandoc
import qualified Text.Blaze.Html5.Attributes as Attr


writeError :: B.ByteString -> IO ()
writeError = B.hPutStrLn stderr


defaultID :: T.Text -> T.Text
defaultID = ("DocumentServer:" <>)

data DocumentServer = DocumentServer
  { _directory :: FilePath
  , _serveName :: T.Text
  }


makeLenses ''DocumentServer


compilerNotImplemented :: Monad m => a -> MaybeT m Html.Html
compilerNotImplemented = const $ return $ Html.string "Compiler not implemented"


mkCompiler :: (ReaderOptions -> String -> Either a Pandoc) -> String -> MaybeT IO Html.Html
mkCompiler reader file = do
  contents <- lift $ readFile file
  case reader def contents of
    Right val -> return $ writeHtml def val
    Left _ -> empty


compilers :: HM.HashMap String (FilePath -> MaybeT IO Html.Html)
compilers = HM.fromList
  [ (".md", mkCompiler readMarkdown)
  , (".markdown", mkCompiler readMarkdown)
  ]


compile :: FilePath -> IO (Maybe Html.Html)
compile path = runMaybeT $ do
  compiler <- MaybeT $ return $ HM.lookup (takeExtension path) compilers
  compiler path


initDocumentServer :: T.Text -> FilePath -> SnapletInit DocumentServer DocumentServer
initDocumentServer name fp =
  makeSnaplet (defaultID name) "Serves static files" Nothing $ do
    directoryExists <- liftIO $ doesDirectoryExist fp
    let dirnameBS = encodeUtf8 name
    if directoryExists
      then void $ addRoutes [(dirnameBS, documentHandler)]
      else void $ liftIO $ writeError $ "Directory " <> dirnameBS <> " does not exist, handler not installed."

    unless (isAbsolute fp) $ liftIO $
      writeError "Running the document server on a relative path is not recommended!"

    return DocumentServer { _directory = fp, _serveName = name }


serveDocument :: Handler b DocumentServer ()
serveDocument = do
  path <- getDocServerPath
  absolutePath <- absolutize path
  isFile <- liftIO $ doesFileExist absolutePath
  if isFile
    then do
      compiled <- liftIO $ compile absolutePath
      respBasicPage (Html.string path) $ fromMaybe (Html.string "Compilation failed") compiled
    else empty


serveDirectory :: Handler b DocumentServer ()
serveDirectory = do
  path <- getDocServerPath
  absolutePath <- absolutize path
  isDir <- liftIO $ doesDirectoryExist absolutePath
  if isDir
    then do
      contents <- liftIO $ getDirectoryContents absolutePath
      uri <- withRequest $ return . B.unpack . rqURI
      let
          navigate path' = case reverse uri of
            '/':_ -> uri <> path'
            _ -> uri <> ('/':path')
          makeEntry thing =
            Html.li $ do
              Html.a Html.! Attr.href (Html.toValue $ navigate thing) $
                Html.string thing

      respBasicPage (Html.string path) $ Html.ul $ mapM_ makeEntry contents
    else empty


respHtml :: MonadSnap m => Html.Html -> m ()
respHtml = writeBuilder . renderHtmlBuilder


respBasicPage :: MonadSnap m => Html.Html -> Html.Html -> m ()
respBasicPage = (respHtml .) . basicPage


basicPage :: Html.Html -> Html.Html -> Html.Html
basicPage title content = do
  Html.docType
  Html.html $ do
    Html.head $ do
      Html.title title
      Html.meta Html.! Attr.charset "utf-8"
    Html.body $ do
      Html.h1 title
      Html.div content


absolutize :: FilePath -> Handler b DocumentServer FilePath
absolutize p = do
  s <- getSnapletState
  return $ s ^. snapletValue . directory </> p


getDocServerPath :: MonadSnap m => m String
getDocServerPath = withRequest $ return . B.unpack . rqPathInfo


documentNotFound = do
  name <- getDocServerPath
  respHtml $ Html.string $ "File " <> name <> "not found."


documentHandler = serveDocument <|> serveDirectory <|> documentNotFound
