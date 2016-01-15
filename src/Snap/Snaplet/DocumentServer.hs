{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Snap.Snaplet.DocumentServer where


import           Control.Applicative
import           Control.Arrow                 ((&&&))
import           Control.Lens
import           Control.Lens.TH
import           Control.Monad                 (unless, void, when)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans.Class     (lift)
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Char8    as LB
import           Data.Default                  (Default, def)
import qualified Data.HashMap.Strict           as HM
import           Data.List.Extra               (intersperse, splitOn)
import           Data.Maybe
import           Data.Maybe                    (fromMaybe)
import           Data.Monoid                   ((<>))
import qualified Data.Text                     as T
import           Data.Text.Encoding            (encodeUtf8)
import           Snap
import           Snap.Snaplet.Heist
import           System.Directory
import           System.FilePath
import           System.IO                     (hPutStrLn, stderr)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import qualified Text.Blaze.Html5              as Html
import qualified Text.Blaze.Html5.Attributes   as Attr
import           Text.Pandoc                   (Pandoc, readDocx, readLaTeX,
                                                readMarkdown, readOdt,
                                                writeHtml)


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


mkCompiler :: (LB.ByteString -> Either a Pandoc) -> String -> MaybeT IO Html.Html
mkCompiler reader file = do
  contents <- lift $ LB.readFile file
  case reader contents of
    Right val -> return $ writeHtml def val
    Left _ -> empty


compilers :: HM.HashMap String (FilePath -> MaybeT IO Html.Html)
compilers = HM.fromList
  [ (".md", markdown)
  , (".markdown", markdown)
  , (".tex", latex)
  , (".docx", docx)
  , (".odt", odt)
  ]
  where
    markdown = mkCompiler $ readMarkdown def . LB.unpack
    latex = mkCompiler $ readLaTeX def . LB.unpack
    docx = mkCompiler $ fmap fst . readDocx def
    odt = mkCompiler $ fmap fst . readOdt def


compile :: FilePath -> IO (Maybe Html.Html)
compile path = runMaybeT $ do
  compiler <- MaybeT $ return $ HM.lookup (takeExtension path) compilers
  compiler path


initDocumentServer :: T.Text -> FilePath -> SnapletInit DocumentServer DocumentServer
initDocumentServer name fp =
  makeSnaplet (defaultID name) "Serves documents" Nothing $ do
    directoryExists <- liftIO $ doesDirectoryExist fp
    let dirnameBS = encodeUtf8 name
    if directoryExists
      then void $ addRoutes [("", documentHandler)]
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
      breadc <- breadcrumbs
      respHtml $
        htmlPage
          (Html.title $ Html.string path)
          $ Html.div $ do
              Html.div breadc
              Html.div $
                makeLinkList $ map (navigate &&& id) contents
    else empty


duplicate a = (a, a)


makeLinkList =
  Html.ul .
    mapM_ (\(target, thing) ->
      Html.li $ do
        Html.a Html.! Attr.href (Html.toValue target) $
          Html.string thing)


joinWith c a b = a <> c <> b


breadcrumbs :: Handler b DocumentServer Html.Html
breadcrumbs = do
  path <- getDocServerPath
  basePath <- withRequest $ return . B.unpack . rqContextPath
  let segments = splitOn "/" path
      partialPaths = scanl (joinWith "/") "" segments
      absolutizedPaths = map (basePath </>) partialPaths
      makeLink (target, value) = Html.a Html.! Attr.href (Html.toValue target) $ Html.string value
  return $ sequence_ $ intersperse (Html.span $ Html.string "/") $ map makeLink $ zip absolutizedPaths ("🏠":segments)


respHtml :: MonadSnap m => Html.Html -> m ()
respHtml = writeBuilder . renderHtmlBuilder


respBasicPage :: MonadSnap m => Html.Html -> Html.Html -> m ()
respBasicPage = (respHtml .) . basicPage


htmlPage :: Html.Html -> Html.Html -> Html.Html
htmlPage head body = do
  Html.docType
  Html.html $ do
    Html.head $ do
      Html.meta Html.! Attr.charset "utf-8"
      head
    Html.body body


basicPage :: Html.Html -> Html.Html -> Html.Html
basicPage title content = htmlPage (Html.title title) (Html.div content)


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
