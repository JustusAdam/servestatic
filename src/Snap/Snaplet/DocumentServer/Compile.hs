module Snap.Snaplet.DocumentServer.Compile
    ( compile
    , CompileResult(..)
    ) where


import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Default               (def)
import qualified Data.HashMap.Strict        as HM
import           Data.Text.Lazy.IO          as T
import           System.FilePath            (takeExtension)
import qualified Text.Blaze.Html5           as Html
import           Text.Pandoc                (Pandoc, readDocx, readLaTeX,
                                             readMarkdown, readOdt, writeHtml)


data CompileResult
  = Embeddable Html.Html
  | Servable LB.ByteString
  | Failed String


compilerNotImplemented :: Monad m => a -> MaybeT m Html.Html
compilerNotImplemented = const $ return $ Html.string "Compiler not implemented"


mkCompiler :: Show a => (LB.ByteString -> Either a Pandoc) -> String -> IO CompileResult
mkCompiler reader file = handle . reader <$> LB.readFile file
  where
    handle (Right val) = Embeddable $ writeHtml def val
    handle (Left  a  ) = Failed $ show a


compilers :: HM.HashMap String (FilePath -> IO CompileResult)
compilers = HM.fromList
  [ (".md", markdown)
  , (".markdown", markdown)
  , (".tex", latex)
  , (".docx", docx)
  , (".odt", odt)
  , (".html", html)
  ]
  where
    markdown = mkCompiler $ readMarkdown def . LB.unpack
    latex = mkCompiler $ readLaTeX def . LB.unpack
    docx = mkCompiler $ fmap fst . readDocx def
    odt = mkCompiler $ fmap fst . readOdt def
    html = (Servable <$>) . LB.readFile


compile :: FilePath -> IO CompileResult
compile path =
  case HM.lookup (takeExtension path) compilers of
    Nothing ->
      Embeddable . Html.lazyText <$> T.readFile path
    Just compiler -> compiler path
