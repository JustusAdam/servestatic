module Snap.Snaplet.DocumentServer.Pages where


import           Control.Category            ((>>>))
import           Prelude                     hiding (div, head, span)
import           Text.Blaze.Html5            as Html
import           Text.Blaze.Html5.Attributes as Attr


type Breadcrumbs = [(String, String)]


htmlPage :: Html -> Html -> Html
htmlPage head_ body = do
  docType
  html $ do
    head $ do
      meta ! charset "utf-8"
      head_
    body body


basicPage :: Html -> Html -> Html
basicPage title content =
  htmlPage
    (title title)
    $ div content


documentPage :: String -> Breadcrumbs -> Html -> Html
documentPage title breadcrumbs content =
  basicPage
    (string title)
    $ div $ do
        div ! class_ "top-bar" $ do
          nav $ renderBreadcrumbs breadcrumbs
        div ! class_ "document-container"


makeLink :: ToValue a => (a, String) -> Html
makeLink (target, name) =
  a ! href (toValue target) $
    string name


renderBreadcrumbs :: Breadcrumbs -> Html
renderBreadcrumbs =
  map makeLink
  >>> intersperse (span $ string "/")
  >>> sequence_
