module Snap.Snaplet.DocumentServer.Pages where


import           Control.Category            ((>>>))
import           Data.List                   (intersperse)
import           Prelude                     hiding (div, head, span)
import           Text.Blaze.Html5            as Html hiding (map)
import           Text.Blaze.Html5.Attributes as Attr hiding (span)


type Breadcrumbs = [(String, String)]


htmlPage :: Html -> Html -> Html
htmlPage head_ body_ = do
  docType
  html $ do
    head $ do
      meta ! charset (toValue "utf-8")
      head_
    body body_


basicPage :: Html -> Html -> Html
basicPage title_ content =
  htmlPage
    (Html.title title_)
    $ div content


documentPage :: String -> Breadcrumbs -> Html -> Html
documentPage title breadcrumbs content =
  basicPage
    (string title)
    $ div $ do
        div ! class_ (toValue "top-bar") $ do
          nav $ renderBreadcrumbs breadcrumbs
        div ! class_ (toValue "document-container") $ content


makeLink :: ToValue a => (a, String) -> Html
makeLink (target, name) =
  a ! href (toValue target) $
    string name


renderBreadcrumbs :: Breadcrumbs -> Html
renderBreadcrumbs =
  map makeLink
  >>> intersperse (span $ string "/")
  >>> sequence_
