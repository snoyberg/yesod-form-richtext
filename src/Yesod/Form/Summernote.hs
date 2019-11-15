-- | Provide the user with the Summernote rich text editor.
--
-- /NOTES:/
--
-- Editor fields have hidden textareas which are updated automatically when
-- editor contents changes.
--
-- @
-- summerForm :: Form HtmlComment
-- summerForm = renderBootstrap3 BootstrapBasicForm $ HtmlComment
--   \<$\> areq (snHtmlFieldCustomized "{toolbar:false}") \"Title\" Nothing
--   \<*\> areq snHtmlField \"Comment\" Nothing
-- @

module Yesod.Form.Summernote
    ( YesodSummernote (..)
    , snHtmlField
    , snHtmlFieldCustomized
    ) where

import           Control.Monad                   (when)
import           Data.Maybe                      (listToMaybe)
import           Data.Text                       (Text, pack)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Hamlet                     (shamlet)
import           Text.HTML.SanitizeXSS           (sanitizeBalance)
import           Text.Julius                     (julius, rawJS)
import           Yesod.Core
import           Yesod.Form
import           Yesod.Form.Jquery

class YesodJquery a => YesodSummernote a where
    -- | Summernote Editor CSS location.
    urlSummernoteCss :: a -> Either (Route a) Text
    urlSummernoteCss _ = Right
        "https://cdnjs.cloudflare.com/ajax/libs/summernote/0.8.12/summernote-lite.css"
    -- | Summernote Editor library location.
    urlSummernoteScript :: a -> Either (Route a) Text
    urlSummernoteScript _ = Right
        "https://cdnjs.cloudflare.com/ajax/libs/summernote/0.8.12/summernote-lite.js"

-- | Customizable Summernote editor field.
--
-- @cfg@ argument should be a JSON formatted string, it will be passed to
-- @$.summernote()@ call as first argument.
--
-- @
-- snHtmlFieldCustomized "{ height: 150, codemirror: { theme:'monokai' } }"
-- @
snHtmlFieldCustomized :: YesodSummernote site
                      => String
                      -> Field (HandlerFor site) Html
snHtmlFieldCustomized cfg = Field
    { fieldParse =
        \e _ -> return $
            Right . fmap (preEscapedToMarkup . sanitizeBalance) . listToMaybe $ e
    , fieldView = \theId name attrs val _isReq -> do
        toWidget [shamlet|
$newline never
<textarea id="#{theId}" *{attrs} name="#{name}" .html>#{showVal val}
|]
        master <- getYesod
        addScript'     urlJqueryJs
        addStylesheet' urlSummernoteCss
        addScript'     urlSummernoteScript
        toWidget $ [julius|
$(document).ready(function(){
  var input = document.getElementById("#{rawJS theId}");
  $(input).summernote(#{rawJS cfg}).on('summernote.change',function(){
    $(input).text($(input).summernote('code'));
  });
});|]
    , fieldEnctype = UrlEncoded
    }
  where
    showVal = either id (pack . renderHtml)

-- | Summernote editor field with default settings.
snHtmlField :: YesodSummernote site
            => Field (HandlerFor site) Html
snHtmlField = snHtmlFieldCustomized ""


addScript' :: (MonadWidget m, HandlerSite m ~ site)
           => (site -> Either (Route site) Text)
           -> m ()
addScript' f = do
    y <- getYesod
    addScriptEither $ f y

addStylesheet' :: (MonadWidget m, HandlerSite m ~ site)
               => (site -> Either (Route site) Text)
               -> m ()
addStylesheet' f = do
    y <- getYesod
    addStylesheetEither $ f y
