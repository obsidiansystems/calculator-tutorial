{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend where

import Cheapskate.Parse (markdown)
import Cheapskate.Html (renderDoc)
import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.FileEmbed (embedFile)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)
import GHCJS.DOM.Document (getElementsByTagName)
import GHCJS.DOM.Element (setInnerHTML)
import GHCJS.DOM.HTMLCollection (getLength)
import Language.Javascript.JSaddle (eval, liftJSM)
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route.Frontend (R, subRoute_)
import Reflex.Dom.Core
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Common.Route
import Tutorial

md :: Text
md = decodeUtf8 $(embedFile "README.md")

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
    el "title" $ text "Building a Calculator with Reflex"
    elAttr "meta" ("charset" =: "utf-8") blank
    let viewportAttrs = mconcat
          [ "name" =: "viewport"
          , "content" =: "width=device-width, initial-scale=1, shrink-to-fit=no"
          ]
    elAttr "meta" viewportAttrs blank
    mapM_ (\x -> elAttr "link" ("rel" =: "stylesheet" <> "href" =: x) blank)
      [ static @"bootstrap.min.css"
      , static @"highlight/styles/sunburst.css"
      ]
    elAttr "script" ("src" =: static @"highlight/highlight.pack.js") blank
  , _frontend_body = subRoute_ $ \case
      FrontendRoute_Main -> prerender_ blank $ do
        (container, _) <- elAttr' "div" ("class" =: "container") blank
        let tutorialHtml = toStrict $ renderHtml $ renderDoc $ markdown def md
        doc <- askDocument
        liftJSM $ forkJSM $ do
          setInnerHTML (_element_raw container) tutorialHtml
          let wait = do
                els <- getElementsByTagName doc ("pre" :: Text)
                n <- getLength els
                if n > 0 then return () else do
                  liftIO $ threadDelay 10000
                  wait
          wait
          void $ eval @Text "hljs.initHighlighting();"
      FrontendRoute_Tutorial -> subRoute_ $ \case
        TutorialRoute_1 -> tutorial1
        TutorialRoute_2 -> tutorial2
        TutorialRoute_3 -> tutorial3
        TutorialRoute_4 -> tutorial4
        TutorialRoute_5 -> tutorial5
        TutorialRoute_6 -> tutorial6
        TutorialRoute_7 -> tutorial7
        TutorialRoute_8 -> tutorial8
  }