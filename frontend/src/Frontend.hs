{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.FileEmbed (embedFile)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)
import GHCJS.DOM.Document (getElementsByTagName)
import GHCJS.DOM.HTMLCollection (getLength)
import Language.Javascript.JSaddle (eval, liftJSM)
import Obelisk.Configs (HasConfigs, getConfigs)
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route.Frontend (R, subRoute_)
import Reflex.Dom.Core
import qualified Text.MMark as MMark
import qualified Lucid.Base as Lucid (renderText)

import Common.Route
import Tutorial

rawMarkdown :: Text
rawMarkdown = decodeUtf8 $(embedFile "README.md")

rewriteLinks :: HasConfigs m => Text -> m Text
rewriteLinks md = do
  route <- Map.lookup "common/route" <$> getConfigs
  pure $ case route of
    Nothing -> md
    Just r -> T.replace "http://localhost:8000" (T.strip $ decodeUtf8 r) md

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
        parseResult <- MMark.parse "README.md" <$> rewriteLinks rawMarkdown
        case parseResult of
          Left err -> do
            liftIO $ print err
            text "Oops! Couldn't parse README.md. Is it valid markdown?"
          Right parsed -> void $ elDynHtmlAttr' "div" ("class" =: "container") $ pure $
            toStrict . Lucid.renderText $ MMark.render parsed
        doc <- askDocument
        liftJSM $ forkJSM $ do
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
