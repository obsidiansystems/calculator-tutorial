{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}
module Frontend where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import GHCJS.DOM.Document (getElementsByTagName)
import GHCJS.DOM.HTMLCollection (getLength)
import "template-haskell" Language.Haskell.TH
import "template-haskell" Language.Haskell.TH.Syntax
import Language.Javascript.JSaddle (eval, liftJSM)
import Obelisk.Configs (HasConfigs, getConfigs)
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route.Frontend (R, subRoute_)
import Reflex.Dom.Core
import qualified Text.MMark as MMark
import qualified Text.MMark.Internal.Type as MMark

import Common.Route
import Reflex.MMark.Render
import Tutorial

rewriteLinks :: HasConfigs m => Text -> m Text
rewriteLinks md = do
  route <- Map.lookup "common/route" <$> getConfigs
  pure $ case route of
    Nothing -> md
    Just r -> T.replace "http://localhost:8000" (T.strip $ decodeUtf8 r) md

-- Parses markdown at compile time into AST so no validation errors at run-time.
parsedMarkdown :: [MMark.Bni]
parsedMarkdown = $(do
  let fp = "README.md"
  qAddDependentFile fp
  rawMarkdown <- fmap decodeUtf8 $ runIO $ BS.readFile fp
  let parseResult = MMark.parse "README.md" rawMarkdown
  case parseResult of
    Left _ -> do
      fail "Oops! Couldn't parse README.md. Is it valid markdown?"
    Right parsed -> lift $ MMark.mmarkBlocks parsed)

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
      , static @"calculator/style.css"
      ]
    elAttr "script" ("src" =: static @"highlight/highlight.pack.js") blank
  , _frontend_body = subRoute_ $ \case
      FrontendRoute_Main -> do
        elAttr "div" ("class" =: "container") $
          renderReflex parsedMarkdown
        prerender_ blank $ do
          doc <- askDocument
          liftJSM $ forkJSM $ do
            getElementsByTagName doc ("pre" :: Text)
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
        TutorialRoute_9 -> tutorial9
        TutorialRoute_10 -> tutorial10
        TutorialRoute_11 -> tutorial11
  }
