{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}
module Frontend where

import Control.Arrow ((&&&))
import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Bitraversable
import Data.Text (Text)
import Data.Function (fix)
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
import Obelisk.Route -- (R (..)) -- TODO switch back after https://github.com/obsidiansystems/obelisk/pull/567
import Obelisk.Route.Frontend (R, subRoute_, routeLink)
import Reflex.Dom.Core
import qualified System.IO
import qualified Text.MMark as MMark
import qualified Text.MMark.Internal.Type as MMark

import Common.Route
import Reflex.MMark.Render
import Tutorial
import Tutorial.Markdown

-- Parses markdown at compile time into AST so no validation errors at run-time.
parsedTutorial :: [Either (TutorialRoute ()) MMark.Bni]
parsedTutorial = $(do
  let -- TODO propose Lift1 class for functors
      shallowLiftEither :: Either Exp Exp -> Exp
      shallowLiftEither = either (AppE $ ConE 'Left) (AppE $ ConE 'Right)
      postProcessed :: [Either Name MMark.Bni]
      postProcessed = go =<< parsedMarkdown
        where
          go = \case
            cb@(MMark.CodeBlock _ infoString)
              -- TODO deduplicate
              | (_:_) <- T.breakOnAll "tutorial1 ::"  infoString -> [Right cb, Left $ mkName "TutorialRoute_1"]
              | (_:_) <- T.breakOnAll "tutorial2 ::"  infoString -> [Right cb, Left $ mkName "TutorialRoute_2"]
              | (_:_) <- T.breakOnAll "tutorial3 ::"  infoString -> [Right cb, Left $ mkName "TutorialRoute_3"]
              | (_:_) <- T.breakOnAll "tutorial4 ::"  infoString -> [Right cb, Left $ mkName "TutorialRoute_4"]
              | (_:_) <- T.breakOnAll "tutorial5 ::"  infoString -> [Right cb, Left $ mkName "TutorialRoute_5"]
              | (_:_) <- T.breakOnAll "tutorial6 ::"  infoString -> [Right cb, Left $ mkName "TutorialRoute_6"]
              | (_:_) <- T.breakOnAll "tutorial7 ::"  infoString -> [Right cb, Left $ mkName "TutorialRoute_7"]
              | (_:_) <- T.breakOnAll "tutorial8 ::"  infoString -> [Right cb, Left $ mkName "TutorialRoute_8"]
              | (_:_) <- T.breakOnAll "tutorial9 ::"  infoString -> [Right cb, Left $ mkName "TutorialRoute_9"]
              | (_:_) <- T.breakOnAll "tutorial10 ::" infoString -> [Right cb, Left $ mkName "TutorialRoute_10"]
              | (_:_) <- T.breakOnAll "tutorial11 ::" infoString -> [Right cb, Left $ mkName "TutorialRoute_11"]
            block -> [Right block]
  fmap ListE $ traverse (fmap shallowLiftEither . bitraverse conE lift) $ postProcessed)

renderReflex'
  :: DomBuilder t m
  => (x -> m ())
  -> [Either x MMark.Bni]
  -> m ()
renderReflex' f md = mapM_ (either f rBlock) md
  where
    rBlock
      = fix defaultBlockRender
      . fmap rInlines
    rInlines
      = MMark.mkOisInternal &&& mapM_ (fix $ defaultInlineRender defaultLinkRender)

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
        let renderLink rt = el "p" $
              routeLink (FrontendRoute_Tutorial :/ rt :/ ()) $
                text "Go to snippet"

        -- TODO determine why this block can't be placed after the rendering code below
        prerender_ blank $ do
          doc <- askDocument
          liftJSM $ forkJSM $ do
            _ <- getElementsByTagName doc ("pre" :: Text)
            void $ eval @Text "hljs.initHighlighting();"

        elAttr "div" ("class" =: "container") $ do
          renderReflex' renderLink parsedTutorial

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
