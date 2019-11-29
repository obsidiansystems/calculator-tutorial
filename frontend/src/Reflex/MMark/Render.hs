-- |
-- Module      :  Text.MMark.Render
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- MMark rendering machinery.

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Reflex.MMark.Render
  ( renderReflex
  ) where

import Control.Arrow
import Control.Monad
import Data.Char (isSpace)
import Data.Function (fix)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.URI as URI
import Reflex.Dom hiding (Link)
import Text.MMark.Internal.Type hiding (Render (..))
import Text.MMark.Internal.Util

-- | Render a 'MMark' markdown document. You can then render @'Html' ()@ to
-- various things:
--
--     * to lazy 'Data.Taxt.Lazy.Text' with 'renderText'
--     * to lazy 'Data.ByteString.Lazy.ByteString' with 'renderBS'
--     * directly to file with 'renderToFile'

renderReflex
  :: DomBuilder t m
  => [Bni]
  -> m ()
renderReflex md = mapM_ rBlock md
  where
    rBlock
      = fix defaultBlockRender
      . fmap rInlines
    rInlines
      = (mkOisInternal &&& mapM_ (fix defaultInlineRender))

-- | The default 'Block' render. Note that it does not care about what we
-- have rendered so far because it always starts rendering. Thus it's OK to
-- just pass it something dummy as the second argument of the inner
-- function.

defaultBlockRender
  :: DomBuilder t m
  => (Block (Ois, m ()) -> m ())
     -- ^ Rendering function to use to render sub-blocks
  -> Block (Ois, m ()) -> m ()
defaultBlockRender blockRender = \case
  ThematicBreak ->
    el "hr" blank
  Heading1 (h,html) ->
    elAttr "h1" (mkId h) html
  Heading2 (h,html) ->
    elAttr "h2" (mkId h) html
  Heading3 (h,html) ->
    elAttr "h3" (mkId h) html
  Heading4 (h,html) ->
    elAttr "h4" (mkId h) html
  Heading5 (h,html) ->
    elAttr "h5" (mkId h) html
  Heading6 (h,html) ->
    elAttr "h6" (mkId h) html
  CodeBlock infoString txt -> do
    let f x = "class" =: ("language-" <> T.takeWhile (not . isSpace) x)
    el "pre" $ elAttr "code" (maybe M.empty f infoString) (text txt)
  Naked (_,html) ->
    html
  Paragraph (_,html) ->
    el "p" html
  Blockquote blocks -> do
    el "blockquote" (mapM_ blockRender blocks)
  OrderedList i items -> do
    let startIndex =
          if i == 1
          then M.empty
          else "start" =: T.pack (show i)
    elAttr "ol" startIndex $ do
      forM_ items $ \x -> do
        el "li" (mapM_ blockRender x)
  UnorderedList items -> do
    el "ul" $ do
      forM_ items $ \x -> do
        el "li" (mapM_ blockRender x)
  Table calign (hs :| rows) -> do
    el "table" $ do
      el "thead" $ do
        el "tr" $
          forM_ (NE.zip calign hs) $ \(a, h) ->
            elAttr "th" (alignStyle a) (snd h)
      el "tbody" $ do
        forM_ rows $ \row -> do
          el "tr" $
            forM_ (NE.zip calign row) $ \(a, h) ->
              elAttr "td" (alignStyle a) (snd h)
  where
    mkId ois = "id" =: headerId (getOis ois)
    alignStyle = \case
      CellAlignDefault -> M.empty
      CellAlignLeft    -> "style" =: "text-align:left"
      CellAlignRight   -> "style" =: "text-align:right"
      CellAlignCenter  -> "style" =: "text-align:center"

-- | The default render for 'Inline' elements. Comments about
-- 'defaultBlockRender' apply here just as well.

defaultInlineRender
  :: DomBuilder t m
  => (Inline -> m ())
     -- ^ Rendering function to use to render sub-inlines
  -> Inline -> m ()
defaultInlineRender inlineRender = \case
  Plain txt ->
    text txt
  LineBreak ->
    el "br" blank
  Emphasis inner ->
    el "em" (mapM_ inlineRender inner)
  Strong inner ->
    el "strong" (mapM_ inlineRender inner)
  Strikeout inner ->
    el "del" (mapM_ inlineRender inner)
  Subscript inner ->
    el "sub" (mapM_ inlineRender inner)
  Superscript inner ->
    el "sup" (mapM_ inlineRender inner)
  CodeSpan txt ->
    el "code" (text txt)
  Link inner dest mtitle ->
    let title = maybe M.empty ("title" =:) mtitle
    in elAttr "a" ("href" =: URI.render dest <> title) $ mapM_ inlineRender inner
  Image desc src mtitle ->
    let title = maybe M.empty ("title" =:) mtitle
    in elAttr "img" ("alt" =: asPlainText desc <> "src" =: URI.render src <> title) blank
