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
module Tutorial.Markdown where

import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8)
import "template-haskell" Language.Haskell.TH
import "template-haskell" Language.Haskell.TH.Syntax
import qualified Text.MMark as MMark
import qualified Text.MMark.Internal.Type as MMark

parsedMarkdown :: [MMark.Bni]
parsedMarkdown = $(do
  let fp = "src/Tutorial.md"
  qAddDependentFile fp
  rawMarkdown <- fmap decodeUtf8 $ runIO $ BS.readFile fp
  let parseResult = MMark.parse "README.md" rawMarkdown
  parsed <- case parseResult of
    Left _ -> do
      fail "Oops! Couldn't parse README.md. Is it valid markdown?"
    Right parsed -> pure $ MMark.mmarkBlocks parsed
  lift parsed)
