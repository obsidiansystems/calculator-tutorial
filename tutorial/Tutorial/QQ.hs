{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Tutorial.QQ
  ( exampleDec
  , parseMode
  , knownExtensions
  ) where

import Data.Text (Text)
import "template-haskell" Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Language.Haskell.Exts as Exts
import qualified Language.Haskell.Meta as Meta

parseMode :: Exts.ParseMode
parseMode = Exts.defaultParseMode
    { Exts.baseLanguage = Exts.Haskell2010
    , Exts.fixities = Just $ concat
      [ Exts.preludeFixities
      , Exts.baseFixities
      , Exts.infixl_ 8 ["^?", "^."]
      , Exts.infixl_ 7 ["=:"]
      , Exts.infixr_ 4 ["%~", ".~", "?~", "<>~"]
      , Exts.infixl_ 1 ["&"]
      ]
    , Exts.extensions = Exts.EnableExtension <$> knownExtensions
    }

knownExtensions :: [Exts.KnownExtension]
knownExtensions =
  [ Exts.DataKinds
  , Exts.ExistentialQuantification
  , Exts.ExplicitForAll
  , Exts.GADTs
  , Exts.LambdaCase
  , Exts.MultiParamTypeClasses
  , Exts.RecordWildCards
  , Exts.RecursiveDo
  , Exts.ScopedTypeVariables
  , Exts.TypeApplications
  , Exts.TypeFamilies
  , Exts.TemplateHaskell
  ]

-- | Quote a single declaration (with type signature). This will turn
-- @
-- [exampleDec|
-- test :: ()
-- test = ()
-- |]
-- @
-- into:
-- `test`, a declaration as above
-- `test_code`, a string containing the contents of the quoter
exampleDec :: QuasiQuoter
exampleDec = QuasiQuoter
  { quoteExp = const $ error "exampleDec: not a declaration"
  , quotePat = const $ error "exampleDec: not a declaration"
  , quoteType = const $ error "exampleDec: not a declaration"
  , quoteDec = \s -> case filter (not . all C.isSpace) $ lines s of
    ty : rest -> do
      let decs = do
            t@(Exts.TypeSig _ [Exts.Ident _ name] _) <- Exts.parseDeclWithMode parseMode ty
            f <- Exts.parseDeclWithMode parseMode $ unlines rest
            pure (name, t, f)
      case decs of
        Exts.ParseFailed x y -> do
          fail $ "exampleDec: expected a type signature and a function declaration: " <> show (x,y)
        Exts.ParseOk (name, tyDec, funDec) -> do
          let cn = mkName (name <> "_code")
          sequence
            [ sigD cn (conT ''Text)
            , funD cn [clause [] (normalB [|T.pack $ unlines (ty:rest)|]) []]
            , pure $ Meta.toDec tyDec
            , pure $ Meta.toDec funDec
            ]
    [] -> fail "exampleDec: expected a single declaration"
  }
