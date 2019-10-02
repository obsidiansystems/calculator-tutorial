```haskell
{-# LANGUAGE OverloadedStrings #-}
module Tutorial where

import Reflex.Dom.Core

tutorial :: DomBuilder t m => m ()
tutorial = text "asdf"
```
