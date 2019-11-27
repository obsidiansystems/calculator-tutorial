# Tutorial
In this example, we'll be following [Luite Stegemann's lead](http://weblog.luite.com/wordpress/?p=127) and building a simple functional reactive calculator to be used in a web browser.

TODO: Add a brief intro to reflex(-dom)/FRP

## The structure of this document

### Literate Haskell
This document is a [literate haskell](https://wiki.haskell.org/Literate_programming) source file written in markdown.  We're using [markdown-unlit](https://github.com/sol/markdown-unlit#literate-haskell-support-for-markdown) to process this source file and turn it into something our compiler can understand.

### Running the code

You can run this tutorial by:

1. [Installing obelisk](https://github.com/obsidiansystems/obelisk/#installing-obelisk), a framework and development tool for multi-platform Haskell applications.

2. Cloning this repository.

    ```bash
    git clone git@github.com:reflex-frp/reflex-calculator-tutorial
    ```

3. Running the application with the `ob` command.

    ```bash
    cd reflex-calculator-tutorial
    ob run
    ```

4. Navigating to [http://localhost:8000](http://localhost:8000). If you want to run it at a different hostname or port, modify the `config/common/route` configuration file.

While `ob run` is running the application, any changes to this source file will cause the code to be reloaded and the application to be restarted. If the changes have introduced any errors or warnings, you'll see those in the `ob run` window. Tinker away!

### Navigating to particular tutorial snippets

Over the course of this tutorial, we're going to progressively build a calculator application. Each step in the process of constructing the calculator will be a numbered function in this document. For example, the first function is `tutorial1`, below.

To see what the code in `tutorial1` does when it runs, you can navigate to [/tutorial/1](http://localhost:8000/tutorial/1). The same applies to any other numbered function: just change the number at the end of the url to match that of the function you'd like to see in action.

### Imports and extensions

Because this is one big source file, all of our functions will share a set of imports and language extensions, declared here:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}

module Tutorial where

import Reflex
import Reflex.Dom
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (pack, unpack, Text)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Control.Applicative ((<*>), (<$>))
import Control.Monad.Fix (MonadFix)

```

That's all for the preliminaries. Let's get to it!

## DOM Basics

Reflex's companion library, Reflex-Dom, contains a number of functions used to build and interact with the Document Object Model. Let's start by getting a basic frontend up and running.

```haskell
tutorial1 :: DomBuilder t m => m ()
tutorial1 = el "div" $ text "Welcome to Reflex"
```
[Go to snippet](http://localhost:8000/tutorial/1)

`el` has the type signature:

```
el :: MonadWidget t m => Text -> m a -> m a
```

The first argument to `el` is a `Text`, which will become the tag of the html element produced. The second argument is a `Widget`, which will become the child of the element being produced. We turned on the `OverloadedStrings` extension so that the literal string in our source file would be interpreted as the appropriate type (`Text` rather than `String`).

> #### Sidebar: Interpreting the MonadWidget type
> FRP-enabled datatypes in Reflex take an argument `t`, which identifies the FRP subsystem being used.  This ensures that wires don't get crossed if a single program uses Reflex in multiple different contexts.  You can think of `t` as identifying a particular "timeline" of the FRP system.
> Because most simple programs will only deal with a single timeline, we won't revisit the `t` parameters in this tutorial.  As long as you make sure your `Event`, `Behavior`, and `Dynamic` values all get their `t` argument, it'll work itself out.

In our example, `el "div" $ text "Welcome to Reflex"`, the first argument to `el` was `"div"`, indicating that we are going to produce a div element.

The second argument to `el` was `text "Welcome to Reflex"`. The type signature of `text` is:

```
text :: MonadWidget t m => Text -> m ()
```

`text` takes a `Text` and produces a `Widget`. The `Text` becomes a text DOM node in the parent element of the `text`. Of course, instead of a `Text`, we could have used `el` here as well to continue building arbitrarily complex DOM. For instance, if we wanted to make a unordered list:

```haskell
tutorial2 :: DomBuilder t m => m ()
tutorial2 = el "div" $ do
 el "p" $ text "Reflex is:"
 el "ul" $ do
   el "li" $ text "Efficient"
   el "li" $ text "Higher-order"
   el "li" $ text "Glitch-free"
```
[Go to snippet](http://localhost:8000/tutorial/2)

### Dynamics and Events
Of course, we want to do more than just view a static webpage. Let's start by getting some user input and printing it.

```haskell
tutorial3 :: (DomBuilder t m, PostBuild t m) => m ()
tutorial3 = el "div" $ do
  t <- inputElement def
  dynText $ _inputElement_value t

```
[Go to snippet](http://localhost:8000/tutorial/3)

Running this in your browser, you'll see that it produces a `div` containing an `input` element. When you type into the `input` element, the text you enter appears inside the div as well.

`inputElement` is a function with the following type:

```
inputElement :: DomBuilder t m
  => InputElementConfig er t (DomBuilderSpace m)
  -> m (InputElement er (DomBuilderSpace m) t)
```

It takes a `InputElementConfig` (given a default value in our example), and produces a `Widget` whose result is a `InputElement`. The `InputElement` exposes the following functionality:

```
data InputElement er d t
   = InputElement { _inputElement_value :: Dynamic t Text
                  , _inputElement_checked :: Dynamic t Bool
                  , _inputElement_checkedChange :: Event t Bool
                  , _inputElement_input :: Event t Text
                  , _inputElement_hasFocus :: Dynamic t Bool
                  , _inputElement_element :: Element er d t
                  , _inputElement_raw :: RawInputElement d
                  , _inputElement_files :: Dynamic t [RawFile d]
                  }
```

Here we are using `_inputElement_value` to access the `Dynamic Text` value of the `InputElement`. Conveniently, `dynText` takes a `Dynamic Text` and displays it. It is the dynamic version of `text`.

### A Number Input
A calculator was promised, I know. We'll start building the calculator by creating an input for numbers.

```haskell
tutorial4 :: (DomBuilder t m, PostBuild t m) => m ()
tutorial4 = el "div" $ do
  t <- inputElement $ def
    & inputElementConfig_initialValue .~ "0"
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
  dynText $ _inputElement_value t
```
[Go to snippet](http://localhost:8000/tutorial/4)

The code above overrides some of the default values of the `InputElementConfig`. We provide a `Map Text Text` value for the `inputElementConfig_elementConfig`'s `elementConfig_initialAttributes`, specifying the html input element's `type` attribute to `number`.

Next, we override the default initial value of the `InputElement`. We gave it `"0"`. Even though we're making an html `input` element with the attribute `type=number`, the result is still a `Text`. We'll convert this later.

Let's do more than just take the input value and print it out. First, let's make sure the input is actually a number:

```haskell
tutorial5 :: (DomBuilder t m, PostBuild t m) => m ()
tutorial5 = el "div" $ do
  x <- numberInput
  let numberString = fmap (pack . show) x
  dynText numberString
  where
    numberInput :: DomBuilder t m => m (Dynamic t (Maybe Double))
    numberInput = do
      n <- inputElement $ def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
      return . fmap (readMaybe . unpack) $ _inputElement_value n
```
[Go to snippet](http://localhost:8000/tutorial/5)

We've defined a function `numberInput` that both handles the creation of the `InputElement` and reads its value. Recall that `_inputElement_value` gives us a `Dynamic Text`. The final line of code in `numberInput` uses `fmap` to apply the function `readMaybe . unpack` to the `Dynamic` value of the `InputElement`. This produces a `Dynamic (Maybe Double)`. Our `main` function uses `fmap` to map over the `Dynamic (Maybe Double)` produced by `numberInput` and `pack . show` the value it contains. We store the new `Dynamic Text` in `numberString` and feed that into `dynText` to actually display the `Text`

Running the app at this point should produce an input and some text showing the `Maybe Double`. Typing in a number should produce output like `Just 12.0` and typing in other text should produce the output `Nothing`.

### Adding
Now that we have `numberInput` we can put together a couple inputs to make a basic calculator.

```haskell
tutorial6 :: (DomBuilder t m, PostBuild t m) => m ()
tutorial6 = el "div" $ do
  nx <- numberInput
  text " + "
  ny <- numberInput
  text " = "
  let result = zipDynWith (\x y -> (+) <$> x <*> y) nx ny
      resultString = fmap (pack . show) result
  dynText resultString
  where
    numberInput :: DomBuilder t m => m (Dynamic t (Maybe Double))
    numberInput = do
      n <- inputElement $ def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
      return . fmap (readMaybe . unpack) $ _inputElement_value n
```
[Go to snippet](http://localhost:8000/tutorial/6)

`numberInput` hasn't changed here. Our `main` function now creates two inputs. `zipDynWith` is used to produce the actual sum of the values of the inputs. The type signature of `zipDynWith` is:

```
Reflex t => (a -> b -> c) -> Dynamic t a -> Dynamic t b -> Dynamic t c
```

You can see that it takes a function that combines two pure values and produces some other pure value, and two `Dynamic`s, and produces a `Dynamic`.

In our case, `zipDynWith` is combining the results of our two `numberInput`s (with a little help from `Control.Applicative`) into a sum.

We use `fmap` again to apply `pack . show` to `result` (a `Dynamic (Maybe Double)`) resulting in a `Dynamic Text`. This `resultText` is then displayed using `dynText`.

### Supporting Multiple Operations
Next, we'll add support for other operations. We're going to add a dropdown so that the user can select the operation to apply. The function `dropdown` has the type:

```
dropdown :: (MonadWidget t m, Ord k) => k -> Dynamic t (Map k Text) -> DropdownConfig t k -> m (Dropdown t k)
```

The first argument is the initial value of the `Dropdown`. The second argument is a `Dynamic (Map k Text)` that represents the options in the dropdown. The `Text` values of the `Map` are the strings that will be displayed to the user. If the initial key is not in the `Map`, it is added and given a `Text` value of `""`. The final argument is a `DropdownConfig`.

Our supported operations will be:

```haskell
data Op = Plus | Minus | Times | Divide
  deriving (Eq, Ord, Show)

runOp :: Fractional a => Op -> a -> a -> a
runOp s =
  case s of
    Plus -> (+)
    Minus -> (-)
    Times -> (*)
    Divide -> (/)
```

Note that because we can't put datastructure definitions in the `where` clause, this definition is going to be shared by all the functions in this module that use this type.

We'll want some kind of string representation of each operation to display:

```
ops = Map.fromList [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/")]
```

We'll use this as an argument to `dropdown`:

```
d <- dropdown Times (constDyn ops) def
```

We are using `constDyn` again here to turn our `Map` of operations into a `Dynamic`. Using `def`, we provide the default `DropdownConfig`. The result, `d`, will be a `Dropdown`. We can retrieve the `Dynamic` selection of a `Dropdown` by using `_dropdown_value`.

```haskell
tutorial7 :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
tutorial7 = el "div" $ do
  nx <- numberInput
  d <- dropdown Times (constDyn ops) def
  ny <- numberInput
  let values = zipDynWith (,) nx ny
      result = zipDynWith (\o (x,y) -> runOp o <$> x <*> y) (_dropdown_value d) values
      resultText = fmap (pack . show) result
  text " = "
  dynText resultText
  where
    numberInput :: DomBuilder t m => m (Dynamic t (Maybe Double))
    numberInput = do
      n <- inputElement $ def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
      return . fmap (readMaybe . unpack) $ _inputElement_value n
    ops :: Map Op Text
    ops = Map.fromList [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/")]
```
[Go to snippet](http://localhost:8000/tutorial/7)

This is our complete program. We've added an uninteresting function `runOp` that takes an `Op` and returns an operation. The keys of the `Map` we used to create the `Dropdown` had the type `Op`. When we retrieve the value of `Dropdown`, we'll use `runOp` to turn the `Dropdown` selection into the function we need to apply to our numbers.

After creating the two `numberInput`s, we combine them using `zipDynWith` applying `(,)`, making a tuple of type `Dynamic (Maybe Double, Maybe Double)` and binding it to `values`.

Next, we call `zipDynWith` again, combining the `_dropdown_value` and `values`. Now, instead of applying `(+)` to our `Double` values, we use `runOp` to select an operation based on the `Dynamic` value of our `Dropdown`.

Running the app at this point will give us our two number inputs with a dropdown of operations sandwiched between them. Multiplication should be pre-selected when the page loads.

### Dynamic Element Attributes
Let's spare a thought for the user of our calculator and add a little UI styling. Our number input currently looks like this:

```
numberInput :: MonadWidget t m => m (Dynamic t (Maybe Double))
numberInput = do
  n <- inputElement $ def
    & inputElementConfig_initialValue .~ "0"
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
  return . fmap (readMaybe . unpack) $ _inputElement_value n
```

Let's give it some html attributes to work with:

```
numberInput :: MonadWidget t m => m (Dynamic t (Maybe Double))
numberInput = do
  let initAttrs = (("type" =: "number") <> ("style" =: "border-color: blue"))
  n <- inputElement $ def
    & inputElementConfig_initialValue .~ "0"
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ initAttrs
  return . fmap (readMaybe . unpack) $ _inputElement_value n
```

Here, we've used a `(Map Text Text)`. This `Map` represents the html attributes of our inputs.

Static attributes are useful and quite common, but attributes will often need to change.
Instead of just making the `InputElement` blue, let's change it's color based on whether the input successfully parses to a `Double`:

```
...
numberInput :: (MonadWidget t m) => m (Dynamic t (Maybe Double))
numberInput = do
  let initAttrs = ("type" =: "number") <> (style False)
      color err = if err then "red" else "green"
      style err = "style" =: ("border-color: " <> color err)
      styleChange :: Maybe Double -> Map AttributeName (Maybe Text)
      styleChange result = case result of
        (Just _) -> fmap Just (style False)
        (Nothing) -> fmap Just (style True)

  rec
    n <- inputElement $ def
      & inputElementConfig_initialValue .~ "0"
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ initAttrs
      & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ modAttrEv
    let result = fmap (readMaybe . unpack) $ _inputElement_value n
        modAttrEv  = fmap styleChange (updated result)
  return result
```

Note that we need to add a language pragma here to enable the `RecursiveDo` language extension.  Here `style` function takes a `Bool` value, whether input is correct or not, and it gives a `Map` of attributes with green or red color respectively.  The next function `styleChange` actually produces a `Map` which tells which attribute to change.  If the value of a key in the `Map` is a `Just` value then the attribute is either added or modified.  If the value of key is `Nothing`, then that attribute is removed.  An `Event` of this `Map` is specified in the `elementConfig_modifyAttributes`.

In the first line of the `rec`, we have supplied this `Event` as argument `modAttrEv`. The `Dynamic` value of the input is bound to `result`. The code for parsing this value has not changed.

After we bind `result`, we use `fmap` again to apply a switching function to the `updated result` `Event`. The switching function checks whether the value was successfully parsed and gives the corresponding `Event` to modify the attributes.

The complete program now looks like this:

```haskell
tutorial8 :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
tutorial8 = el "div" $ do
  nx <- numberInput
  d <- dropdown Times (constDyn ops) def
  ny <- numberInput
  let values = zipDynWith (,) nx ny
      result = zipDynWith (\o (x,y) -> runOp o <$> x <*> y) (_dropdown_value d) values
      resultText = fmap (pack . show) result
  text " = "
  dynText resultText
  where
    numberInput :: (DomBuilder t m, MonadFix m) => m (Dynamic t (Maybe Double))
    numberInput = do
      let initAttrs = ("type" =: "number") <> (style False)
          color err = if err then "red" else "green"
          style err = "style" =: ("border-color: " <> color err)
          styleChange :: Maybe Double -> Map AttributeName (Maybe Text)
          styleChange result = case result of
            (Just _) -> fmap Just (style False)
            (Nothing) -> fmap Just (style True)
      rec
        n <- inputElement $ def
          & inputElementConfig_initialValue .~ "0"
          & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ initAttrs
          & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ modAttrEv
        let result = fmap (readMaybe . unpack) $ _inputElement_value n
            modAttrEv  = fmap styleChange (updated result)
      return result
    ops :: Map Op Text
    ops = Map.fromList [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/")]
```
[Go to snippet](http://localhost:8000/tutorial/8)

The input border colors will now change depending on their value.

### Number Pad

```haskell
numberPad :: (DomBuilder t m) => m (Event t Text)
numberPad = do
  b0 <- ("0" <$) <$> button "0"
  b1 <- ("1" <$) <$> button "1"
  b2 <- ("2" <$) <$> button "2"
  b3 <- ("3" <$) <$> button "3"
  b4 <- ("4" <$) <$> button "4"
  b5 <- ("5" <$) <$> button "5"
  b6 <- ("6" <$) <$> button "6"
  b7 <- ("7" <$) <$> button "7"
  b8 <- ("8" <$) <$> button "8"
  b9 <- ("9" <$) <$> button "9"
  return $ leftmost [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9]

tutorial9 :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
tutorial9 = el "div" $ do
  numberButton <- numberPad
  clearButton <- button "C"
  let buttons = leftmost [ Nothing <$ clearButton , Just <$> numberButton ]
  dstate <- accumDyn collectButtonPresses initialState buttons
  dynText dstate
  where
    initialState = T.empty
    collectButtonPresses state = \case
      Nothing -> initialState
      Just digit -> state <> digit
```
[Go to snippet](http://localhost:8000/tutorial/9)

### Four Function Calculator

```haskell
data CalcState = CalcState
  { _calcState_accum :: Double
  , _calcState_op    :: Maybe Op
  , _calcState_input :: Text
  } deriving (Show)

data Button
  = ButtonNumber Text
  | ButtonOp Op
  | ButtonEq
  | ButtonClear

tutorial10 :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
tutorial10 = el "div" $ do
  numberButtons <- numberPad
  bPeriod <- ("." <$) <$> button "."
  bPlus <- (Plus <$) <$> button "+"
  bMinus <- (Minus <$) <$> button "-"
  bTimes <- (Times <$) <$> button "*"
  bDivide <- (Divide <$) <$> button "/"
  let opButtons = leftmost [bPlus, bMinus, bTimes, bDivide]
  bEq <- button "="
  bClear <- button "C"
  let buttons = leftmost
        [ ButtonNumber <$> numberButtons
        , ButtonNumber <$> bPeriod
        , ButtonOp <$> opButtons
        , ButtonEq <$ bEq
        , ButtonClear <$ bClear
        ]
  d0 <- accumDyn collectButtonPresses initState buttons
  dynText (debugDisplayState <$> d0)
  el "br" blank
  dynText (displayState <$> d0)
  where
    initState = CalcState 0 Nothing ""
    collectButtonPresses state@(CalcState accum op input) = \case
      ButtonNumber d ->
        if d == "." && T.find (== '.') input /= Nothing
        then state
        else CalcState accum op (input <> d)
      ButtonOp pushedOp ->
        if T.null input
        then CalcState accum (Just pushedOp) input
        else apply state (Just pushedOp)
      ButtonEq ->
        if T.null input
        then state
        else apply state Nothing
      ButtonClear -> initState

    apply state@(CalcState accum mOp input) pushedOp =
      case readMaybe (unpack input) of
        Nothing -> state   -- this shouldn't happen.
        Just x -> case mOp of
          Nothing -> CalcState x pushedOp ""
          Just op -> CalcState (runOp op accum x) pushedOp ""

    displayState (CalcState accum _op input) =
      if T.null input
      then T.pack (show accum)
      else input

    debugDisplayState = T.pack . show
```
[Go to snippet](http://localhost:8000/tutorial/10)
