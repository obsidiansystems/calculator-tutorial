# Tutorial
In this example, we'll be following [Luite Stegemann's lead](http://weblog.luite.com/wordpress/?p=127) and building a simple functional reactive calculator to be used in a web browser or as a desktop or mobile app.

### The structure of this document

This document is a [literate haskell](https://wiki.haskell.org/Literate_programming) source file written in markdown.
We're using [markdown-unlit](https://github.com/sol/markdown-unlit#literate-haskell-support-for-markdown) to process this source file and turn it into something our compiler can understand.

### Running the code

You can run this tutorial by:

1. [Installing obelisk](https://github.com/obsidiansystems/obelisk/#installing-obelisk), a framework and development tool for multi-platform Haskell applications.

2. Cloning the calculator-tutorial repository.

    ```bash
    git clone git@github.com:obsidiansystems/calculator-tutorial
    ```

3. Running the application with the `ob` command.

    ```bash
    cd calculator-tutorial
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
{-# LANGUAGE ScopedTypeVariables #-}

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

### Dynamics and Events
Of course, we want to do more than just view a static webpage. Let's start by getting some user input and printing it.

```haskell
tutorial3 :: (DomBuilder t m, PostBuild t m) => m ()
tutorial3 = el "div" $ do
  t <- inputElement def
  text " "
  dynText $ _inputElement_value t

```

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
  text " "
  dynText $ _inputElement_value t
```

The code above overrides some of the default values of the `InputElementConfig`. We provide a `Map Text Text` value for the `inputElementConfig_elementConfig`'s `elementConfig_initialAttributes`, specifying the html input element's `type` attribute to `number`.

Next, we override the default initial value of the `InputElement`. We gave it `"0"`. Even though we're making an html `input` element with the attribute `type=number`, the result is still a `Text`. We'll convert this later.

Let's do more than just take the input value and print it out. First, let's make sure the input is actually a number:

```haskell
tutorial5 :: (DomBuilder t m, PostBuild t m) => m ()
tutorial5 = el "div" $ do
  x <- numberInput
  let numberString = fmap (pack . show) x
  text " "
  dynText numberString
  where
    numberInput :: DomBuilder t m => m (Dynamic t (Maybe Double))
    numberInput = do
      n <- inputElement $ def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
      return . fmap (readMaybe . unpack) $ _inputElement_value n
```

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

`numberInput` hasn't changed here. Our `main` function now creates two inputs. `zipDynWith` is used to produce the actual sum of the values of the inputs. The type signature of `zipDynWith` is:

```
Reflex t => (a -> b -> c) -> Dynamic t a -> Dynamic t b -> Dynamic t c
```

You can see that it takes a function that combines two pure values and produces some other pure value, and two `Dynamic`s, and produces a `Dynamic`.

In our case, `zipDynWith` is combining the results of our two `numberInput`s (with a little help from `Control.Applicative`) into a sum.

We use `fmap` again to apply `pack . show` to `result` (a `Dynamic (Maybe Double)`) resulting in a `Dynamic Text`. This `resultText` is then displayed using `dynText`.

### Supporting Multiple Operations
Next, we'll add support for other operations. We're going to add a dropdown so that the user can select the operation to apply.  Our supported operations will be:

```haskell
data Op = Plus | Minus | Times | Divide
  deriving (Eq, Ord, Show)

runOp :: Fractional a => Op -> a -> a -> a
runOp s = case s of
  Plus -> (+)
  Minus -> (-)
  Times -> (*)
  Divide -> (/)

ops :: Map Op Text
ops = Map.fromList [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/")]
```

We also want a nice simple way of interpreting these operations,  and some kind of string representation to display.  We will reuse all of these definitions in all of the remaining examples.

Here is our program:

```haskell
tutorial7 :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
tutorial7 = el "div" $ do
  nx <- numberInput
  op <- _dropdown_value <$> dropdown Times (constDyn ops) def
  ny <- numberInput
  let values = zipDynWith (,) nx ny
      result = zipDynWith (\o (x,y) -> runOp o <$> x <*> y) op values
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
```

We've covered `numberInput` in our last tutorial; the first new function is `dropdown`, which has type:

```
dropdown :: (MonadWidget t m, Ord k) => k -> Dynamic t (Map k Text) -> DropdownConfig t k -> m (Dropdown t k)
```

The first argument is the initial value of the `Dropdown`,  which in our case is of type `Op`. The second argument is a `Dynamic (Map Op Text)` that represents the options in the dropdown. The `Text` values of the `Map` are the strings that will be displayed to the user. If the initial key is not in the `Map`, it is added and given a `Text` value of `""`. The final argument is a `DropdownConfig`.We are using `dropdown` on the following line:

```
op <- _dropdown_value <$> dropdown Times (constDyn ops) def
```

This particular `reflex-dom` widget allows us to dynamically update the list of options.  We don't need this,  so we use `constDyn` to turn `ops` into a dynamic behavior that never changes.  Finally, we use `def` to provide a sensible default configuration.   In our case, the return type of `dropdown` in our case is `m (Dropdown t Op)`,  and we use the accessor `_dropdown_value` to fetch the `Dynamic Op` that represents the operation currently held in the input box.

Our use of `zipDynWith` is very similar to the last tutorial;  but instead of aggregating two `Dynamic`s, now we need to aggregate three.  So we call `zipDynWith` twice.

```
let values = zipDynWith (,) nx ny
    result = zipDynWith (\o (x,y) -> runOp o <$> x <*> y) op values
```

The first call,  we aggregate the state of the two `numberInputs`, `nx` and `ny`, into a pair of numbers.  The result is of type `Dynamic (Maybe Double, Maybe Double)`, which is then bound to `values`.


Next, we call `zipDynWith` again, combining `values` with the selected operation `op`. Now, instead of applying `(+)` to our `Double` values, we use `runOp` to select an operation based on the `Dynamic` value of our `Dropdown`.

Running the app at this point will give us our two number inputs with a dropdown of operations sandwiched between them. Multiplication should be pre-selected when the page loads.

### Events and State Machines

While the previous examples are a nice introduction to producing spreadsheet-style interactions,  sometimes you'll want to use events to update a state machine;  a reasonably faithful implementation of a traditional four function calculator is a fairly natural example.

In the next three examples, we'll use `accumDyn` to collect button presses and use them to update a widget's state.   This function is a close analogy to `foldl`:  it takes a pure function describing the state changes,  an initial state, and a `Event` stream,  and returns a `Dynamic` behavior.

```
accumDyn :: (Reflex t, MonadHold t m, MonadFix m)
         => (a -> b -> a) -> a -> Event t b -> m (Dynamic t a)
```

So, let's get started!

### Number Pad

As a slimmed down example, we'll start with a number pad that allows you to type in numbers, and clear them.  We'll start with a numeric keypad:

```haskell
buttonClass :: DomBuilder t m => Text -> Text -> m (Event t ())
buttonClass c s = do
  (e, _) <- elAttr' "button" ("type" =: "button" <> "class" =: c) $ text s
  return $ domEvent Click e

numberPad :: (DomBuilder t m) => m (Event t Text)
numberPad = do
  b7 <- ("7" <$) <$> numberButton "7"
  b8 <- ("8" <$) <$> numberButton "8"
  b9 <- ("9" <$) <$> numberButton "9"
  b4 <- ("4" <$) <$> numberButton "4"
  b5 <- ("5" <$) <$> numberButton "5"
  b6 <- ("6" <$) <$> numberButton "6"
  b1 <- ("1" <$) <$> numberButton "1"
  b2 <- ("2" <$) <$> numberButton "2"
  b3 <- ("3" <$) <$> numberButton "3"
  b0 <- ("0" <$) <$> buttonClass "number zero" "0"
  return $ leftmost [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9]
  where
    numberButton n = buttonClass "number" n
```

This definition will come in handy for the rest of the tutorial examples. Here, `button` is a reflex-dom default widget,  which isn't affected by the surrounding `divClass`:

```
button :: DomBuilder t m => Text -> m (Event t ())
```

For each button click, the event simply returns `()`.  However, we want to easily distinguish the buttons from each other, so we turn the result into an `m (Event t Text)` using two layers of fmap.  So, in the `b0` line, we use `<$>` to apply `("0" <$)` to the `Event` inside `m`,  then `<$` replaces the `()` inside the `Event` with `"0"`.

Finally we funnel all these (now distinct) events into one `Event` channel, using `leftmost`,  which is of type:

```
leftmost :: Reflex t => [Event t a] -> Event t a
```

Here's the complete code:

```haskell
tutorial8 :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
tutorial8 = el "div" $ do
  numberButton <- numberPad
  clearButton <- button "C"
  let buttons = leftmost
        [ Nothing <$ clearButton
        , Just <$> numberButton
        ]
  dstate <- accumDyn collectButtonPresses initialState buttons
  text " "
  dynText dstate
  where
    initialState :: Text
    initialState = T.empty

    collectButtonPresses :: Text -> Maybe Text -> Text
    collectButtonPresses state buttonPress =
      case buttonPress of
        Nothing -> initialState
        Just digit -> state <> digit
```


So `tutorial8` starts by tacking on a button to clear the input.   We then aggregate the events coming from the numberpad with the events coming from the clear button, by marking all the numberpad events with a `Just` constructor and using `Nothing` to represent the clear button.

We use `accumDyn` to observe button presses and update our widget's state.  Our widget's state is very simple:  it's just `Text`.  The state transition function is also quite simple:  we simply check to see if the clear button was pressed,  and if not, append the Text returned by our `numberPad`.

There are significant potential benefits to testing, reliability and security if you can specify your widget's state transition as a pure function, which we do here.  It provides a high degree of assurance that the transition function does not have complicated interactions with other parts of the system,  and makes it easier to check that part of the logic using say, quickcheck, an SMT solver, model checker, proof assistant, or other formal techniques.  However, if you need it, there is `accumMDyn` which allows the transition function to exhibit certain other effects.

### A Minimal Four Function Calculator

For a simple four-function calculator,  we basically just take the previous example and do a lot more of it.  Our widget's state becomes much more complex: in addition to the input,  we have an accumulator which we display when then the input is empty, and we keep track of the most recently requested operation.  Then we have to project the widget state down  `Dynamic` behaviors:  one to determine the `Text` representing the number to display on the screen,  and some dynamic attributes to indicate the selected operation.   While the state transition function is significantly more complicated, it's still a pure function:  as far as Reflex is concerned, it's really no different than the previous implementation.

```haskell
data CalcState = CalcState
  { _calcState_acc   :: Double     -- accumulator
  , _calcState_op    :: Maybe Op   -- most recently requested operation
  , _calcState_input :: Text       -- current input
  } deriving (Show)

data Button
  = ButtonNumber Text
  | ButtonOp Op
  | ButtonEq
  | ButtonClear

initCalcState :: CalcState
initCalcState = CalcState 0 Nothing ""

updateCalcState :: CalcState -> Button -> CalcState
updateCalcState state@(CalcState acc mOp input) btn =
  case btn of
    ButtonNumber d ->
      if d == "." && T.find (== '.') input /= Nothing
      then state
      else CalcState acc mOp (input <> d)
    ButtonOp pushedOp -> applyOp state (Just pushedOp)
    ButtonEq -> applyOp state Nothing
    ButtonClear -> initCalcState

applyOp :: CalcState -> Maybe Op -> CalcState
applyOp state@(CalcState acc mOp input) mOp' =
  if T.null input
  then
    CalcState acc mOp' input
  else
    case readMaybe (unpack input) of
      Nothing -> state
      Just x -> case mOp of
        Nothing -> CalcState x mOp' ""
        Just op -> CalcState (runOp op acc x) mOp' ""

displayCalcState :: CalcState -> Text
displayCalcState (CalcState acc _op input) =
  if T.null input
  then T.pack (show acc)
  else input
```

A more significant difference from Reflex's perspective is that the number pad displayed the application state directly:  as the state was already `Text`, we could pass it directly to `dynText`.   However,  this time the application state is of type `CalcState`.    We use `displayCalcState` to determine what text should be displayed;  we either display the current input,  or the accumulator if we have no input.  In order to apply `displayCalcState` to the result of `accumDyn`,  we use `Dynamic`'s instance of `Functor` via `<$>`.

```haskell
tutorial9 :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
tutorial9 = el "div" $ do
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
  calcState <- accumDyn updateCalcState initCalcState buttons
  text " "
  dynText (displayCalcState <$> calcState)
```

### Dynamic Attributes and Cyclic Dependencies

For our final example,  we will go beyond the limitations of a traditional four-function calculator,  whose feedback was usually limited to a single-row 7-segment display.   We will indicate the selected operation by dynamically changing the background color of the button,  and deal with cyclic dependencies using the recursive do notation.  Instead of collapsing the accumulator and input registers, we'll simply display them both,  as if we had a 2-line 7 segment display.  And in doing so, the widget's entire state is visually presented to the user; nothing is hidden.

Note, by using recursive do notation, we can reorder the declarations in any way that we see fit:  doing so usually reorders HTML tags,  but will not change the dynamic relationship between widgets.  Thus, it's a mistake to read a Reflex program in an overly imperative way;  instead you are declaring behaviors and scoping how they interact with other parts of the system.

```haskell
tutorial10 :: forall t m. (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
tutorial10 = el "div" $ do
  rec
    numberButtons <- numberPad
    bPeriod <- ("." <$) <$> button "."
    let opState = _calcState_op <$> calcState
    bPlus <- opButton Plus "+" opState
    bMinus <- opButton Minus "-" opState
    bTimes <- opButton Times "*" opState
    bDivide <- opButton Divide "/" opState
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
    calcState <- accumDyn updateCalcState initCalcState buttons
    text " "
    dynText (T.pack . show . _calcState_acc <$> calcState)
    el "br" blank
    dynText (_calcState_input <$> calcState)
  return ()
  where
    opButton :: Op -> Text -> Dynamic t (Maybe Op) -> m (Event t Op)
    opButton op label selectedOp = do
      (e, _) <- elDynAttr' "button" (pickColor <$> selectedOp) $ text label
      return (op <$ domEvent Click e)
      where
        pickColor mOp =
          if Just op == mOp
          then "style" =: "color: red"
          else Map.empty
```


For the final example,  we'll add some extra HTML `div`s and add `class` attributes to a variety of tags in order to make styling the output easier:

```haskell
tutorial11 :: forall t m. (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
tutorial11 = divClass "calculator" $ do
  rec
    divClass "output" $ dynText $ displayCalcState <$> calcState
    buttons <- divClass "input" $ do
      (numberButtons, bPeriod) <- divClass "number-pad" $ do
        numberButtons <- numberPad
        bPeriod <- ("." <$) <$> buttonClass "number" "."
        return (numberButtons, bPeriod)
      (opButtons, bEq) <- divClass "ops-pad" $ do
        let opState = _calcState_op <$> calcState
        bPlus <- opButton Plus "+" opState
        bMinus <- opButton Minus "-" opState
        bTimes <- opButton Times "*" opState
        bDivide <- opButton Divide "/" opState
        let opButtons = leftmost [bPlus, bMinus, bTimes, bDivide]
        bEq <- buttonClass "primary" "="
        return (opButtons, bEq)
      bClear <- divClass "other-pad" $ do
        bClear <- buttonClass "secondary" "C"
        _ <- buttonClass "secondary" "+/-"
        _ <- buttonClass "secondary" "%"
        return bClear
      let buttons = leftmost
            [ ButtonNumber <$> numberButtons
            , ButtonNumber <$> bPeriod
            , ButtonOp <$> opButtons
            , ButtonEq <$ bEq
            , ButtonClear <$ bClear
            ]
      return buttons
    calcState <- accumDyn updateCalcState initCalcState buttons
  return ()
  where
    opButton :: Op -> Text -> Dynamic t (Maybe Op) -> m (Event t Op)
    opButton op label selectedOp = do
      (e, _) <- elDynAttr' "button" (("class" =: "primary" <>) <$> (pickColor <$> selectedOp)) $ text label
      return (op <$ domEvent Click e)
      where
        pickColor mOp =
          if Just op == mOp
          then "style" =: "background: lightblue"
          else Map.empty
```

### In Summary

Congratulations! You’ve written a simple functional reactive calculator with Reflex!

We hope that you now feel confident in your understanding of Reflex, and are ready to begin working on your own project.

If you have additional time or just want to continue practicing your new Reflex skills, here are some ideas for improvements that you could make to the calculator:

1. Implement the `+/-` and `%` buttons.

2. Implement "repeat the last operation" when the `=` button is repeatedly pushed.

3. Turn this app into a naive iOS or Android application.

Again, nice work on making it to the end of this tutorial, if you have any additional questions, check out our resources page.

Happy Coding!
