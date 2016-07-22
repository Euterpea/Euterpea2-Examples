Simple GUI/MUI Examples
Donya Quick
Last modified 22-July-2016

This file contains a number of UISF-based GUIs that are 
fairly self-explanitory. Many of them do not produce 
sound and are just to show how individual widgets in the
UISF library work.

To run an example, just type the name of the corresponding 
function into GHCi. For example, there is a MUI defined as 
textboxMUI. To run this, just type textobMUI into GHCi.

> {-# LANGUAGE Arrows #-}
> module Main where
> import Euterpea
> import HSoM
> import FRP.UISF.AuxFunctions


> textboxMUI = runMUI  defaultMUIParams{uiSize=(200,50)} $ proc _ -> do
>     x <- textbox "" -< Nothing -- a text box you can type into
>     returnA -< ()

> textboxMUI2 = runMUI  defaultMUIParams{uiSize=(200,50)} $ proc _ -> do
>     x <- textbox "" -< Nothing -- type into this one...
>     y <- textbox "" <<< unique -< x -- and it updates this one!
>     returnA -< ()

> radioMUI1 = runMUI defaultMUIParams{uiSize=(200,100)} $ proc _ -> do
>     r <- radio ["Dog", "Cat", "Hamster"] 0 -< ()
>     displayStr -< "Current selection: "++show r


> radioMUI2 = runMUI defaultMUIParams{uiSize=(250,100)} $ 
>  let labels = ["Dog", "Cat", "Hamster"] in proc _ -> do
>     r <- radio labels 0 -< ()
>     displayStr -< "Current selection: "++(labels !! r)


> checkboxMUI1 = runMUI defaultMUIParams{uiSize=(200,80)} $ proc _ -> do
>     c <- checkbox "Coffee?" False -< ()
>     displayStr -< if c then "You like coffee"
>                   else "You do not like coffee"

> checkboxMUI2 = runMUI defaultMUIParams{uiSize=(200,100)} $ 
>     let vals = [("One", 1::Int), ("Two", 2), ("Three", 3)] 
>     in  proc _ -> do
>         c <- checkGroup vals -< ()
>         displayStr -< "Sum: " ++ show (sum c)


> listboxMUI1 =  runMUI defaultMUIParams{uiSize=(200,100)} $ proc _ -> do
>     rec x <- listbox [C, D, E, F] 0 -< (Nothing, Nothing)
>         let x' = if x<0 then 0 else x
>     display -< x

> sliderMUI1 = runMUI defaultMUIParams{uiSize=(350,150)} $ proc _ -> do
>     y <- vSlider (0.0, 1.0::Double) 0.5 -< ()
>     x <- hSlider (0.0, 1.0::Double) 0.5 -< ()
>     display -< (x,y)

> sliderMUI2 = runMUI defaultMUIParams{uiSize=(350,150)} $ proc _ -> do
>     y <- viSlider 2 (0, 10::Int) 5 -< ()
>     x <- hiSlider 2 (0, 10::Int) 5 -< ()
>     display -< (x,y)


> displayMUI1 = runMUI defaultMUIParams{uiSize=(200,50)} $ proc _ -> do
>     x <- withDisplay $ hiSlider 2 (0, 10::Int) 5 -< ()
>     returnA -< ()


> holdMUI1 = runMUI defaultMUIParams{uiSize=(200,100)} $ proc _ -> do
>     b1 <- edge <<< button "Button 1" -< ()
>     b2 <- edge <<< button "Button 2" -< ()
>     let b1' = fmap (const "You pressed 1") b1
>         b2' = fmap (const "You pressed 2") b2
>     x <- hold "Nothing pressed" -< mappend b1' b2'
>     displayStr -< x
>     returnA -< ()

> accumMUI1 = runMUI defaultMUIParams{uiSize=(200,100)} $ proc _ -> do
>     b1 <- edge <<< button "Increment" -< ()
>     b2 <- edge <<< button "Reset" -< ()
>     let b1' = fmap (const (+1)) b1
>         b2' = fmap (const (const 0)) b2
>         e = maybe b2' (Just) b1' 
>     x <- accum (0::Int) -< e
>     display -< x
>     returnA -< ()

This version plays a note every time the value changes.

> uniqueMUI1 = runMUI defaultMUIParams{uiSize=(200,200)} $ proc _ -> do
>     d <- selectOutput -< ()
>     b1 <- edge <<< button "Increment" -< ()
>     b2 <- edge <<< button "Reset" -< ()
>     let b1' = fmap (const (+1)) b1
>         b2' = fmap (const (const 0)) b2
>         e = maybe b2' (Just) b1'
>     x <- accum (0::Int) -< e
>     u <- unique -< x
>     display -< x
>     let x' = min (60+x) 127
>     midiOut -< (d, fmap (const [ANote 0 x' 120 0.5]) u)
>     returnA -< ()

> layoutMUI1 = runMUI defaultMUIParams{uiSize=(300,50)} $ proc _ -> do
>     y <- leftRight $ radio (map show [1..5]) 2 -< ()
>     returnA -< ()

This example routs MIDI messages directly from a MIDI input device
to a MIDI output device. You must have one of each in order to use 
this MUI.

> midiInMUI = runMUI defaultMUIParams{uiSize=(300,300)} $ proc _ -> do
>     mi <- selectInput -< ()
>     mo <- selectOutput -< ()
>     mevs <- midiIn -< mi
>     midiOut -< (mo, mevs)


Some computers require compilation to executable to run 
UISF-based GUIs/MUIs. To do this, you need the module name
to be Main (as it is in this module) and to have a function
called "main" like this:

> main = midiInMUI

You can then compile the program as:

ghc -O2 SimpleGUIS.lhs

and run the executable that is produced.
