Drum Machine MUI
Donya Quick
Last modified: 22-July-2016

This file demonstrates the creation of a fairly complex MUI that 
generates drum patterns based on selected items in the GUI. The
user can change the drum pattern and increase or decrease the
tempo.

For best results, compile this program as follows:

ghc -O2 DrumMUI.lhs

and then run the resulting executable. The program will also run
in GHCi, but may be a bit laggy/inconsistent on some systems.

> {-# LANGUAGE Arrows #-}
> module Main where
> import Euterpea
> import HSoM
> import FRP.UISF.AuxFunctions

> main = drumMUI

> drumMUI = runMUI defaultMUIParams{uiSize=(500,220), uiTitle="Drum MUI"} drumUI

> drumUI = leftRight $ proc _ -> do
>     devID <- setSize (300,300) $ topDown $ selectOutput -< () -- device selection
>     (dPat, tVal, start, stop) <- rightPane1 -< () -- controls
>     isPlaying <- handleButtons -< (start, stop) -- manage program state
>     handleMIDI -< (start, stop, tVal, dPat, isPlaying, devID) -- produce output

Organizational GUI unit to select drum features:

> rightPane1 = topDown $ proc _ -> do
>     dPat <- title "Drum Pattern" $ radio drumPats 0 -< ()
>     tVal <- withDisplay $ title "Tempo" $ hiSlider 5 (60, 240) 120 -< ()
>     (start,stop) <- (| leftRight ( do
>         s1 <- edge <<< button "Start" -< ()
>         s2 <- edge <<< button "Stop" -< ()
>         returnA -< (s1,s2) ) |)
>     returnA -< (dPat, tVal, start, stop)

> drumPats = ["Kick Only", "Kick + HiHat", "Funky"]

Set the playing state based on which button is clicked:

> handleButtons :: UISF (SEvent a, SEvent b) Bool
> handleButtons = proc (start, stop) -> do
>     rec isPlaying <- delay False -< isPlaying'
>         let isPlaying' = case (start, stop) of
>                              (Just _, _) -> True
>                              (_, Just _) -> False
>                              _ -> isPlaying
>     returnA -< isPlaying

MIDI events are sent out based on a timer, the triggering 
rate of which is proportional to the selected tempo. A counter
keeps track of the position within the measure.

> handleMIDI = proc (start, stop, tVal, dPat, isPlaying, devID) -> do
>     trigger <- timer -< (measureDur $ fromIntegral tVal) / 8
>     beat <- counter 0 7 -< trigger
>     let mVal = makeDrums trigger beat tVal dPat isPlaying
>     status <- midiOutB -< (devID, mVal)
>     returnA -< ()

> counter min max = proc trig -> do
>     rec val <- delay min -< val'
>         let val' = case trig of 
>                 Nothing -> val
>                 Just _ -> if val==max then min else val+1
>     returnA -< val'

> measureDur bpm = 1 / (bpm / (4*60))

MIDI messages are build from Music data structures and converted
to a buffer operation to queue notes for playback.

> makeDrums Nothing beat tVal dPat isPlaying = NoBOp
> makeDrums (Just _) beat tVal dPat isPlaying = 
>     if not isPlaying then NoBOp else
>         let mPat = musicPats !! dPat !! beat 
>             mPat' = tempo (fromIntegral tVal / 120) mPat
>         in  AppendToBuffer $ musicToMsgs' defParams mPat'

Musical patterns:

> musicPats :: [[Music Pitch]]
> musicPats = [
>     kicks1,
>     zipWith (:=:) kicks1 hihats1,
>     zipWith (:=:) kicks2 hihats1
>     ]

> kicks1 = map (instrument Percussion) $ take 8 $ 
>     concat $ repeat [perc BassDrum1 en, rest en]

> hihats1 = map (instrument Percussion) $ take 8 $ 
>     repeat $ perc ClosedHiHat en

> kicks2 = 
>     let b = perc BassDrum1 en
>         s = perc AcousticSnare en
>     in  map (instrument Percussion) 
>         [b, rest en, s, rest en, rest en, b, s, rest en]

