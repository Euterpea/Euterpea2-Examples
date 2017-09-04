Interactive Stochastic Music
Donya Quick
A Euterpea/HSoM port of a JythonMusic program I wrote a couple years back.

> {-# LANGUAGE Arrows #-}
> module Main where
> import Euterpea
> import HSoM
> import FRP.UISF
> import System.Random

> main :: IO()
> main = runMUI defaultMUIParams{uiSize=(300,500), uiTickDelay=0} interactive

> interactive :: UISF () ()
> interactive = proc _ -> do
>     mIn <- selectInput -< ()
>     mOut <- selectOutput -< ()
>     playOut <- stickyButton "Play" -< ()
>     label "" -< () -- to make some space on the screen
>     rate <- title "Rate (Seconds)" $ withDisplay $ hSlider (0.1, 1.5) 0.2 -< ()
>     bSize <- title "Buffer Size" $ withDisplay $ hiSlider 1 (1,12) 5 -< ()
>     tick <- timer -< rate
>     mi <- midiIn -< mIn
>     rec lastPlay <- delay False -< playOut -- to catch T-F edge and turn notes off when it happens
>         lastP <- delay 60 -< nextP -- for correct ordering of on-off messages
>         (pBuffer, vBuffer) <- delay ([],[]) -< procBuffer (pBuffer, vBuffer) bSize mi
>         title "Pitches" $ display -< pBuffer
>         title "Volumes" $ display -< vBuffer
>         let (pbSize, vbSize) = (length pBuffer, length vBuffer)
>         r1 <- liftAIO randomRIO -< (0::Int, pbSize-1) -- random pitch index
>         r2 <- liftAIO randomRIO -< (0::Int, vbSize-1) -- random volume index
>         let nextP = if pbSize <= 0 then 60 else pBuffer !! r1 -- select from buffer if not empty
>             nextV = if vbSize <= 0 then 80 else vBuffer !! r2
>     let outMsgs = if not playOut && lastPlay then Just [Std(ControlChange 0 123 0)] else 
>                   if playOut && tick/=Nothing then Just [Std (NoteOff 0 lastP 0), Std (NoteOn 0 nextP nextV)] else Nothing
>     midiOut -< (mOut, outMsgs) 
>     returnA -< () 

> procBuffer :: ([AbsPitch], [Volume]) -> Int -> Maybe [MidiMessage] -> ([AbsPitch], [Volume])
> procBuffer (pb, vb) s ms = -- extract pitches and volumes from MIDI message lists
>     let (ps, vs) = unzip $ getPVs ms
>     in  (take s (ps ++ pb), take s (vs++vb)) where
>     getPVs Nothing = []
>     getPVs (Just ms) = f ms where
>         f (ANote c k v d : t) = (k,v) : f t
>         f (Std (NoteOn c k v) : t) = (k,v) : f t
>         f (h:t) = f t
>         f [] = []