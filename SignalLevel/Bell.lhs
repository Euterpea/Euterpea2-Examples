Bell Instrument
Donya Quick
Last modified: 22-July-2016

> {-# LANGUAGE Arrows #-}
> module Bell where
> import Euterpea

The following instrument is intended to emulate a bell sound.

> sineTable :: Table
> sineTable = tableSinesN 4096 [1]

> bellInstr :: Instr (AudSF () Double)
> bellInstr dur ap vol pfields = 
>   let dur' = fromRational dur 
>       f = apToHz ap
>   in  proc () -> do
>         x1 <- osc sineTable 0 -< f
>         x2 <- osc sineTable 0 -< f*4.1
>         x3 <- osc sineTable 0 -< f*6.05
>         x4 <- osc sineTable 0 -< f*8.2
>         env1 <- envLineSeg [1.0, 0.2, 0, 0] [1, 2, 100] -< ()
>         env2 <- envLineSeg [0, 0.8, 0, 0] [0.05, 2, 100] -< ()
>         env3 <- envLineSeg [0, 0.5, 0, 0] [0.08, 2, 100] -< ()
>         env4 <- envLineSeg [0, 0.3, 0, 0] [0.015, 1, 100] -< ()
>         envx1 <- envLineSeg [0,1,1,0] [0.0001*dur',0.9999*dur', 0.0001*dur'] -< ()
>         envx2 <- envLineSeg [1,0.5,0.2,0,0] [0.05,0.2,3,100] -< ()
>         let envs = envx2
>             partials = ((x1*env1) + (x2*env2) + (x3*env3) + (x4*env4)) / 4
>         outA -< 0.95 * envs * partials

Constructing the InstrMap:

> bellName :: InstrumentName
> bellName = CustomInstrument "Bell Instrument"

> myInstrMap :: InstrMap (AudSF () Double)
> myInstrMap = [(bellName, bellInstr)]

Single note demonstration:

> bellNote = writeWav "bellNote.wav" myInstrMap 
>     (tempo 0.5 $ transpose 12 $ instrument bellName (g 5 wn))

Scale with bells truncated by note duration:


> mel1 = toMusic1 $ line $ map ($en) [c 5, d 5, e 5, f 5, g 5, a 5, b 5, c 6]

> bellMel = writeWav "bellMel.wav" myInstrMap 
>    (tempo 0.5 $ transpose 12 $ instrument bellName mel1) 

Allowed to resonate:

> bellMel2 = writeWav "bellMel2.wav" myInstrMap 
>     (parMod wn $ tempo 0.5 $ transpose 12 $ instrument bellName mel1)

Music modifier to parallelize notes and stretch their lengths by d amount

> parMod d (Prim (Rest d')) = rest d
> parMod d (Prim (Note d' x)) = note (d+d') x
> parMod d (m1 :+: m2) = (parMod d m1) :=: ((rest $ dur m1) :+: parMod d m2)
> parMod d (m1 :=: m2) = parMod d m1 :=: parMod d m2
> parMod d (Modify c m) = Modify c $ parMod d m

