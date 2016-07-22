Basic Sine Wave Instrument Example
Donya Quick
Last Modified: 22-July-2016

> {-# LANGUAGE Arrows #-}
> module SineDemos where
> import Euterpea

Here we will look at a wave table approach to making 
a simple sine wave instrument. Using a pre-computed 
table of samples is faster than trying to compute the 
sine wave function at each time step during signal 
generation. 

First, we will construct a table of 4096 sine wave 
values. Euterpea has a built-in function for this. 
It captures one cyce of the sine function. When 
repeated at a certain rate, it will sound like a 
continuous sine wave.

> sineTable :: Table
> sineTable = tableSinesN 4096 [1]


We can use this table directly without going through 
Euterpea's instrument features. The code below shows 
how to do this with the basic oscillator (osc) to 
use the table we defined previously. We will write 
2 seconds of a sine wave.

> sine440 :: AudSF () Double
> sine440 = proc _ -> do
>     y <- osc sineTable 0 -< 440
>     returnA -< y

> testSine440 :: IO ()
> testSine440 = outFile "sine440.wav" 2.0 sine440

Now, let's use Euterpea's Instr type to create a 
simple sine wave instrument that we can use to play 
melodies written with Euterpea's Music data type.

To do this, we will use apToHz to convert an AbsPitch
value to a frequency in Hz. Then, we use essentially 
the same code as before to generate the signal at 
this frequency.

> sineInstr :: Instr (Mono AudRate)
> sineInstr dur pch vol params = 
>     let freq = apToHz pch 
>     in  proc _ -> do
>         y <- osc sineTable 0 -< freq
>         returnA -< y

To use this instrument, we need to give it a name and
then create an entry in an InstrMap lookup table. We're 
actually going to make two sine wave instruments, so
there will be two entries. We'll get to what the second
instrument is later on.

> sineName, sineName2 :: InstrumentName
> sineName = CustomInstrument "Sine"
> sineName2 = CustomInstrument "Sine2"

> instrMap :: InstrMap (Mono AudRate)
> instrMap = [(sineName, sineInstr), (sineName2, sineInstr2)]

Now we can define a Music structure and hear it with the 
sinewave instrument. Run the unction "testMyMel" below to 
generate a wave file using the sine wave instrument!

> myMel = instrument sineName $ 
>     line [c 4 qn, e 4 qn, g 4 qn, c 5 qn]

> testMyMel = writeWav "mymel.wav" instrMap myMel

You may notice that the transitions between notes have 
clicks. This is because the sine wave stops in its tracks 
as soon as a note ends, and then begins anew for the next 
note, creating a large vertical jump in the wave form. The 
general strategy to avoid this is to create an envelope 
based on the note's duration that slowly scales the amplitude
up and down. If done quickly, it makes the transitions sound 
smooth. We can use Euterpea's envLineSeg to do this. 

> sineInstr2 :: Instr (Mono AudRate)
> sineInstr2 dur pch vol params = 
>     let freq = apToHz pch 
>         d = fromRational dur
>     in  proc _ -> do
>         y <- osc sineTable 0 -< freq
>         e <- envLineSeg [0,1,1,0] [0.01*d, 0.94*d, 0.05*d] -< ()
>         returnA -< y*e

Now, we can listen to the same melody using the new instrument:

> myMel2 = instrument sineName2 $ 
>     line [c 4 qn, e 4 qn, g 4 qn, c 5 qn]

> testMyMel2 = writeWav "mymel2.wav" instrMap myMel2