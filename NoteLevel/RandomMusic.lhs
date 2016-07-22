> module RandomMusic where
> import Euterpea
> import System.Random

Generating music with random numbers
Last modified: 22-July-2016
Donya Quick

This example will illustrate the basics of using random numbers 
to create novel musical structures with Euterpea. First, we'll 
look at how to create infinite sequences of random numbers to use 
as musical "inspiration," and then we'll look at mapping those 
values to things we can hear.

The randInts function below will make an infintie series of random 
Ints from a seed. 

> randInts :: Int -> [Int]
> randInts seed = recInts (mkStdGen seed) where
>     recInts g = let (i,g') = next g in i : recInts g'

This numbers from randInts will be over the entire range of the 
integers, so we will need to take the modulo a base to keep them 
in a more usable range. The function below creates a random 
series of integers within a user-specified range.

> randIntsRange :: (Int, Int) -> Int -> [Int]
> randIntsRange (lower, upper) = 
>     map (\i -> (i `mod` (upper-lower)) + lower) . randInts 

We can use this function to generate random pitches and volumes,
the standard MIDI range for each is 0-127.

The function below will create a random "melody" using a 
specified random number seed, s. Each note will have the 
duration of a sixteenth note (sn). 

> melGen :: Int -> Music (Pitch, Volume)
> melGen s = 
>     let pitches = map pitch $ randIntsRange (30,80) s
>         vols = randIntsRange (40,100) (s+1)
>     in  line $ map (note sn) $ zip pitches vols

Because Euterpea supports infinite playback, we can actually 
listen to one of these melodies indefinitely! Try calling 
the function below from GHCi:

> infinitePlay = play $ melGen 42

Finally we use this function to create three lines in parallel,
each affected by some Control options. To make this piece finite, 
we will use Euterpea's "cut" function to take a fixed number of
measures from the otherwise infinite series that melGen produces.
Various Modify constructors are used to alter the performance of 
each instrument's part: speeding up (Accelerando), slowing down 
(Ritardando), and getting louder (Crescendo). The tempo (Tmp) 
modifiers result in interesting rhythmic textures, such that the 
voices do not play in lock-step.

> somethingWeird = 
>     let part1 = instrument Xylophone $ dim $ rit $ cut 6 $ melGen 345
>         part2 = instrument Marimba $ cut 4 $ melGen 234
>         part3 = instrument TubularBells $ cre $ acc $ cut 8 $ melGen 789
>     in  chord [part1, part2, part3] where
>     rit = Modify (Phrase [Tmp $ Ritardando 0.5])
>     acc = Modify (Phrase [Tmp $ Accelerando 0.5])
>     dim = Modify (Phrase [Dyn $ Diminuendo 0.5])
>     cre = Modify (Phrase [Dyn $ Crescendo 0.5])
