A Simple Wind Generator
Donya Quick
October 13, 2019

This program is best used after compilation with ghc rather than from 
within GHCi. When compiled, the program can take a while if you ask it
to generate more than a few seconds of audio. The more audio you ask it
to generate, the longer it will take. Because the wind sound evolves 
slowly over time, I recommend trying it with at least 60 seconds of 
output to get a good idea of how it sounds. Note that the results will 
be different each time you run the program. The random seed used will 
be printed to the console if you want to save it and use it again. 
The volume can be rather quiet depending on the seed. Open the output
in a wave editor (like Audacity) to amplify the output if desired.

To compile the program, use the following command:

ghc -O2 WindEffect.lhs

On Mac, run the program with:

./WindEffect seconds outFileName [seed]

The last argument is optional. For example, doing this:

./WindEffect 10.0 wind10.wav

will generate 10 seconds of output and the program will use its own 
random seed. To generate with a seed specified, you can supply an 
integer as the seed:

./WindEffect 10.0 wind10.wav 1234

On Windows, use one of the following formats to invoke the program
(depending on if you use cmd or powershell):

WindEffect.exe seconds outFileName [seed]
.\WindEffect.exe seconds outFileName [seed]

> {-# LANGUAGE Arrows #-}
> module Main where
> import Euterpea
> import System.Random
> import System.Environment

First, a function to create an infinite list of random points, where each one
is at most maxDelta away from the previous point (higher or lower). The values
are bounded between limitL and limitU. 

> randomPoints :: StdGen -> Double -> Double -> Double -> Double -> [Double]
> randomPoints g lastY maxDelta limitL limitU = 
>     let (r,g') = randomR (-maxDelta, maxDelta) g -- generate an amount to move
>         nextY = min limitU (max (lastY+r) limitL) -- stay within lower/upper bounds
>     in  lastY : randomPoints g' nextY maxDelta limitL limitU where

Now we effectively wrap this function to make it work at the signal level. Points 
will fill secs number of seconds and be placed according to spacing (number of 
seconds between points). 

> randEnv :: Double -> Double -> Double -> Double -> Double -> Double -> StdGen -> AudSF () Double
> randEnv secs spacing y0 maxDelta limitL limitU g = 
>     let n = ceiling (secs / spacing) -- how many envelope points do we need? (Must be finite)
>         ys = take n $ randomPoints g y0 maxDelta limitL limitU 
>         xs = take (n-1) $ repeat spacing
>     in  proc () -> do
>         y <- envLineSeg ys xs -< ()
>         returnA -< y

Utility function to split a random generator.

> splitN :: StdGen -> [StdGen]
> splitN g = let (g1,g2) = split g in g1 : splitN g2

> wind :: Double -> StdGen -> AudSF () Double
> wind secs g = let gs = splitN g in proc _ -> do
>     n    <- noiseWhite (fst $ next (gs !! 0)) -< () -- white noise source
>     vEnv <- randEnv secs 0.05 0    0.1  (0.1)  1.0  (gs !! 2) -< () -- volume envelope
>     bpF  <- randEnv secs 0.05 2000 50   100    5000 (gs !! 3) -< () -- bandpass center frequency 
>     bw   <- randEnv secs 0.05 50   5    5      100  (gs !! 4) -< () -- bandpass bandwidth 
>     nBP  <- filterBandPassBW -< (n, bpF, bw)  -- filtered noise
>     returnA -< vEnv * nBP 

Since the wind function needs to know the amount of seconds being
generated, we will wrap outFile to provide seconds in all the places
it's needed.

> genWind :: StdGen -> Double -> FilePath  -> IO ()
> genWind g seconds filename = outFile filename seconds (wind seconds g)

Finally, the main function will write 30 seconds of sound to wind.wav 
if no arguments are given. If compiled and called from command-line, the
program takes three arguments: the number of seconds (a double), the 
name of the file to write (make sure it ends with .wav), and the random
seed to use. The third, random seed argument is optional. If none is 
supplied then the program will seed itself.

> main = do
>   args <- getArgs
>   rio <- randomIO
>   let r = if length args == 3 then read (args !! 2) else rio
>       g = mkStdGen r
>   putStr ("Random seed: "++show r)
>   if length args >= 2 then do
>       let secs = read (args !! 0) :: Double
>       genWind g secs (args !! 1)
>   else genWind g 30.0 "wind.wav"