Real-time MIDI I/O with Euterpea.
Donya Quick
Last modified: 24-Feb-2020

This program has the following MIDI I/O features:
- Receiving input from a MIDI device (like a user-played MIDI controller)
- Echoing input MIDI events to an output MIDI device
- Sending timed, generated Euterpea Music values to the same MIDI output device.

The generated music in this case is a simple C-major arppegio that will  
accompany whatever the user plays via the MIDI input device.

The program continues indefinitely until the user exits via Ctrl+C. You may 
need to press this more than once depending on the system.

This implementation uses only Euterpea and its dependencies (PortMidi) to 
handle MIDI I/O (it does not use HSoM or MUIs). The general framework it 
presents is suitable for being adapted into a real-time accompaniment 
system that generates Euterpea Music values provided that the generative 
functions are sufficiently fast.

On Mac, the timing of generative output should appear sufficiently steady with
most synthesizers, includig SimpleSynth.
 
On Windows, it will also be steady if using an ASIO-based sound device and 
synthesizer. However, using the default GS synth on Windows, there will both 
be lag responding to user input and an initial timing irregularity with the 
first note piped out. This is not a fault of the program - it's a result of 
the high latency associated with the default Windows synth. Windows users 
should use ASIO-based synths if possible, and if not should seek a different 
synthesizer like Coolsoft VirualMidiSynth that permits some degree of control
over its latency. 

Within GHCi, load this file and then use 'devices' to check your MIDI input 
and output device numbers. Then run the program using 'mainLoop' as follows:

mainLoop inDevNum outDevNum

Mac compilation and execution from Terminal:
    ghc -O2 RealTimeMidiIO.lhs -o Main
    ./Main inDevNum outDevNum
    (ex: Main 0 1)
    
Windows compilation and execution from PowerShell
    ghc -O2 RealTimeMidiIO.lhs -o Main.exe
    ./Main.exe inDevNum outDevNum
    (ex: ./Main.exe 0 1)

> module Main where
> import Euterpea
> import Euterpea.IO.MIDI.MidiIO
> import System.Exit
> import System.IO
> import Control.Exception
> import Data.List
> import System.Environment
> import Control.Concurrent
> import Control.Concurrent.STM
> import Data.Time
> import Data.Time.Clock.POSIX
> import System.Random
> import System.Exit 

> main = do
>  hSetBuffering stdout NoBuffering
>  hSetBuffering stdin NoBuffering
>  hFlush stdout
>  args <- getArgs
>  let inDev = read (args !! 0)
>      outDev = read (args !! 1)
>  mainLoop inDev outDev

> mainLoop inDev outDev = do
>     inBuf <- newTVarIO [] -- MIDI buffer for user input 
>     genBuf <- newTVarIO [] -- MIDI buffer for generated values
>     stopSig <- newTVarIO False -- stop signal
>     putStrLn ("Clearing MIDI Devices...")
>     wait 0.5
>     handleCtrlC stopSig $ do
>         putStrLn ("Initializing MIDI Devices...")
>         initializeMidi
>         forkIO (midiInRec (unsafeInputID inDev) inBuf stopSig) -- poll input and add to buffer
>         forkIO (midiOutRec 0 (unsafeOutputID outDev) inBuf genBuf stopSig) -- take from buffer and output
>         forkIO (genRec genBuf stopSig) -- a generative placeholder
>         putStrLn ("MIDI I/O services started.")
>         detectExitLoop stopSig -- should only exit this via handleCtrlC
>         terminateMidi -- in case the recursion ends by some irregular means
>         exitFailure where -- in case the recursion ends by some irregular means
>         handleCtrlC stopSig op = onException op f where
>             f = do
>                 atomically $ writeTVar stopSig True -- signal the other threads to stop
>                 putStrLn "Stopping MIDI devices" -- not clear why Ctrl+C is needed again
>                 wait 1.0 -- give the other threads time to stop before closing down MIDI!
>                 terminateMidi -- close down MIDI
>                 wait 0.5 -- give MIDI time to close down
>                 putStrLn "Done. Bye!"
>                 exitSuccess 
>     

Some music that we'll repeat. Think of this as a placeholder for a backing 
track or some other computer accompaniment.

> someMusic :: Music AbsPitch
> someMusic = line $ map (note qn) [60,64,67,64]

Now we send this to the MIDI out indefinitely, but only adding to the 
buffer on an as-needed basis. This is necessary if we were to have a computer 
response that adapts to the user's input in some way over time.

> genRec genBuf stopSig = do
>     wait 0.05 -- we're generating a measure at a time; don't need to regen very often
>     stopNow <- atomically $ readTVar stopSig
>     if stopNow then return () else do
>         buf <- atomically $ readTVar genBuf -- what's left in the buffer?
>         let newMidiMsgs = musicToMsgs' defParams $ someMusic
>         if bufAmtGT buf 0.5 then return () else do -- if low buffer, add to it
>             putStrLn "Adding music to buffer."
>             atomically $ writeTVar genBuf (buf ++ newMidiMsgs)
>     genRec genBuf stopSig where
>     bufAmtGT :: [(Time,a)] -> Time -> Bool
>     bufAmtGT [] tAmt = False
>     bufAmtGT ((t,x):txs) tAmt = if t>tAmt then True else bufAmtGT txs (tAmt-t)

This function is just to delay main until the user has pressed Ctrl+C.

> detectExitLoop stopSignal = do
>     wait 0.25 -- we only need to check for stopping periodically
>     stopNow <- atomically $ readTVar stopSignal
>     if stopNow then return () else detectExitLoop stopSignal 

MIDI input is received by repeatedly polling the MIDI input device. 
Importantly, this can't be done too fast. Trying to do it as fast as the 
program can possibly execute will actually result in lag.

> midiInRec :: InputDeviceID -> TVar [(Time, MidiMessage)] -> TVar Bool -> IO ()
> midiInRec inDev inBuf stopSignal = do
>     wait 0.01 -- must throttle! Otherwise we get lag and may overwhelm MIDI devices.
>     stopNow <- atomically $ readTVar stopSignal
>     if stopNow then return () else do
>         let g Nothing = []
>             g (Just (t,ms)) = map (\m -> (0, Std $ m)) ms
>         msgs <- sequence $ map getMidiInput [inDev] -- get MIDI messages coming
>         let outVal = concatMap g msgs
>         if null outVal then return () else print ("User input: "++show outVal)
>         if null outVal then return () else atomically $ addMsgs inBuf outVal
>         midiInRec inDev inBuf stopSignal

MIDI output is done by checking the output buffer and sending messages as 
needed based on the current time. We use a TVar for this to communicate 
information between the input and output threads. 

> midiOutRec :: Double -> OutputDeviceID -> TVar [(Time, MidiMessage)] -> TVar [(Time, MidiMessage)] -> TVar Bool -> IO ()
> midiOutRec lastMsgTime outDev inBuf genBuf stopSig = do
>     wait 0.001 -- must throttle! Otherwise we get lag and may overwhelm MIDI devices.
>     currT <- getPOSIXTime -- get the current time
>     let currT' = posixFix currT
>         tEllapsed = currT' - lastMsgTime
>     stopNow <- atomically $ readTVar stopSig
>     if stopNow then return () else do
>         outVal1 <- atomically $ getClearMsgs inBuf -- fetch user input
>         outVal2 <- atomically $ getUpdateMsgs genBuf tEllapsed -- fetch generated music
>         let newMsgTime = if null outVal2 then lastMsgTime else  currT'
>         sendMidiOut outDev (outVal1++outVal2) -- send out to MIDI device
>         midiOutRec newMsgTime outDev inBuf genBuf stopSig where
>     posixFix :: NominalDiffTime -> Double
>     posixFix x = fromIntegral (round(x * 1000)) / 1000

====== Just utility functions below this point ======

Just add values to the end of the buffer:

> addMsgs :: TVar [a] -> [a] -> STM ()
> addMsgs v [] = return () 
> addMsgs v xs = do
>     x <- readTVar v
>     let newVal = x ++ xs
>     writeTVar v newVal 

Clear the buffer after reading it:

> getClearMsgs :: TVar [a] -> STM [a]
> getClearMsgs v = do 
>     x <- readTVar v -- what's in the buffer?
>     case x of [] -> return [] -- it's empty, so do nothing
>               _ -> do -- it has values!
>                   writeTVar v [] -- empty the TVar buffer
>                   return x -- return the values it had

Read and return/clear events that are ready to be sent to a MIDI device.

> getUpdateMsgs :: TVar [(Time, a)] -> Time -> STM [(Time, a)]
> getUpdateMsgs v tElapsed = do
>     ms <- readTVar v -- what's in the buffer?
>     case ms of
>         [] -> return [] -- nothing, so do nothing
>         ((t,x):txs) -> -- it has values!
>             let simul = takeWhile ((<=0).fst) txs -- check for 0-timestamped messages
>             in  if t <= tElapsed then do -- is it time to send something?
>                     writeTVar v (drop (length simul) txs) -- yes, update buffer and then...
>                     return ((0,x) : simul) -- return what we removed from the buffer
>                 else do -- not time yet
>                     return []

Utilities to interface with lower-level MIDI functions

> sendMidiOut :: OutputDeviceID -> [(Time, MidiMessage)] -> IO ()
> sendMidiOut dev [] = return ()
> sendMidiOut dev ms = outputMidi dev >> (mapM_ (\(t,m) -> deliverMidiEvent dev (0, m))) ms

> getMidiInput :: InputDeviceID -> IO (Maybe (Time, [Message])) -- Codec.Midi message format
> getMidiInput dev = pollMidi dev

Thread delay utility:

> type Seconds = Double
> wait :: Seconds -> IO () 
> wait s = threadDelay $ round $ s * 1000000
