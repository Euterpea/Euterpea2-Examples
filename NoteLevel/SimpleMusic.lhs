Simple examples with Euterpea's Music data structures
Donya Quick
Last modified: 22-July-2016

> module SimpleMusic where
> import Euterpea

This module gives examples of constructing small fragments
of music with Euterpea's Music data structures. At the end, 
there is also an example of pattern matching against these 
data structures.

First, we will start with several ways to build the same 
melody with different syntax and functions from Euterpea:

> mel1, mel2, mel3, mel4, mel5 :: Music Pitch

> mel1 = Prim (Note hn (C,4)) :+: Prim (Note hn (E,4)) :+:
>     Prim (Note hn (G,4)) :+: Prim (Note hn (C,5))

> mel2 = note qn (C,4) :+: note qn (E,4) :+: 
>     note qn (G,4) :+: note qn (C,5)

> mel3 = c 4 qn :+: e 4 qn :+: g 4 qn :+: c 5 qn

> mel4 = line [c 4 qn, e 4 qn, g 4 qn, c 5 qn]

> mel5 = line $ map ($qn) [c 4, e 4, g 4, c 5]
 

We can do similar things with chords:

> chord1, chord2, chord3, chord4, chord5 :: Music Pitch

> chord1 = Prim (Note hn (C,4)) :=: Prim (Note hn (E,4)) :=:
>     Prim (Note hn (G,4)) :=: Prim (Note hn (C,5))

> chord2 = note qn (C,4) :=: note qn (E,4) :=: 
>     note qn (G,4) :=: note qn (C,5)

> chord3 = c 4 qn :=: e 4 qn :=: g 4 qn :=: c 5 qn

> chord4 = chord [c 4 qn, e 4 qn, g 4 qn, c 5 qn]

> chord5 = chord $ map ($qn) [c 4, e 4, g 4, c 5] 


We can also mix the parallel and sequential operators
to create a mix of single notes and chords. When 
doing this, remember that parentheses can be important!

> combo1 = c 4 qn :+: e 4 qn :=: g 4 qn :+: c 5 qn
> combo2 = (c 4 qn :+: e 4 qn :=: g 4 qn) :+: c 5 qn -- same as above 
> combo3 = c 4 qn :+: (e 4 qn :=: (g 4 qn :+: (c 5 qn))) -- same as above 
> combo4 = c 4 qn :+: (e 4 qn :=: g 4 qn :+: c 5 qn) -- same as above
> combo5 = (c 4 qn :+: e 4 qn) :=: (g 4 qn :+: c 5 qn) -- different!


Now let's use some of the Modify constructors to change 
how a melody sounds. To work with dynamics, we will use
the type Music (Pitch,Volume). We can convert a Music Pitch
value to Music (Pitch,Volume) with addVolume.

> myMel1 = line [c 4 en, c 4 en, g 4 en, g 4 en, a 4 en, a 4 en, g 4 qn]
> myMel2 = Modify (Phrase [Tmp $ Ritardando 0.5]) myMel1
> myMel3 = Modify (Phrase [Tmp $ Accelerando 0.5]) myMel1
> myMel4 = Modify (Phrase [Dyn $ Diminuendo 0.8]) $ addVolume 100 myMel1
> myMel5 = Modify (Phrase [Dyn $ Crescendo 2.0]) $ addVolume 50 myMel1
> myMel6 = Modify (Tempo 2) myMel1
> myMel7 = Modify (Tempo 0.3) myMel1


Here is a pattern matching example with Music. This
function transposes a music value up an octave.

> octUp :: Music Pitch -> Music Pitch
> octUp (Prim (Note d (p,o))) = note d (p, o+1)
> octUp (Prim (Rest d)) = rest d
> octUp (m1 :+: m2) = octUp m1 :+: octUp m2
> octUp (m1 :=: m2) = octUp m1 :=: octUp m2
> octUp (Modify c m) = Modify c (octUp m)

You can try the following in GHCi:

play (octUp combo1)
play (octUp (octUp combo1))

Note that this is more than one way to accomplish
pattern matching over Music. For example, we could 
also separate handling of the inner constructor 
(Primitive) into another function:

> octUp' :: Music Pitch -> Music Pitch
> octUp' (m1 :+: m2) = octUp' m1 :+: octUp' m2
> octUp' (m1 :=: m2) = octUp' m1 :=: octUp' m2
> octUp' (Modify c m) = Modify c (octUp' m)
> octUp' (Prim p) = Prim (octUpP p) where
>     octUpP :: Primitive Pitch -> Primitive Pitch
>     octUpP (Note d (p,o)) = Note d (p, o+1)
>     octUpP x = x