module MusicComposer.Primitives where

import MusicComposer.Types
import MusicComposer.Modulation

-- | Musical primitives
rest :: Duration -> [MusicalObject]
rest dur = [Rest dur]

note :: NoteLetter -> Octave -> Duration -> [MusicalObject]
note l o d = [SingleNote (Note l o) d]

notes :: [(NoteLetter, Octave)] -> Duration -> [MusicalObject]
notes note dur = [NoteGroup (map makeNote note) dur]
  where
    makeNote (letter, octave) = Note letter octave

-- Vertical Intervals 
diatonic :: Root -> Octave -> Scale -> Int -> Int -> Duration -> [MusicalObject]
diatonic l o scale 1 second d = [NoteGroup (n:((scale !! (second-2)) $ n):[]) d]
	where 
    	n = Note l o
diatonic l o scale first second d = [NoteGroup (newNote:((scale !! (second-2)) $ n):[]) d]
  	where 
    	n = Note l o
    	newNote = (scale !! (first-2)) $ n

-- Chromatic vertical interval
chromatic :: Root -> Octave -> Int -> Duration -> [MusicalObject]
chromatic l o lift d = [NoteGroup (n:(liftNoteBy (lift-1) n):[]) d]
  where
    n = Note l o

    liftNoteBy :: Int -> Note -> Note
    liftNoteBy 0 = id
    liftNoteBy n = sharp . liftNoteBy (n-1)    

-- | Triad chords
-- | Major chords of different inversions
triadMj l o d = triadGeneric l o majorScale 0 d
triadMj1 l o d = triadGeneric l o majorScale 1 d 
triadMj2 l o d = triadGeneric l o majorScale 2 d 
-- | Minor chords of different inversions
triadMin l o d = triadGeneric l o minorScale 0 d
triadMin1 l o d = triadGeneric l o minorScale 1 d 
triadMin2 l o d = triadGeneric l o minorScale 2 d 

-- | Generic triad chord builder
triadGeneric :: Root -> Octave -> Scale -> Inversion -> Duration -> [MusicalObject]
triadGeneric l o scale 0 d = [NoteGroup (first:third:fifth:[]) d]
  where 
    first = Note l o
    third = scale !! 1 $ first
    fifth = scale !! 3 $ first
triadGeneric l o scale 1 d = [NoteGroup (third:fifth:first:[]) d]
  where 
    firstOld = Note l o
    third = scale !! 1 $ firstOld
    fifth = scale !! 3 $ firstOld
    first = scale !! 6 $ firstOld
triadGeneric l o scale 2 d = [NoteGroup (fifth:first:third:[]) d]
  where 
    firstOld = Note l o
    thirdOld = scale !! 1 $ firstOld
    fifth = scale !! 3 $ firstOld
    first = scale !! 6 $ firstOld
    third = sixUp thirdOld

-- | Generic scale function
scale :: Scale -> Root -> Octave -> Duration -> [MusicalObject]
scale s l o d = scaleNotes s (take 7 (repeat (Note l o)))
  where
    scaleNotes :: Scale -> [Note] -> [MusicalObject]
    scaleNotes [] _ = []
    scaleNotes (f:fs) (x:xs) = SingleNote (f x) d : scaleNotes fs xs 

-- | Array of interval functions that represent structure of music scales
majorScale = [wholeUp, twoUp, twoHalfUp, threeHalfUp, fourHalfUp, fiveHalfUp, sixUp]
minorScale = [wholeUp, oneHalfUp, twoHalfUp, threeHalfUp, fourUp, fiveUp, sixUp]
heptatonicScale = [wholeUp, oneHalfUp, twoHalfUp, threeUp, fourHalfUp, fiveUp, sixUp]