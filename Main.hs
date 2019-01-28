{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import CodeWorld
import Control.Monad
import Control.Concurrent
import Control.Monad.Fix
import qualified SDL
import qualified SDL.Mixer as Mixer
import Data.Time.Clock

main :: IO ()
main = do
  SDL.initialize [SDL.InitAudio]
  Mixer.openAudio (Mixer.Audio { Mixer.audioFrequency = 44100,
                                 Mixer.audioFormat = Mixer.FormatS16_LSB,
                                 Mixer.audioOutput = Mixer.Mono }) 4096
  Mixer.setChannels 64

  playSingleVoiceMelody 72 planetTelex

  threadDelay 1000000
  Mixer.closeAudio
  Mixer.quit
  SDL.quit


playSingleVoiceMelody :: Tempo -> Voice -> IO ()
playSingleVoiceMelody _ [] = return ()
playSingleVoiceMelody tempo ((Rest d):xs) = do
  obj <- Mixer.load ("assets/notes/rest.aiff")
  channel <- Mixer.playOn (-1) Mixer.Once obj
  threadDelay ((round ((240 / fromIntegral(d*tempo))*1000000)) :: Int)
  Mixer.halt channel
  playSingleVoiceMelody tempo xs

playSingleVoiceMelody tempo ((SingleNote (Note l o) d):xs) = do
  obj <- Mixer.load ("assets/notes/Piano.pp." ++ (show l) ++ (show o) ++ ".aiff")
  channel <- Mixer.playOn (-1) Mixer.Once obj
  threadDelay ((round ((240 / fromIntegral(d*tempo))*1000000)) :: Int)
  Mixer.halt channel
  playSingleVoiceMelody tempo xs
  
playSingleVoiceMelody tempo ((NoteGroup n@((Note l o):ns) d):xs) = do
  channels <- playNotes n []
  threadDelay ((round ((240 / fromIntegral(d*tempo))*1000000)) :: Int)
  mapM_ Mixer.halt channels
  playSingleVoiceMelody tempo xs
  where
    playNotes [] chnls = return chnls
    playNotes a@((Note l o):ns) chnls = do
      obj <- Mixer.load ("assets/notes/Piano.pp." ++ (show l) ++ (show o) ++ ".aiff")
      channel <- Mixer.playOn (-1) Mixer.Once obj
      playNotes ns (channel:chnls)

-- | Examples 
test1 :: Voice 
test1 = concat $ take 8 (repeat (note C 4 2))

-- Recommended TEMPO = 144
littleDogWaltz :: Voice
littleDogWaltz = note Eb 4 8 ++ note Db 4 8 ++ note Gb 3 4 ++ chromatic Bb 3 9 4 ++ chromatic Bb 3 9 4
              ++ note Eb 4 8 ++ note Db 4 8 ++ note Gb 3 4 ++ chromatic Bb 3 9 4 ++ chromatic Bb 3 9 4
              ++ note Eb 4 8 ++ note Db 4 8 ++ note Gb 3 4 ++ chromatic Bb 3 9 4 ++ note Eb 3 4
              ++ chromatic Bb 3 9 4 ++ note Db 3 4  ++ chromatic B 3 7 4 ++ chromatic B 3 7 4 
              ++ note Eb 4 8 ++ note Db 4 8 ++ note Db 3 4 ++ chromatic B 3 7 4 ++ chromatic B 3 7 4 
              ++ note Eb 4 8 ++ note Db 4 8 ++ note Db 3 4 ++ chromatic B 3 7 4 ++ chromatic B 3 7 4 
              ++ note Eb 4 8 ++ note Db 4 8 ++ note Db 3 4 ++ chromatic B 3 7 4 ++ note Eb 3 4
              ++ chromatic B 3 7 4 ++ note Gb 3 4 ++ chromatic Bb 3 9 4 ++ chromatic Bb 3 9 4
              ++ rest 4 ++ note Gb 3 4 ++ chromatic Bb 3 9 4 ++ note Eb 3 4 ++ chromatic Bb 3 9 4
              ++ note Db 3 4 ++ chromatic B 3 7 4 ++ chromatic B 3 7 4 ++ rest 4
              ++ note Db 3 4 ++ chromatic B 3 7 4 ++ note Eb 3 4 ++ chromatic B 3 7 4
              ++ note Gb 3 4 ++ chromatic Bb 3 9 4 ++ chromatic Bb 3 9 4

test2 :: Voice
test2 = note C 4 2 ++ diatonic C 4 majorScale 5 2 ++ chromatic C 4 2 2 ++ triadMj C 4 2 ++ triadMj1 C 4 2 ++ triadMj2 C 4 2
     ++ transpose semiUp (triadMj2 C 4 2) ++ rest 1 ++ rest 1

test3 :: Voice 
test3 = triadMj1 C 4 2 ++ triadMj1 C 4 2 ++ triadMj1 C 4 2 ++ triadMj1 C 4 2

-- Recommended TEMPO = 72
planetTelex :: Voice 
planetTelex = concat $ take 4 $ repeat $ triadMj1 E 3 4 ++ triadMj1 E 3 8 ++ triadMj A 3 4 ++ triadMj A 3 2 
              ++ diatonic B 3 minorScale 3 4 ++ diatonic B 3 minorScale 3 8 
              ++ triadMin A 3 4 ++ triadMin A 3 2

-- | Data structures 
type Octave = Int
type Duration = Int
type Tempo = Int

type Scale = [Note->Note]
type Root = NoteLetter
type Inversion = Int


type Music = [Voice]
type Voice = [MusicalObject]
data MusicalObject = 
    SingleNote Note Duration 
  | NoteGroup [Note] Duration
  | Rest      Duration
  deriving (Show)
  
data Note = Note NoteLetter Octave
  deriving (Eq, Show)

instance Ord Note where
  (Note l1 o1) `compare` (Note l2 o2)
    | o1 == o2 = l1 `compare` l2
    | o1 /= o2 = o1 `compare` o2

data NoteLetter = C | Db | D | Eb | E | F | Gb | G | Ab | A | Bb | B
  deriving (Enum, Eq, Show, Bounded, Ord)

-- | Getters and setters for duration manipulation
setDuration :: MusicalObject -> Duration -> MusicalObject
setDuration obj dur =
    case obj of
        NoteGroup pitch duration -> NoteGroup pitch dur
        Rest duration -> Rest dur

getDuration :: MusicalObject -> Duration
getDuration obj = case obj of
  NoteGroup _ dur ->  dur
  Rest dur -> dur

whole :: MusicalObject -> MusicalObject
whole obj = setDuration obj 1

half :: MusicalObject -> MusicalObject
half obj = setDuration obj 2

quarter :: MusicalObject -> MusicalObject
quarter obj = setDuration obj 4

-- | Note alternation functions. Make note flat or sharp
flat :: Note -> Note
flat (Note l o) = Note l' o'
  where
    upper = (maxBound :: NoteLetter)
    lower = (minBound :: NoteLetter)
    (l', o')
      | l == lower = (upper, o-1)
      | otherwise = ((pred l), o)

sharp :: Note -> Note
sharp (Note l o) = Note l' o'
  where
    upper = (maxBound :: NoteLetter)
    lower = (minBound :: NoteLetter)
    (l', o')
      | l == upper = (lower, o+1)
      | otherwise = ((succ l), o)

-- | Defined fixed alternations, Intervals
semiUp = sharp
wholeUp = sharp . sharp 
oneHalfUp = semiUp . wholeUp
twoUp = oneHalfUp . semiUp
twoHalfUp = twoUp . semiUp
threeUp = twoHalfUp . semiUp
threeHalfUp = threeUp . semiUp
fourUp = threeHalfUp . semiUp
fourHalfUp = fourUp . semiUp
fiveUp = fourHalfUp . semiUp
fiveHalfUp = fiveUp . semiUp
sixUp = fiveHalfUp . semiUp

semiDown = flat
wholeDown = flat . flat

-- | Scales
majorScale = [wholeUp, twoUp, twoHalfUp, threeHalfUp, fourHalfUp, fiveHalfUp, sixUp]
minorScale = [wholeUp, oneHalfUp, twoHalfUp, threeHalfUp, fourUp, fiveUp, sixUp]
-- | Sort of Blue scale
heptatonicScale = [wholeUp, oneHalfUp, twoHalfUp, threeUp, fourHalfUp, fiveUp, sixUp]


-- | Musical units
rest :: Duration -> [MusicalObject]
rest dur = [Rest dur]

note :: NoteLetter -> Octave -> Duration -> [MusicalObject]
note l o d = [SingleNote (Note l o) d]

notes :: [(NoteLetter, Octave)] -> Duration -> [MusicalObject]
notes note dur = [NoteGroup (map makeNote note) dur]
  where
    makeNote (letter, octave) = Note letter octave

major :: NoteLetter -> Octave -> Duration -> [MusicalObject]
major l o d = makeScale l o d majorScale

minor l o d = makeScale l o d minorScale

makeScale :: NoteLetter -> Octave -> Duration -> Scale -> [MusicalObject]
makeScale l o d scale = scaleNotes scale (take 7 (repeat (Note l o)))
  where
    scaleNotes :: Scale -> [Note] -> [MusicalObject]
    scaleNotes [] _ = []
    scaleNotes (f:fs) (x:xs) = SingleNote (f x) d : scaleNotes fs xs 



-- Vertical Intervals 
diatonic :: Root -> Octave -> Scale -> Int -> Duration -> [MusicalObject]
diatonic l o scale lift d = [NoteGroup (n:((scale !! (lift-2)) $ n):[]) d]
  where 
    n = Note l o

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


-- | Transpose musical voice by given interval
transpose :: (Note -> Note) -> Voice -> Voice
transpose _ [] = []
transpose shift ((Rest d):xs) = (Rest d) : transpose shift xs
transpose shift ((SingleNote note d):xs) = (SingleNote (shift note) d) : transpose shift xs
transpose shift ((NoteGroup notes d):xs) = (NoteGroup (map shift notes) d) : transpose shift xs
