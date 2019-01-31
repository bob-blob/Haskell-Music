module MusicComposer.Modulation where

import MusicComposer.Types

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

-- | Transpose musical voice by given interval
transpose :: (Note -> Note) -> Voice -> Voice
transpose _ [] = []
transpose shift ((Rest d):xs) = (Rest d) : transpose shift xs
transpose shift ((SingleNote note d):xs) = (SingleNote (shift note) d) : transpose shift xs
transpose shift ((NoteGroup notes d):xs) = (NoteGroup (map shift notes) d) : transpose shift xs


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


