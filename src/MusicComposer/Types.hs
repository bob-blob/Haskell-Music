module MusicComposer.Types where

type Octave = Int
--type Duration = Int
type Tempo = Int
type Scale = [Note->Note]
type Root = NoteLetter
type Inversion = Int

type Music = [Voice]
type Voice = [MusicalObject]

data RhythmObject = NoteR Duration
                  | RestR Duration
                  

data Duration = 
    WN   -- ^ Whole note duration
  | HN   -- ^ Half note duration
  | QN   -- ^ Quarter note duration
  | EN   -- ^ Eighth note duration
  | SN   -- ^ Sixteenth note duration
  | WNd  -- ^ Whole and Half note duration
  | HNd  -- ^ Half and Quarter note duration
  | QNd  -- ^ Quarter and Eighth note duration
  | ENd  -- ^ Eighth and Sixteenth note duration
  deriving (Eq, Show)

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
whole obj = setDuration obj WN

half :: MusicalObject -> MusicalObject
half obj = setDuration obj HN

quarter :: MusicalObject -> MusicalObject
quarter obj = setDuration obj QN

durationValue :: Duration -> Double
durationValue dur = 
	case dur of
		WN -> 1.0
		HN -> 2.0
		QN -> 4.0
		EN -> 8.0
		SN -> 16.0
		HNd -> 1.20 -- 1.29
		QNd -> 2.6667
		ENd -> 5.33333
