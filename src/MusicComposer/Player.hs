module MusicComposer.Player where

import MusicComposer.Types
import Control.Monad
import Control.Concurrent
import Control.Monad.Fix
import qualified SDL
import qualified SDL.Mixer as Mixer
import Control.Concurrent.ParallelIO.Global

initSDLMixer :: IO ()
initSDLMixer =  do
    SDL.initialize [SDL.InitAudio]
    Mixer.openAudio (Mixer.Audio { Mixer.audioFrequency = 44100,
                                 Mixer.audioFormat = Mixer.FormatS16_LSB,
                                 Mixer.audioOutput = Mixer.Mono }) 4096
    Mixer.setChannels 64

closeSDLMixer :: IO ()
closeSDLMixer = do 
    threadDelay 1000000
  
    Mixer.closeAudio
    Mixer.quit
    SDL.quit

-- | Player for Single-Voice melody
playVoice :: Tempo -> Voice -> IO ()
playVoice _ [] = return ()
playVoice tempo ((Rest d):xs) = do
    obj <- Mixer.load ("assets/notes/rest.aiff")
    channel <- Mixer.playOn (-1) Mixer.Once obj
    threadDelay ((round ((240 / (fromIntegral(tempo)*durationValue(d)))*1000000)) :: Int)
    Mixer.halt channel
    playVoice tempo xs
playVoice tempo ((SingleNote (Note l o) d):xs) = do
    obj <- Mixer.load ("assets/notes/Piano.pp." ++ (show l) ++ (show o) ++ ".aiff")
    channel <- Mixer.playOn (-1) Mixer.Once obj
    threadDelay ((round ((240 / (fromIntegral(tempo)*durationValue(d)))*1000000)) :: Int)
    Mixer.halt channel
    playVoice tempo xs
playVoice tempo ((NoteGroup n@((Note l o):ns) d):xs) = do
    channels <- playNotes n []
    threadDelay ((round ((240 / (fromIntegral(tempo)*durationValue(d)))*1000000)) :: Int)
    mapM_ Mixer.halt channels
    playVoice tempo xs
    where
        playNotes [] chnls = return chnls
        playNotes a@((Note l o):ns) chnls = do
            obj <- Mixer.load ("assets/notes/Piano.pp." ++ (show l) ++ (show o) ++ ".aiff")
            channel <- Mixer.playOn (-1) Mixer.Once obj
            playNotes ns (channel:chnls)

-- | Player for Multi-Voice melody
-- | [ Instrument1@(SingleNote, ...), Instrument2@(NoteGroup, ...) ]
playMusic :: Tempo -> Music -> IO () 
playMusic t m = parallel_ funcs >> stopGlobalPool 
  where
    funcs = collectFuncs t m

collectFuncs :: Tempo -> Music -> [IO ()]
collectFuncs _ [] = []
collectFuncs t (x:xs) = playVoice t x : collectFuncs t xs