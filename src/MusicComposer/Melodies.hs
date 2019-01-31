module MusicComposer.Melodies where

import MusicComposer.Primitives
import MusicComposer.Modulation
import MusicComposer.Types

test1 :: Voice 
test1 = concat $ take 8 (repeat (note C 4 HN))

-- Recommended TEMPO = 144
littleDogWaltz :: Voice
littleDogWaltz = note Eb 4 EN ++ note Db 4 EN ++ note Gb 3 QN ++ chromatic Bb 3 9 QN ++ chromatic Bb 3 9 QN
              ++ note Eb 4 EN ++ note Db 4 EN ++ note Gb 3 QN ++ chromatic Bb 3 9 QN ++ chromatic Bb 3 9 QN
              ++ note Eb 4 EN ++ note Db 4 EN ++ note Gb 3 QN ++ chromatic Bb 3 9 QN ++ note Eb 3 QN
              ++ chromatic Bb 3 9 QN ++ note Db 3 QN  ++ chromatic B 3 7 QN ++ chromatic B 3 7 QN 
              ++ note Eb 4 EN ++ note Db 4 EN ++ note Db 3 QN ++ chromatic B 3 7 QN ++ chromatic B 3 7 QN 
              ++ note Eb 4 EN ++ note Db 4 EN ++ note Db 3 QN ++ chromatic B 3 7 QN ++ chromatic B 3 7 QN 
              ++ note Eb 4 EN ++ note Db 4 EN ++ note Db 3 QN ++ chromatic B 3 7 QN ++ note Eb 3 QN
              ++ chromatic B 3 7 QN ++ note Gb 3 QN ++ chromatic Bb 3 9 QN ++ chromatic Bb 3 9 QN
              ++ rest QN ++ note Gb 3 QN ++ chromatic Bb 3 9 QN ++ note Eb 3 QN ++ chromatic Bb 3 9 QN
              ++ note Db 3 QN ++ chromatic B 3 7 QN ++ chromatic B 3 7 QN ++ rest QN
              ++ note Db 3 QN ++ chromatic B 3 7 QN ++ note Eb 3 QN ++ chromatic B 3 7 QN
              ++ note Gb 3 QN ++ chromatic Bb 3 9 QN ++ chromatic Bb 3 9 QN

test2 :: Voice
test2 = note C 4 QN ++ diatonic C 4 majorScale 1 5 QN ++ chromatic C 4 2 HN ++ triadMj C 4 HN ++ triadMj1 C 4 HN ++ triadMj2 C 4 HN
     ++ transpose semiUp (triadMj2 C 4 HN) ++ rest WN ++ rest WN

test3 :: Voice 
test3 = triadMj1 C 4 HN ++ triadMj1 C 4 HN ++ triadMj1 C 4 HN ++ triadMj1 C 4 HN

-- Recommended TEMPO = 72
planetTelex :: Voice 
planetTelex = concat $ take 4 $ repeat $ triadMj1 E 3 QNd 
                                      ++ triadMj A 3 EN ++ triadMj A 3 HN 
                                      ++ diatonic B 3 minorScale 1 3 QNd 
                                      ++ triadMin A 3 EN ++ triadMin A 3 HN


multiTest :: Music
multiTest = [keys] ++ [bass]
    where
        keys :: Voice
        keys = concat $ take 16 $ repeat $ triadMj1 C 4 HN 

        bass :: Voice
        bass = concat $ take 8 $ repeat $ note C 3 HNd ++ note C 2 QN


imagineLennon :: Music
imagineLennon = [piano] ++ [bass] ++ [voice]
    where
        piano = diatonic C 3 majorScale 3 5 EN ++ note C 3 EN ++ diatonic C 3 majorScale 3 5 EN ++ note C 3 EN 
            ++ diatonic C 3 majorScale 3 5 EN ++ note C 3 EN ++ diatonic C 3 majorScale 5 7 EN ++ note C 3 EN

            ++ diatonic F 3 majorScale 1 3 EN ++ note C 3 EN ++ diatonic F 3 majorScale 1 3 EN ++ note C 3 EN
            ++ diatonic F 3 majorScale 1 3 EN ++ note C 3 EN ++ diatonic F 3 majorScale 1 3 SN ++ note Bb 3 SN
            ++ note B 3 EN

            ++ diatonic C 3 majorScale 3 5 EN ++ note C 3 EN ++ diatonic C 3 majorScale 3 5 EN ++ note C 3 EN 
            ++ diatonic C 3 majorScale 3 5 EN ++ note C 3 EN ++ diatonic C 3 majorScale 5 7 EN ++ note C 3 EN

            ++ diatonic F 3 majorScale 1 3 EN ++ note C 3 EN ++ diatonic F 3 majorScale 1 3 EN ++ note C 3 EN
            ++ diatonic F 3 majorScale 1 3 EN ++ note C 3 EN ++ diatonic F 3 majorScale 1 3 SN ++ note Bb 3 SN
            ++ note B 3 EN

            ++ diatonic C 3 majorScale 3 5 EN ++ note C 3 EN ++ diatonic C 3 majorScale 3 5 EN ++ note C 3 EN 
            ++ diatonic C 3 majorScale 3 5 EN ++ note C 3 EN ++ diatonic C 3 majorScale 5 7 EN ++ note C 3 EN

            ++ diatonic F 3 majorScale 1 3 EN ++ note C 3 EN ++ diatonic F 3 majorScale 1 3 EN ++ note C 3 EN
            ++ diatonic F 3 majorScale 1 3 EN ++ note C 3 EN ++ diatonic F 3 majorScale 1 3 SN ++ note Bb 3 SN
            ++ note B 3 EN


        bass = note C 2 HNd ++ note E 2 QN 
            ++ note F 2 HNd ++ note F 2 QN
            ++ note C 2 HNd ++ note E 2 QN
            ++ note F 2 HNd ++ note F 2 QN
            ++ note C 2 HNd ++ note E 2 QN
            ++ note F 2 HNd ++ note F 2 QN


        voice = rest WN ++ rest WN ++ rest WN ++ rest WN ++ rest QN
             ++ rest ENd ++ note G 4 SN ++ note G 4 SN ++ note G 4 QNd ++ note G 4 QN ++ note B 4 EN ++ note B 4 SN 
             ++ note A 4 EN
