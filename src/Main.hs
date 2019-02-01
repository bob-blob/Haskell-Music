{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import MusicComposer.Player
import MusicComposer.Melodies

main :: IO ()
main = do
  initSDLMixer
  --playVoice 72 planetTelex
  --playMusic 120 multiTest
  --playMusic 70 imagineLennon
  playVoice 120 testConnect
  closeSDLMixer