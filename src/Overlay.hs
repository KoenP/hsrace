module Overlay where

import Vec
import Util

import Graphics.Gloss

fromWindowTop :: Vec Window -> Double -> Picture -> Picture
fromWindowTop _windowSize@(Vec _ y) dst pic = translatePic (Vec 0 ((y/2) - dst)) pic
