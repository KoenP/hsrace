module Overlay where

import Vec
import Util

import Graphics.Gloss

fromWindowTop :: Vec Window -> Double -> Picture -> Picture
fromWindowTop _windowSize@(Vec _ y) dst pic = translatePic (Vec 0 ((y/2) - dst)) pic
                                              
fromWindowLeft :: Vec Window -> Double -> Picture -> Picture
fromWindowLeft _windowSize@(Vec x _) dst pic = translatePic (Vec (dst - (x/2)) 0) pic
                                               
