module Util (
    fst'
  , snd'
  , third'
  ) where

fst' :: (a,b,c) -> a
fst' (a,_,_) = a

snd' :: (a,b,c) -> b
snd' (_,b,_) = b

third' :: (a,b,c) -> c
third' (_,_,c) = c
