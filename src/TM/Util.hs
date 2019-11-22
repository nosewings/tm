module TM.Util where

unfoldrHomo :: (a -> Maybe a) -> a -> [a]
unfoldr 
