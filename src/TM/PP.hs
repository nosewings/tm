module TM.PP
  ( ppTraceMachine
  ) where

import Control.Monad (forM_)
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Maybe (maybe)

import Data.Sequence (Seq)

import Control.Lens

import TM.Tape (Tape)
import qualified TM.Tape as Tape
import TM.Machine (Machine)
import qualified TM.Machine as Machine

ppTape :: forall g. (Show g) => Tape g -> IO [Int]
ppTape t = do
  let cells = t^.Tape.cells&toList
      strs = maybe "□" show <$> cells
      strs' = intersperse " " strs
      cums = scanl (+) 0 (length <$> strs')
      cums' = map (cums !!) [0, 2 .. length cums - 1]
  putStrLn (concat strs')
  return cums'

ppTraceMachine :: (Show q, Show g) => Machine q g -> Seq g -> IO ()
ppTraceMachine m c = do
  let (cs, r) = Machine.trace m c
  forM_ cs $ \c -> do
    putStrLn ("State: " ++ (c^.Machine.state&show))
    cums <- c^.Machine.tape&ppTape
    let i = (cums !! (c^.Machine.tape^.Tape.head))
    putStr (replicate i ' ')
    putStrLn "^"
    putStrLn ""
  putStrLn (if r then "ACCEPT" else "REJECT")
