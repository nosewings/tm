module TM.Tape
  ( Symbol
  , Tape
  , cells
  , head
  , fromSeq
  , read
  , write
  , shiftLeft
  , shiftRight
  , Shift(..)
  , shift
  ) where

import Prelude hiding (head, read)

import Control.Lens
import Control.Lens.TH

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

type Symbol g = Maybe g

data Tape g =
  Tape { _cells :: Seq (Symbol g)
       , _head :: Int
       }
  deriving (Show)
makeLenses ''Tape

fromSeq :: Seq g -> Tape g
fromSeq c = Tape (Just <$> c) 0

read :: Tape g -> Symbol g
read t = Seq.index (t^.cells) (t^.head)

write :: Symbol g -> Tape g -> Tape g
write s t = t&cells %~ Seq.update (t^.head) s

shiftLeft :: Tape g -> Tape g
shiftLeft t
  | t^.head == 0 = Tape (Nothing <| t^.cells) 0
  | otherwise    = Tape (t^.cells) (t^.head - 1)

shiftRight :: Tape g -> Tape g
shiftRight t
  | t^.head == length (t^.cells) - 1 =
      Tape (t^.cells |> Nothing) (t^.head + 1)
  | otherwise                        =
      Tape (t^.cells) (t^.head + 1)

data Shift = L | N | R

shift :: Shift -> Tape g -> Tape g
shift L = shiftLeft
shift N = id
shift R = shiftRight
