module TM.Tape
  ( Symbol(..)
  , Tape
  , cells
  , head
  , fromList
  , read
  , write
  , Shift(..)
  , shift
  ) where

import Prelude
  hiding ( head
         , read
         )

import Control.Monad
  ( forM_
  , replicateM_
  )
import Control.Monad.Writer (tell)

import Control.Lens
import Control.Lens.TH (makeLenses)

import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Builder as Text.Builder

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import TM.PP

data Symbol s g = InputSymbol s | TapeSymbol g | Blank
  deriving (Show)

instance (PP s, PP g) => PP (Symbol s g) where
  pp' (InputSymbol s) = pp' s
  pp' (TapeSymbol g)  = pp' g
  pp' Blank           = tell "□"

data Tape g s = Tape { _cells :: Seq (Symbol g s)
                     , _head  :: Int
                     }
              deriving (Show)
makeLenses ''Tape

instance (PP s, PP g) => PP (Tape s g) where
  pp' t = do
    let strs  = pp <$> t^.cells
        strs' = Seq.intersperse " " strs
    forM_ strs' (tell . Text.Builder.fromLazyText)
    tell "\n"
    let cums  = Seq.scanl (+) 0 (Text.length <$> strs')
        cums' = Seq.index cums <$> [0, 2 .. length cums - 1]
        i     = Seq.index cums' (t^.head)
    replicateM_ (fromIntegral i) (tell " ")
    tell "^\n"

fromList :: [s] -> Tape s g
fromList c = Tape c'' 0
  where
    c'  = Seq.fromList c
    c'' = if Seq.null c' then Seq.singleton Blank else InputSymbol <$> c'

read :: Tape g s -> Symbol g s
read t = Seq.index (t^.cells) (t^.head)

write :: Symbol g s -> Tape g s -> Tape g s
write s t = t & cells %~ Seq.update (t^.head) s

shiftLeft :: Tape g s -> Tape g s
shiftLeft t
  | t^.head == 0 = Tape (Blank <| t^.cells) 0
  | otherwise    = Tape (t^.cells) (t^.head - 1)

shiftRight :: Tape g s -> Tape g s
shiftRight t
  | t^.head == length (t^.cells) - 1 =
      Tape (t^.cells |> Blank) (t^.head + 1)
  | otherwise                        =
      Tape (t^.cells) (t^.head + 1)

data Shift = L | N | R
  deriving (Show)

shift :: Shift -> Tape g s -> Tape g s
shift L = shiftLeft
shift N = id
shift R = shiftRight
