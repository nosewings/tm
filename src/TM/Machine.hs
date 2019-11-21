module TM.Machine
  ( DeltaFunction
  , Machine
  , delta
  , init
  , mkMachine
  , Trace(..)
  , trace
  , traceMachine
  ) where

import Prelude hiding (init)

import Control.Monad.Writer

import Control.Lens
import Control.Lens.TH (makeLenses)

import Data.List (unfoldr)

import TM.PP
import TM.Action
import TM.Answer
import TM.Configuration
import TM.Next
import TM.Tape (Tape)
import qualified TM.Tape as Tape

type DeltaFunction q s g = q -> Tape.Symbol s g -> Action q s g

data Machine q s g =
  Machine { _init  :: q
          , _delta :: DeltaFunction q s g
          }
makeLenses ''Machine

mkMachine :: q -> DeltaFunction q s g -> Machine q s g
mkMachine = Machine

step :: Machine q s g -> Configuration q s g -> Maybe (Configuration q s g)
step m c =
  flip fmap (c^.state^?_Continue) $ \q ->
      let a  = (m^.delta) q (Tape.read (c^.tape))
          t  = Tape.write (a^.symbol) (c^.tape)
          t' = Tape.shift (a^.shift) t
      in mkConfiguration (a^.next) t'

newtype Trace q s g = Trace { unTrace :: [Configuration q s g] }

trace :: Machine q s g -> Configuration q s g -> Trace q s g
trace m c = Trace (c : unfoldr (\c' -> fmap (\x -> (x, x)) (step m c')) c)

traceMachine :: Machine q s g -> [s] -> Trace q s g
traceMachine m t = trace m (mkConfiguration (Continue (m^.init)) (Tape.fromList t))

instance (PP q, PP s, PP g) => PP (Trace q s g) where
    pp' = mapM_ pp' . unTrace
