module TM.Machine
  ( Result
  , DeltaFunction
  , Machine
  , delta
  , init
  , make
  , Configuration
  , state
  , tape
  , traceConfiguration
  , trace
  )
where

import Prelude hiding (init)

import Data.Sequence (Seq)

import Control.Lens
import Control.Lens.TH (makeLenses)

import TM.Tape (Tape, Shift(..))
import qualified TM.Tape as Tape

type Result a = Either Bool a

type DeltaFunction q g = q -> Tape.Symbol g -> Result (Tape.Symbol g, Shift, q)

data Machine q g =
  Machine { _delta :: DeltaFunction q g
          , _init :: q
          }
makeLenses ''Machine

make :: DeltaFunction q g -> q -> Machine q g
make d q = Machine d q

data Configuration q g =
  Configuration { _state :: q
                , _tape :: Tape g
                }
  deriving (Show)
makeLenses ''Configuration

step :: Machine q g -> Configuration q g -> Result (Configuration q g)
step m c =
  let newState = (m^.delta) (c^.state) (c^.tape&Tape.read)
  in flip fmap newState $ \(g, s, q) ->
    let t = Tape.write g (c^.tape)
        t' = Tape.shift s t
    in Configuration q t'

traceConfiguration ::
  Machine q g -> Configuration q g -> ([Configuration q g], Bool)
traceConfiguration m c = case step m c of
  Left b -> ([c], b)
  Right c' -> let (cs, b) = traceConfiguration m c'
              in (c:cs, b)

trace :: Machine q g -> Seq g -> ([Configuration q g], Bool)
trace m c = traceConfiguration m (Configuration (m^.init) (Tape.fromSeq c))
