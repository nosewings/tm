module TM.Configuration
  ( Configuration
  , state
  , tape
  , mkConfiguration
  ) where

import Control.Monad.Writer (tell)

import Control.Lens
import Control.Lens.TH (makeLenses)

import TM.PP
import TM.Next (Next)
import TM.Tape (Tape)

data Configuration q s g =
  Configuration { _state :: Next q
                , _tape  :: Tape s g
                }
makeLenses ''Configuration

instance (PP q, PP s, PP g) => PP (Configuration s g q) where
  pp' c = do
    tell "State: "
    pp' (c^.state)
    tell "\nTape:\n"
    pp' (c^.tape)

mkConfiguration :: Next q -> Tape s g -> Configuration q s g
mkConfiguration = Configuration
