module TM.Next where

import Control.Lens.TH (makePrisms)

import TM.PP
import TM.Answer

data Next q = Stop Answer | Continue q
  deriving Functor
makePrisms ''Next

pattern Reject' = Stop Reject
pattern Accept' = Stop Accept

instance (PP q) => PP (Next q) where
  pp' (Stop a)     = pp' a
  pp' (Continue q) = pp' q
