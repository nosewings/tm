module TM.Answer where

import Control.Monad.Writer
import Data.Bool

import TM.PP

data Answer = Reject | Accept

instance PP Answer where
  pp' Reject = tell "REJECT"
  pp' Accept = tell "ACCEPT"
