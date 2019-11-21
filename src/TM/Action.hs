module TM.Action
  ( Next(..)
  , Action
  , symbol
  , shift
  , next
  , mkAction
  , reject
  , accept
  ) where

import Control.Lens.TH (makeLenses)

import TM.Answer
import TM.Tape (Shift(..))
import qualified TM.Tape as Tape
import TM.Next

data Action q s g = Action { _symbol :: Tape.Symbol s g
                           , _shift  :: Tape.Shift
                           , _next   :: Next q
                           }
makeLenses ''Action

mkAction :: Tape.Symbol s g -> Tape.Shift -> Next q -> Action q s g
mkAction = Action

reject :: Tape.Symbol s g -> Action q s g
reject x = mkAction x N Reject'

accept :: Tape.Symbol s g -> Action q s g
accept x = mkAction x N Accept'
