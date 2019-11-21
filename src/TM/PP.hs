module TM.PP where

import Control.Monad.Writer
import Data.Void
import Data.Text.Lazy
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder
import Data.Text.Lazy.IO as Text.IO

class PP a where

  pp' :: (Monad m) => a -> WriterT Builder m ()
  pp' = tell . Text.Builder.fromLazyText . pp

  pp :: a -> Text
  pp = Text.Builder.toLazyText . execWriter . pp'

instance PP Void where

ppIO :: (PP a) => a -> IO ()
ppIO = Text.IO.putStr . pp
