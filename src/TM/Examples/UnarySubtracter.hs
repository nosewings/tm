module TM.Examples.UnarySubtracter
  ( Alphabet(..)
  , State(..)
  , unarySubtracter
  ) where

import Data.Void

import TM.PP
import TM.Action
import TM.Machine hiding (delta)
import TM.Next
import TM.Tape

data Alphabet = O | M
  deriving (Show)

instance PP Alphabet where
  pp O = "1"
  pp M = "-"

pattern O' = InputSymbol O
pattern M' = InputSymbol M

data State
  = Q0
  | Q1
  | Q2
  | Q3
  | Q4
  | Q5
  deriving (Show)

pattern Q0' = Continue Q0
pattern Q1' = Continue Q1
pattern Q2' = Continue Q2
pattern Q3' = Continue Q3
pattern Q4' = Continue Q4
pattern Q5' = Continue Q5

instance PP State where
  pp Q0 = "0"
  pp Q1 = "1"
  pp Q2 = "2"
  pp Q3 = "3"
  pp Q4 = "4"
  pp Q5 = "5"

delta :: DeltaFunction State Alphabet Void

delta Q0 O'    = mkAction O' R Q0'
delta Q0 M'    = mkAction M' R Q1'
delta Q0 Blank = reject Blank

delta Q1 O'    = mkAction O' R Q1'
delta Q1 M'    = reject M'
delta Q1 Blank = mkAction Blank L Q2'

delta Q2 O'    = mkAction Blank L Q3'
delta Q2 M'    = accept Blank
delta Q2 Blank = reject Blank

delta Q3 O'    = mkAction O' L Q3'
delta Q3 M'    = mkAction M' L Q4'
delta Q3 Blank = reject Blank

delta Q4 O'    = mkAction Blank R Q5'
delta Q4 M'    = reject M'
delta Q4 Blank = mkAction Blank L Q4'

delta Q5 O'    = reject O'
delta Q5 M'    = mkAction M' R Q1'
delta Q5 Blank = mkAction Blank R Q5'

unarySubtracter :: Machine State Alphabet Void
unarySubtracter = mkMachine Q0 delta
