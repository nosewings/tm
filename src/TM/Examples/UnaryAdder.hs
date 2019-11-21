module TM.Examples.UnaryAdder
  ( Alphabet(..)
  , State(..)
  , unaryAdder
  ) where

import Data.Void

import TM.PP
import TM.Action
import TM.Machine hiding (delta)
import TM.Next
import TM.Tape

data Alphabet = O | P
  deriving (Show)

instance PP Alphabet where
  pp O = "1"
  pp P = "+"

pattern O' = InputSymbol O
pattern P' = InputSymbol P

data State
  = Q0
  | Q1
  | Q2
  | Q3
  | Q4
  | Q5
  | Q6
  deriving (Show)

pattern Q0' = Continue Q0
pattern Q1' = Continue Q1
pattern Q2' = Continue Q2
pattern Q3' = Continue Q3
pattern Q4' = Continue Q4
pattern Q5' = Continue Q5
pattern Q6' = Continue Q6

instance PP State where
  pp Q0 = "0"
  pp Q1 = "1"
  pp Q2 = "2"
  pp Q3 = "3"
  pp Q4 = "4"
  pp Q5 = "5"
  pp Q6 = "6"

delta :: DeltaFunction State Alphabet Void

delta Q0 O'    = mkAction O' R Q0'
delta Q0 P'    = mkAction P' R Q1'
delta Q0 Blank = reject Blank

delta Q1 O'    = mkAction O' R Q1'
delta Q1 P'    = reject P'
delta Q1 Blank = mkAction Blank L Q2'

delta Q2 O'    = mkAction O' L Q2'
delta Q2 P'    = mkAction P' L Q2'
delta Q2 Blank = mkAction Blank R Q3'

delta Q3 O'    = mkAction O' R Q3'
delta Q3 P'    = mkAction Blank R Q4'
delta Q3 Blank = reject Blank

delta Q4 O'    = mkAction Blank L Q5'
delta Q4 P'    = reject P'
delta Q4 Blank = accept Blank

delta Q5 O'    = reject O'
delta Q5 P'    = reject P'
delta Q5 Blank = mkAction O' R Q6'

delta Q6 O'    = reject O'
delta Q6 P'    = reject P'
delta Q6 Blank = mkAction Blank R Q4'

unaryAdder :: Machine State Alphabet Void
unaryAdder = mkMachine Q0 delta
