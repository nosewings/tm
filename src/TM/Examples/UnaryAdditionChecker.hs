module TM.Examples.UnaryAdditionChecker
  ( Alphabet(..)
  , State(..)
  , unaryAdditionChecker
  ) where

import Data.Void

import TM.PP
import TM.Action
import TM.Machine hiding (delta)
import TM.Next
import TM.Tape

data Alphabet = O | P | E
  deriving (Show)

instance PP Alphabet where
  pp O = "1"
  pp P = "+"
  pp E = "="

pattern O' = InputSymbol O
pattern P' = InputSymbol P
pattern E' = InputSymbol E

data State
  = Q0
  | Q1
  | Q2
  | Q3
  | Q4
  | Q5
  | Q6
  | Q7
  | Q8
  deriving (Show)

pattern Q0' = Continue Q0
pattern Q1' = Continue Q1
pattern Q2' = Continue Q2
pattern Q3' = Continue Q3
pattern Q4' = Continue Q4
pattern Q5' = Continue Q5
pattern Q6' = Continue Q6
pattern Q7' = Continue Q7
pattern Q8' = Continue Q8

instance PP State where
  pp Q0 = "0"
  pp Q1 = "1"
  pp Q2 = "2"
  pp Q3 = "3"
  pp Q4 = "4"
  pp Q5 = "5"
  pp Q6 = "6"
  pp Q7 = "7"
  pp Q8 = "8"

delta :: DeltaFunction State Alphabet Void

delta Q0 O'    = mkAction O' R Q0'
delta Q0 P'    = mkAction P' R Q1'
delta Q0 E'    = reject E'
delta Q0 Blank = reject Blank

delta Q1 O'    = mkAction O' R Q1'
delta Q1 P'    = reject P'
delta Q1 E'    = mkAction E' R Q2'
delta Q1 Blank = reject Blank

delta Q2 O'    = mkAction O' R Q2'
delta Q2 P'    = reject P'
delta Q2 E'    = reject E'
delta Q2 Blank = mkAction E' L Q3'

delta Q3 O'    = mkAction O' L Q3'
delta Q3 P'    = mkAction P' L Q3'
delta Q3 E'    = mkAction E' L Q3'
delta Q3 Blank = mkAction Blank R Q4'

delta Q4 O'    = mkAction Blank R Q5'
delta Q4 P'    = mkAction Blank R Q4'
delta Q4 E'    = mkAction Blank R Q8'
delta Q4 Blank = reject Blank

delta Q5 O'    = mkAction O' R Q5'
delta Q5 P'    = mkAction P' R Q5'
delta Q5 E'    = mkAction E' R Q6'
delta Q5 Blank = reject Blank

delta Q6 O'    = mkAction Blank L Q7'
delta Q6 P'    = reject P'
delta Q6 E'    = reject E'
delta Q6 Blank = mkAction Blank R Q6'

delta Q7 O'    = reject O'
delta Q7 P'    = reject P'
delta Q7 E'    = mkAction E' L Q3'
delta Q7 Blank = mkAction Blank L Q7'

delta Q8 O'    = reject O'
delta Q8 P'    = reject P'
delta Q8 E'    = accept Blank
delta Q8 Blank = mkAction Blank R Q8'

unaryAdditionChecker = mkMachine Q0 delta
