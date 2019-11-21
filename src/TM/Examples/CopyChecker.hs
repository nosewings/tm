module TM.Examples.CopyChecker
  ( Alphabet(..)
  , State(..)
  , copyChecker
  ) where

import Data.Void

import TM.PP
import TM.Action
import TM.Machine hiding (delta)
import TM.Next
import TM.Tape

data Alphabet = A | B | P
  deriving (Show)

instance PP Alphabet where
  pp A = "a"
  pp B = "b"
  pp P = "#"

pattern A' = InputSymbol A
pattern B' = InputSymbol B
pattern P' = InputSymbol P

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
  | Q9
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
pattern Q9' = Continue Q9

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
  pp Q9 = "9"

delta :: DeltaFunction State Alphabet Void

delta Q0 A'    = mkAction A' R Q0'
delta Q0 B'    = mkAction B' R Q0'
delta Q0 P'    = mkAction P' R Q1'
delta Q0 Blank = reject Blank

delta Q1 A'    = mkAction A' R Q1'
delta Q1 B'    = mkAction B' R Q1'
delta Q1 P'    = reject P'
delta Q1 Blank = mkAction P' L Q2'

delta Q2 A'    = mkAction A' L Q2'
delta Q2 B'    = mkAction B' L Q2'
delta Q2 P'    = mkAction P' L Q2'
delta Q2 Blank = mkAction Blank R Q3'

delta Q3 A'    = mkAction Blank R Q4'
delta Q3 B'    = mkAction Blank R Q6'
delta Q3 P'    = mkAction Blank R Q9'
delta Q3 Blank = reject Blank

delta Q4 A'    = mkAction A' R Q4'
delta Q4 B'    = mkAction B' R Q4'
delta Q4 P'    = mkAction P' R Q5'
delta Q4 Blank = reject Blank

delta Q5 A'    = mkAction Blank L Q8'
delta Q5 B'    = reject B'
delta Q5 P'    = reject P'
delta Q5 Blank = mkAction Blank R Q5'

delta Q6 A'    = mkAction A' R Q6'
delta Q6 B'    = mkAction B' R Q6'
delta Q6 P'    = mkAction P' R Q7'
delta Q6 Blank = reject Blank

delta Q7 A'    = reject A'
delta Q7 B'    = mkAction Blank L Q8'
delta Q7 P'    = reject P'
delta Q7 Blank = mkAction Blank R Q7'

delta Q8 A'    = reject A'
delta Q8 B'    = reject B'
delta Q8 P'    = mkAction P' L Q2'
delta Q8 Blank = mkAction Blank L Q8'

delta Q9 A'    = reject A'
delta Q9 B'    = reject B'
delta Q9 P'    = accept Blank
delta Q9 Blank = mkAction Blank R Q9'

copyChecker :: Machine State Alphabet Void
copyChecker = mkMachine Q0 delta
