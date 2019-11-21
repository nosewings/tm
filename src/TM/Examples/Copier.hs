module TM.Examples.Copier
  ( InputAlphabet(..)
  , TapeAlphabet(..)
  , State(..)
  , copier
  ) where

import TM.PP
import TM.Action
import TM.Machine hiding (delta)
import TM.Next
import TM.Tape

data InputAlphabet = AL | BL
  deriving (Show)

instance PP InputAlphabet where
  pp AL = "a"
  pp BL = "b"

pattern AL' = InputSymbol AL
pattern BL' = InputSymbol BL

data TapeAlphabet = AU | BU | P
  deriving (Show)

instance PP TapeAlphabet where
  pp AU = "A"
  pp BU = "B"
  pp P  = "#"

pattern AU' = TapeSymbol AU
pattern BU' = TapeSymbol BU
pattern P'  = TapeSymbol P

data State
  = Q0
  | Q1
  | Q2
  | Q3
  | Q4
  | Q5
  | Q6
  | Q7
  deriving (Show)

pattern Q0' = Continue Q0
pattern Q1' = Continue Q1
pattern Q2' = Continue Q2
pattern Q3' = Continue Q3
pattern Q4' = Continue Q4
pattern Q5' = Continue Q5
pattern Q6' = Continue Q6
pattern Q7' = Continue Q7

instance PP State where
  pp Q0 = "0"
  pp Q1 = "1"
  pp Q2 = "2"
  pp Q3 = "3"
  pp Q4 = "4"
  pp Q5 = "5"
  pp Q6 = "6"
  pp Q7 = "7"

delta :: DeltaFunction State InputAlphabet TapeAlphabet

delta Q0 AL'   = mkAction AL' R Q0'
delta Q0 BL'   = mkAction BL' R Q0'
delta Q0 AU'   = reject AU'
delta Q0 BU'   = reject BU'
delta Q0 P'    = reject P'
delta Q0 Blank = mkAction P' L Q1'

delta Q1 AL'   = mkAction AL' L Q1'
delta Q1 BL'   = mkAction BL' L Q1'
delta Q1 AU'   = mkAction AU' R Q2'
delta Q1 BU'   = mkAction BU' R Q2'
delta Q1 P'    = mkAction P' L Q1'
delta Q1 Blank = mkAction Blank R Q2'

delta Q2 AL'   = mkAction AU' R Q3'
delta Q2 BL'   = mkAction BU' R Q4'
delta Q2 AU'   = mkAction AU' R Q2'
delta Q2 BU'   = mkAction BU' R Q2'
delta Q2 P'    = mkAction P' L Q7'
delta Q2 Blank = reject Blank

delta Q3 AL'   = mkAction AL' R Q3'
delta Q3 BL'   = mkAction BL' R Q3'
delta Q3 AU'   = reject AU'
delta Q3 BU'   = reject BU'
delta Q3 P'    = mkAction P' R Q5'
delta Q3 Blank = reject Blank

delta Q4 AL'   = mkAction AL' R Q4'
delta Q4 BL'   = mkAction BL' R Q4'
delta Q4 AU'   = reject AU'
delta Q4 BU'   = reject BU'
delta Q4 P'    = mkAction P' R Q6'
delta Q4 Blank = reject Blank

delta Q5 AL'   = mkAction AL' R Q5'
delta Q5 BL'   = mkAction BL' R Q5'
delta Q5 AU'   = reject AU'
delta Q5 BU'   = reject BU'
delta Q5 P'    = reject P'
delta Q5 Blank = mkAction AL' L Q1'

delta Q6 AL'   = mkAction AL' R Q6'
delta Q6 BL'   = mkAction BL' R Q6'
delta Q6 AU'   = reject AU'
delta Q6 BU'   = reject BU'
delta Q6 P'    = reject P'
delta Q6 Blank = mkAction BL' L Q1'

delta Q7 AL'   = reject AL'
delta Q7 BL'   = reject BL'
delta Q7 AU'   = mkAction AL' L Q7'
delta Q7 BU'   = mkAction BL' L Q7'
delta Q7 P'    = reject P'
delta Q7 Blank = accept Blank

copier :: Machine State InputAlphabet TapeAlphabet
copier = mkMachine Q0 delta
