module TM.Examples.CopyChecker
 ( Alphabet(..)
 , State(..)
 , copyChecker
 ) where

import TM.Tape (Shift(..))
import qualified TM.Tape as Tape
import TM.Machine (Machine(..))
import qualified TM.Machine as Machine

data Alphabet = A | B | P

instance Show Alphabet where
  show A = "a"
  show B = "b"
  show P = "#"

data State
  = Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine

instance Show State where
  show Zero = "0"
  show One = "1"
  show Two = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"
  show Six = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine = "9"

delta :: Machine.DeltaFunction State Alphabet

delta Zero (Just A) = Right (Just A, R, Zero)
delta Zero (Just B) = Right (Just B, R, Zero)
delta Zero (Just P) = Right (Just P, R, One)
delta Zero Nothing  = Left False

delta One (Just A) = Right (Just A, R, One)
delta One (Just B) = Right (Just B, R, One)
delta One (Just P) = Left False
delta One Nothing  = Right (Just P, L, Two)

delta Two (Just A) = Right (Just A, L, Two)
delta Two (Just B) = Right (Just B, L, Two)
delta Two (Just P) = Right (Just P, L, Two)
delta Two Nothing  = Right (Nothing, R, Three)

delta Three (Just A) = Right (Nothing, R, Four)
delta Three (Just B) = Right (Nothing, R, Six)
delta Three (Just P) = Right (Nothing, R, Nine)
delta Three Nothing  = Left False

delta Four (Just A) = Right (Just A, R, Four)
delta Four (Just B) = Right (Just B, R, Four)
delta Four (Just P) = Right (Just P, R, Five)
delta Four Nothing  = Left False

delta Five (Just A) = Right (Nothing, L, Eight)
delta Five (Just B) = Left False
delta Five (Just P) = Left False
delta Five Nothing  = Right (Nothing, R, Five)

delta Six (Just A) = Right (Just A, R, Six)
delta Six (Just B) = Right (Just B, R, Six)
delta Six (Just P) = Right (Just P, R, Seven)
delta Six Nothing  = Left False

delta Seven (Just A) = Left False
delta Seven (Just B) = Right (Nothing, L, Eight)
delta Seven (Just P) = Left False
delta Seven Nothing  = Right (Nothing, R, Seven)

delta Eight (Just A) = Right (Just A, L, Eight)
delta Eight (Just B) = Right (Just B, L, Eight)
delta Eight (Just P) = Right (Just P, L, Two)
delta Eight Nothing  = Right (Nothing, L, Eight)

delta Nine (Just A) = Left False
delta Nine (Just B) = Left False
delta Nine (Just P) = Left True
delta Nine Nothing  = Right (Nothing, R, Nine)

copyChecker = Machine.make delta Zero
