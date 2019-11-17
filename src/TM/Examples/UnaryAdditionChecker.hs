module TM.Examples.UnaryAdditionChecker where

import TM.Tape
import TM.Machine hiding (delta)
import qualified TM.Machine as Machine

data Alphabet = O | P | E

instance Show Alphabet where
  show O = "1"
  show P = "+"
  show E = "="

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

instance Show State where
  show Zero  = "0"
  show One   = "1"
  show Two   = "2"
  show Three = "3"
  show Four  = "4"
  show Five  = "5"
  show Six   = "6"
  show Seven = "7"
  show Eight = "8"

delta :: DeltaFunction State Alphabet

-- Scan right until we see "+".
delta Zero (Just O) = Continue (Just O, R, Zero)
delta Zero (Just P) = Continue (Just P, R, One)
delta Zero (Just E) = Reject
delta Zero Nothing  = Reject

-- Scan right until we see "=".
delta One (Just O) = Continue (Just O, R, One)
delta One (Just P) = Reject
delta One (Just E) = Continue (Just E, R, Two)
delta One Nothing  = Reject

-- Scan right until we see a blank, then write "=".
delta Two (Just O) = Continue (Just O, R, Two)
delta Two (Just P) = Reject
delta Two (Just E) = Reject
delta Two Nothing  = Continue (Just E, L, Three)

-- Scan left until we see "□".
delta Three (Just O) = Continue (Just O, L, Three)
delta Three (Just P) = Continue (Just P, L, Three)
delta Three (Just E) = Continue (Just E, L, Three)
delta Three Nothing  = Continue (Nothing, R, Four)

-- If we see "1", erase it and prepare to erase the corresponding "1" on the
-- other side of "=".
-- If we see "+", erase it and continue.
-- If we see "=", prepare for the final check.
-- "□" cannot occur.
delta Four (Just O) = Continue (Nothing, R, Five)
delta Four (Just P) = Continue (Nothing, R, Four)
delta Four (Just E) = Continue (Nothing, R, Eight)
delta Four Nothing  = Reject

-- Scan right until we see "=".
-- "□" cannot occur.
delta Five (Just O) = Continue (Just O, R, Five)
delta Five (Just P) = Continue (Just P, R, Five)
delta Five (Just E) = Continue (Just E, R, Six)
delta Five Nothing  = Reject

-- Scan right until we see "1". Erase it and go back to the beginning.
-- "+" cannot occur.
-- If we see "=", reject.
delta Six (Just O) = Continue (Nothing, L, Seven)
delta Six (Just P) = Reject
delta Six (Just E) = Reject
delta Six Nothing  = Continue (Nothing, R, Six)

-- Scan left until we see "=".
-- "1" and "+" cannot occur.
delta Seven (Just O) = Reject
delta Seven (Just P) = Reject
delta Seven (Just E) = Continue (Just E, L, Three)
delta Seven Nothing  = Continue (Nothing, L, Seven)

-- The final check. Scan right until we see "=", and then accept. Reject on
-- anything else that isn't blank.
delta Eight (Just O) = Reject
delta Eight (Just P) = Reject
delta Eight (Just E) = Accept
delta Eight Nothing  = Continue (Nothing, R, Eight)

unaryAdditionChecker = Machine.make delta Zero
