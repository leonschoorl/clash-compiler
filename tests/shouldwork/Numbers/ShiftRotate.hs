module ShiftRotate where
import Clash.Prelude
import Clash.Explicit.Testbench
import Data.Word
import Data.Int

testpattern = 0b10011000

-- test with shift/rotate amounts: 0..16
type Shifts = 17
amounts :: Vec Shifts Int
amounts = iterateI succ 0

testall v i
  = ( testAs @(Unsigned 8)  v i shiftsAndRots
    , testAs @(Signed 8)    v i shiftsAndRots
    , testAs @(BitVector 8) v i shiftsAndRots
    , testAs @(Word8)       v i shiftsAndRots
    , testAs @(Int8)        v i shiftsAndRots
    )
{-# NOINLINE testall #-}

testAs
  :: Num b => Integer -> Int -> Vec n (b -> Int -> b) -> Vec n b
testAs v i = map (\f -> f (fromInteger v) i)

type Ops = 4

shiftsAndRots :: Bits a => Vec Ops (a -> Int -> a)
shiftsAndRots = shiftL :> shiftR :> rotateL :> rotateR :> Nil
