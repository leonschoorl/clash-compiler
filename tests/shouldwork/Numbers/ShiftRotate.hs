module ShiftRotate where
import Clash.Prelude
import Clash.Explicit.Testbench
import Data.Word
import Data.Int

pat = 0b10011000

inputs = $(listToVecTH [(0::Int)..16])

testall v i
  = ( testAs @(Unsigned 8)  v i shiftsAndRots
    , testAs @(Signed 8)    v i shiftsAndRots
    , testAs @(BitVector 8) v i shiftsAndRots
    -- , testAs @(Word8)       v i shiftsAndRots
    -- , testAs @(Int8)        v i shiftsAndRots
    )
{-# NOINLINE testall #-}

testAs
  :: Num b => Integer -> Int -> Vec n (b -> Int -> b) -> Vec n b
testAs v i = map (\f -> f (fromInteger v) i)

shiftsAndRots :: Bits a => Vec 4 (a -> Int -> a)
shiftsAndRots = shiftL :> shiftR :> rotateL :> rotateR :> Nil
