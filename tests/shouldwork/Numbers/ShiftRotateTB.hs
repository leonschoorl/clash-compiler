{-
This tests the HDL implementations of shiftL,shiftR,rotateL and rotateR
for Unsigned 8,Signed 8,BitVector 8,Word8,Int8
by checking their results against compile-time evaluated calls to the same functions.

So it checks HDL implementations have the same behaviour as the Haskell implementations.
(And it assumes that the Haskell implementations are correct.)

-}
module ShiftRotateTB where
import Clash.Prelude
import Clash.Explicit.Testbench
import Data.Int
import Data.Word
import ShiftRotate

topEntity = testall

expected :: Vec Shifts ( Vec Ops (Unsigned 8)
                       , Vec Ops (Signed 8)
                       , Vec Ops (BitVector 8)
                       , Vec Ops Word8
                       , Vec Ops Int8
                       )
expected = $(lift $ map (testall pat) amounts)

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst amounts
    expectedOutput = outputVerifier clk rst expected
    done           = expectedOutput ((liftA2 topEntity) (pure testpattern) testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
