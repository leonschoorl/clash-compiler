{-# LANGUAGE PartialTypeSignatures #-}
module ShiftRotateTB where
import Clash.Prelude
import Clash.Explicit.Testbench
import Data.Int
import Data.Word
import ShiftRotate

expected :: Vec Shifts ( Vec Ops (Unsigned 8)
                       , Vec Ops (Signed 8)
                       , Vec Ops (BitVector 8)
                       , Vec Ops Word8
                       , Vec Ops Int8
                       )
expected = $(lift $ map (testall pat) inputs)

topEntity = testall

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst inputs
    expectedOutput = outputVerifier clk rst expected
    done           = expectedOutput ((liftA2 topEntity) (pure pat) testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
