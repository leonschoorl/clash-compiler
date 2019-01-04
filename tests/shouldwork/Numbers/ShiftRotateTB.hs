module ShiftRotateTB where
import Clash.Prelude
import Clash.Explicit.Testbench
import ShiftRotate

expected = $(lift $ map (testall pat) inputs)

topEntity = testall

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst $ inputs
    expectedOutput = outputVerifier clk rst expected
    done           = expectedOutput ((liftA2 topEntity) (pure pat) testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
