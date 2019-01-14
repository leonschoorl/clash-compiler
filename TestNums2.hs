{-# LANGUAGE PartialTypeSignatures #-}
module TestNums2 where
import Clash.Prelude
import Data.Word
import Clash.Explicit.Testbench


-- inferred type: expected :: Num b => Vec _ (Unsigned 8, b)

-- writing this type explicitly fixes the error:
-- expected :: Vec _ (Unsigned 8, Word8)
expected = $(lift $
  let
    f :: Integer -> (Unsigned 8, Word8)
    f x = (fromInteger x, fromInteger x)

    -- inputs = replicate d10 0 -- ok
    inputs = replicate d11 0 -- error

  in map f inputs)

topEntity :: Integer -> (Unsigned 8, Word8)
topEntity x = (fromInteger x, fromInteger x)

testBench :: Signal System Bool
testBench = done
  where
    inputs         = replicate d10 0
    testInput      = stimuliGenerator clk rst $ inputs
    expectedOutput = outputVerifier clk rst expected
    done           = expectedOutput ((liftA topEntity) testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
