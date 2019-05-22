module ConstWrapping where
import Clash.Prelude
import Clash.Explicit.Testbench

twoS :: Signed 3
twoS = 10 -- wraps to 2

twoU :: Unsigned 3
twoU = 10 -- wraps to 2

twoBv :: BitVector 3
twoBv = 10 -- wraps to 2

threeN :: Signed 3
threeN = 5 -- wraps to -3

topEntity = ( fromIntegral twoS :: Int
            , zeroExtend twoS :: Signed 4

            , fromIntegral twoU :: Int
            , zeroExtend twoU :: Unsigned 4

            , fromIntegral twoBv :: Int
            , zeroExtend twoBv :: BitVector 4

            , fromIntegral threeN :: Int
            , resize threeN :: Signed 4
            )

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier clk rst ((2,2, 2,2, 2,2, -3,-3) :> Nil)
    done           = expectedOutput (pure topEntity)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
