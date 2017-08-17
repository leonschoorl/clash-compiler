module BigIntegers where

import CLaSH.Prelude

topEntity :: Signed 70 -> Signed 70
topEntity = fromIntegral
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = stimuliGenerator $(listToVecTH ([2^65]::[Signed 70]))
    expectedOutput = outputVerifier   $(listToVecTH ([2^65]::[Signed 70]))
    done           = expectedOutput (topEntity <$> testInput)
    done'          = withClockReset (tbSystemClock (not <$> done')) systemReset done
