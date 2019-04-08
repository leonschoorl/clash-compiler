module DDRout where

import Clash.Prelude (mux)
import Clash.Explicit.Prelude
import Clash.Explicit.DDR

type DomReal = Dom "A" 2000 -- real clock domain
type DomDDR  = Dom "A" 1000 -- fake doublespeed domain, used to model a ddr signal

{-
The four variants defined here are all the combinations of
  clock: Gated  or Ungated
  reset: Asynch or Sync
-}

-- | Ignore signal for a number of cycles, while outputting a static value.
ignoreFor
  :: forall dom gated sync n a
   . Clock dom gated
  -> Reset dom sync
  -> SNat n
  -- ^ Number of cycles to ignore incoming signal
  -> a
  -- ^ Value function produces when ignoring signal
  -> Signal dom a
  -- ^ Incoming signal
  -> Signal dom a
ignoreFor clk rst SNat a i =
  mux ((==) <$> counter <*> (pure maxBound)) i (pure a)
 where
  counter :: Signal dom (Index (n+1))
  counter = register clk rst 0 (next <$> counter)

  next c | c == maxBound = maxBound
         | otherwise     = succ c

topEntityGeneric :: Clock DomReal gated
          -> Reset DomReal synchronous
          -> Signal DomReal (Unsigned 8,Unsigned 8)
          -> Signal DomDDR (Unsigned 8)
topEntityGeneric clk rst = ddrOut clk rst 0
-- topEntityGeneric = xilinxOddr
-- topEntityGeneric = altddioOut (SSymbol @"Cyclone IV GX")


topEntityUA :: Clock DomReal Source
          -> Reset DomReal Asynchronous
          -> Signal DomReal (Unsigned 8,Unsigned 8)
          -> Signal DomDDR (Unsigned 8)
topEntityUA = topEntityGeneric

topEntityUS :: Clock DomReal Source
          -> Reset DomReal Synchronous
          -> Signal DomReal (Unsigned 8,Unsigned 8)
          -> Signal DomDDR (Unsigned 8)
topEntityUS = topEntityGeneric

topEntityGA :: Clock DomReal Gated
          -> Reset DomReal Asynchronous
          -> Signal DomReal (Unsigned 8,Unsigned 8)
          -> Signal DomDDR (Unsigned 8)
topEntityGA = topEntityGeneric

topEntityGS :: Clock DomReal Gated
          -> Reset DomReal Synchronous
          -> Signal DomReal (Unsigned 8,Unsigned 8)
          -> Signal DomDDR (Unsigned 8)
topEntityGS = topEntityGeneric


testBenchUS :: Signal DomDDR Bool
testBenchUS = done
  where
    testInput      = stimuliGenerator clkReal rstReal ((0,1):>(2,3):>(4,5):>(6,7):>((8,9)::(Unsigned 8,Unsigned 8)) :> Nil)
    actualOutput   = ignoreFor clkDDR rstDDR d1 0 (topEntityUS clkReal rstReal testInput)
    expectedOutput = outputVerifier clkDDR rstDDR (0:>0:> 0:>1:>2:>3:>4:>5:>6:>7:>8:>(9 :: Unsigned 8) :> Nil)
    done           = expectedOutput actualOutput
    done'          = not <$> done
    clkDDR         = tbClockGen @DomDDR done'
    clkReal        = tbClockGen @DomReal (unsafeSynchronizer clkDDR clkReal done')
    rstDDR         = syncResetGen @DomDDR
    rstReal        = syncResetGen @DomReal

testBenchUA :: Signal DomDDR Bool
testBenchUA = done
  where
    testInput      = stimuliGenerator clkReal rstReal ((0,1):>(2,3):>(4,5):>(6,7):>((8,9)::(Unsigned 8,Unsigned 8)) :> Nil)
    actualOutput   = ignoreFor clkDDR rstDDR d1 0 (topEntityUA clkReal rstReal testInput)
    expectedOutput = outputVerifier clkDDR rstDDR (0:>0:> 0:>1:>2:>3:>4:>5:>6:>7:>8:>(9 :: Unsigned 8) :> Nil)
    done           = expectedOutput actualOutput
    done'          = not <$> done
    clkDDR         = tbClockGen @DomDDR done'
    clkReal        = tbClockGen @DomReal (unsafeSynchronizer clkDDR clkReal done')
    rstDDR         = asyncResetGen @DomDDR
    rstReal        = asyncResetGen @DomReal

testBenchGA :: Signal DomDDR Bool
testBenchGA = done
  where
    testInput      = stimuliGenerator clkReal rstReal ((0,1):>(2,3):>(4,5):>(6,7):>((8,9)::(Unsigned 8,Unsigned 8)) :> Nil)
    actualOutput   = ignoreFor clkDDR rstDDR d1 0 (topEntityGA clkReal rstReal testInput)
    expectedOutput = outputVerifier clkDDR rstDDR (0:>0:> 0:>1:>2:>3:>4:>5:>6:>7:>8:>(9 :: Unsigned 8) :> Nil)
    done           = expectedOutput actualOutput
    done'          = not <$> done
    clkDDR         = let c = tbClockGen @DomDDR done' in clockGate c (pure True)
    clkReal        = let c = tbClockGen @DomReal (unsafeSynchronizer clkDDR clkReal done') in clockGate c (pure True)
    rstDDR         = asyncResetGen @DomDDR
    rstReal        = asyncResetGen @DomReal

testBenchGS :: Signal DomDDR Bool
testBenchGS = done
  where
    testInput      = stimuliGenerator clkReal rstReal ((0,1):>(2,3):>(4,5):>(6,7):>((8,9)::(Unsigned 8,Unsigned 8)) :> Nil)
    actualOutput   = ignoreFor clkDDR rstDDR d1 0 (topEntityGS clkReal rstReal testInput)
    expectedOutput = outputVerifier clkDDR rstDDR (0:>0:> 0:>1:>2:>3:>4:>5:>6:>7:>8:>(9 :: Unsigned 8) :> Nil)
    done           = expectedOutput actualOutput
    done'          = not <$> done
    clkDDR         = let c = tbClockGen @DomDDR done' in clockGate c (pure True)
    clkReal        = let c = tbClockGen @DomReal (unsafeSynchronizer clkDDR clkReal done') in clockGate c (pure True)
    rstDDR         = syncResetGen @DomDDR
    rstReal        = syncResetGen @DomReal
