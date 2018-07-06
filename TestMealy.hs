module TestMealy where

import Clash.Explicit.Prelude hiding (mealy)

topEntity
  :: Clock System Source
  -> Reset System Asynchronous
  -> Signal System Int
  -> Signal System Int
topEntity clk rst = mealie clk rst mac 0

mac :: Int -> Int -> (Int,Int)
mac s i = (s+i, s+i)


-- mealy :: Clock dom gated   -- ^ 'Clock' to synchronize to
--       -> Reset dom synchronous
--       -> (s -> i -> (s,o)) -- ^ Transfer function in mealy machine form:
--                            -- @state -> input -> (newstate,output)@
--       -> s                 -- ^ Initial state
--       -> (Signal dom i -> Signal dom o)
--       -- ^ Synchronous sequential function with input and output matching that
--       -- of the mealy machine
mealie :: Clock System Source   -- ^ 'Clock' to synchronize to
      -> Reset System Asynchronous
      -> (Int -> Int -> (Int,Int)) -- ^ Transfer function in mealy machine form:
                           -- @state -> input -> (newstate,output)@
      -> Int                 -- ^ Initial state
      -> (Signal System Int -> Signal System Int)
mealie clk rst f iS =
  \i -> let (s',o) = unbundle $ f <$> s <*> i
            s      = register clk rst iS s'
        in  o
