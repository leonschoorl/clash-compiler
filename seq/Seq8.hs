{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Seq8 where

import Clash.Prelude


f :: Unsigned 3
  -> Bool
  -> Unsigned 3
-- f rpntr rdd = rpntr + 1
--   where
--     !unused  | rdd        = not rdd -- rpntr + 1
--             --  | otherwise = rpntr

f rpntr rdd = unused `seq` (rpntr + 1)
  where
    unused  | rdd       = not rdd -- rpntr + 1
            | rpntr == 3 = False
            | rpntr == 4 = True
            | otherwise = rpntr == 1 -- rpntr


topEntity :: _
topEntity rpntr rd = f rpntr rd
