{-# LANGUAGE BangPatterns #-}
module SatMult where

import Clash.Prelude

topEntity :: Signed 3 -> Signed 3 -> Signed 3
topEntity !a = satMult SatSymmetric a
