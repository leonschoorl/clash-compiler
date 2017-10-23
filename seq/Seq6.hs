{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Seq6 where

import Clash.Prelude

type Elm  = Unsigned 8
type Pntr n = Unsigned (n + 1)
type Elms = Vec 4 Elm

fifo :: Vec 4 Elm
     -> Pntr 2
     -> Bool
     -> (Pntr 2,_)
fifo elms rpntr rd = output
  where
    !rpntr'  | rd        = rpntr + 1
             | otherwise = rpntr

    dataout = elms !! rpntr
    output = (rpntr',dataout)
    {- # NOINLINE output #-} -- enabling this noinline magically makes everything work
topEntity :: _
topEntity rpntr rd = fifo (replicate d4 0) rpntr rd


{-
can't eliminate casts because they end up on different sides of a case

needs CaseCast rule (see thesis p.101)
-}
