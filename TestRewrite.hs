module TestRewrite where
import Clash.Prelude

-- f :: Int -> Int -> (Int, Int)
f x y = (x+y, x-y)
{-# NOINLINE f #-}

type Nr = Unsigned 4

topEntity :: Int -> Int -> Int
-- topEntity :: Nr -> Nr -> Nr
topEntity x y = snd a
  where
    a = f x y
