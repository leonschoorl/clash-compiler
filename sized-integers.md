# Sized integers

Currently CLaSH always translates Integer to a fixed width (64 or 32 bits, depending on your the machine word size of your compiler and the `-fclash-intwidth` option).

This is incorrect because Integers in Haskell can have any size.

fromIntegral can be used in normal Haskell as a general number conversion function.
But it's implementated by going through Integer: `fromIntegral = fromInteger  . toInteger`



The idea was to have replace the Integer with another type which has some optional size annotation at the type level, kind of like Signed.

```haskell
newtype SInteger (nM :: Maybe Nat) = SI Integer
```

Then when the compiler sees a specific application of
```haskell
toInteger :: Signed n -> Integer
```
We can replace it with:
```haskell
toSInteger :: Signed n -> SInteger ('Just n)
```

`fromInteger` becomes `fromSInteger :: SInteger ('Just n) -> Signed n`

In order the keep the property that Integers can't over/underflow we have the grow the result type of operations like addition (simular to [ExtendingNum])


```haskell
plusInteger :: Integer -> Integer -> Integer
-- gets replaced with
plusSInteger :: SInteger (Just n) -> SInteger (Just m) -> SInteger (Just ((Max n m) + 1))
```





fromIntegral problem might be solvable with RULES, like: https://hackage.haskell.org/package/base-4.10.0.0/docs/src/GHC.Real.html#fromIntegral




[ExtendingNum]: http://hackage.haskell.org/package/clash-prelude-0.11.2/docs/CLaSH-Class-Num.html#t:ExtendingNum
