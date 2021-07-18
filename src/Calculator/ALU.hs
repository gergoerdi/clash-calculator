module Calculator.ALU where

import Clash.Prelude hiding ((^))
import Prelude ((^))
import Test.QuickCheck

type Digit = Index 10
type BCD n = Vec n Digit

fromDigit :: Digit -> Unsigned 4
fromDigit = bitCoerce

addBCD :: BCD n -> BCD n -> BCD n
addBCD xs ys = snd . mapAccumR addDigit False $ zip xs ys
  where
    addDigit :: Bool -> (Digit, Digit) -> (Bool, Digit)
    addDigit c (x, y) = (c', fromIntegral z')
      where
        z = add (fromDigit x) (fromDigit y) + if c then 1 else 0

        (c', z') = if z <= 9 then (False, z) else (True, z - 10)

subBCD :: BCD n -> BCD n -> BCD n
subBCD xs ys = snd . mapAccumR subDigit False $ zip xs ys
  where
    subDigit :: Bool -> (Digit, Digit) -> (Bool, Digit)
    subDigit b (x, y) = (b', fromIntegral z')
      where
        z = sub (fromDigit x) (fromDigit y) - if b then 1 else 0

        (b', z') = if z <= 9 then (False, z) else (True, z + 10)

fromBCD :: BCD n -> Integer
fromBCD = foldl (\x d -> x * 10 + fromIntegral d) 0

infix 4 ~=
(~=) :: forall n. (KnownNat n) => BCD n -> Integer -> Bool
x ~= y = fromBCD x == y `mod` magnitude
  where
    magnitude = 10 ^ natVal (SNat @n)

prop_add :: forall n. (KnownNat n) => BCD n -> BCD n -> Bool
prop_add x y = addBCD x y ~= fromBCD x + fromBCD y

prop_sub :: forall n. (KnownNat n) => BCD n -> BCD n -> Bool
prop_sub x y = subBCD x y ~= fromBCD x - fromBCD y
