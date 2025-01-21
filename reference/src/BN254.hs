
-- | The BN254 scalar field

module BN254 where

--------------------------------------------------------------------------------

newtype F = MkF Integer deriving (Eq,Show)

fromF :: F -> Integer
fromF (MkF x) = x

toF :: Integer -> F
toF = MkF . modP

fieldPrime :: Integer
fieldPrime = 21888242871839275222246405745257275088548364400416034343698204186575808495617

modP :: Integer -> Integer
modP x = mod x fieldPrime

instance Num F where
  fromInteger = toF . fromInteger
  negate (MkF x)         = toF (negate x)
  (+)    (MkF x) (MkF y) = toF (x+y)
  (-)    (MkF x) (MkF y) = toF (x-y)
  (*)    (MkF x) (MkF y) = toF (x*y)
  abs    x = x
  signum _ = toF 1  

--------------------------------------------------------------------------------