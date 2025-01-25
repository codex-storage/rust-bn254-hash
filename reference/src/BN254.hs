
-- | The BN254 scalar field

{-# LANGUAGE BangPatterns #-}
module BN254 where

--------------------------------------------------------------------------------

import Data.Bits

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

square :: F -> F
square x = x*x

--------------------------------------------------------------------------------

power :: F -> Integer -> F
power x0 exponent
  | exponent < 0  = error "power: expecting positive exponent"
  | otherwise     = go 1 x0 exponent
  where
    go !acc _ 0 = acc
    go !acc s e = go acc' s' (shiftR e 1) where
      s'   = s*s
      acc' = if e .&. 1 == 0 then acc else acc*s

--------------------------------------------------------------------------------
