
-- | Degree 2 and 3 field extension of the BN254 scalar field

{-# LANGUAGE BangPatterns, DataKinds, TypeFamilies #-}
module Skyscraper.FieldExt where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Word
import Data.Proxy

import BN254

--------------------------------------------------------------------------------

type F2 = Ext2 F
type F3 = Ext3 F

--------------------------------------------------------------------------------

class Num ext => FieldExtension ext where
  dimension   :: Proxy ext -> Int
  scale       :: F -> ext -> ext
  extToList   :: ext -> [F]
  extFromList :: [F] -> ext

instance FieldExtension F where
  dimension = const 1
  scale     = (*)
  extToList   x = [x]
  extFromList [x] = x

--------------------------------------------------------------------------------

-- | @F[X] / (X^2 + 5)@
data Ext2 a 
  = MkExt2 !a !a
  deriving (Eq,Show)

negF2 :: F2 -> F2
negF2 (MkExt2 a0 a1) = MkExt2 (negate a0) (negate a1)

addF2 :: F2 -> F2 -> F2
addF2 (MkExt2 a0 a1) (MkExt2 b0 b1) = MkExt2 (a0+b0) (a1+b1)

mulF2 :: F2 -> F2 -> F2
mulF2 (MkExt2 a0 a1) (MkExt2 b0 b1) = MkExt2 c0 c1 where
  c0 = a0*b0 - 5*a1*b1
  c1 = a0*b1 +   a1*b0

sclF2 :: F -> F2 -> F2
sclF2 s (MkExt2 a0 a1) = MkExt2 (s*a0) (s*a1)

instance Num F2 where
  fromInteger x = MkExt2 (fromInteger x) 0
  negate = negF2
  (+)    = addF2
  (*)    = mulF2
  signum = error "Num/F2/signum"
  abs    = error "Num/F2/abs"

instance FieldExtension F2 where
  dimension = const 2
  scale     = sclF2
  extToList   (MkExt2 x y) = [x,y]
  extFromList [x,y] = MkExt2 x y

----------------------------------------

-- | @F[X] / (X^3 + 3)@
data Ext3 a 
  = MkExt3 !a !a !a
  deriving (Eq,Show)

negF3 :: F3 -> F3
negF3 (MkExt3 a0 a1 a2) = MkExt3 (negate a0) (negate a1) (negate a2)

addF3 :: F3 -> F3 -> F3
addF3 (MkExt3 a0 a1 a2) (MkExt3 b0 b1 b2) = MkExt3 (a0+b0) (a1+b1) (a2+b2)

mulF3 :: F3 -> F3 -> F3
mulF3 (MkExt3 a0 a1 a2) (MkExt3 b0 b1 b2) = MkExt3 c0 c1 c2 where
  c0 = a0*b0 - 3*a2*b1 - 3*a1*b2 
  c1 = a1*b0 +   a0*b1 - 3*a2*b2
  c2 = a2*b0 +   a1*b1 +   a0*b2

sclF3 :: F -> F3 -> F3
sclF3 s (MkExt3 a0 a1 a2) = MkExt3 (s*a0) (s*a1) (s*a2)

instance Num F3 where
  fromInteger x = MkExt3 (fromInteger x) 0 0
  negate = negF3
  (+)    = addF3
  (*)    = mulF3
  signum = error "Num/F3/signum"
  abs    = error "Num/F3/abs"

instance FieldExtension F3 where
  dimension = const 3
  scale     = sclF3
  extToList   (MkExt3 x y z) = [x,y,z]
  extFromList [x,y,z] = MkExt3 x y z

--------------------------------------------------------------------------------
