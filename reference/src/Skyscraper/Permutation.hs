
{-# LANGUAGE BangPatterns #-}
module Skyscraper.Permutation where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Word

import BN254
import Skyscraper.RoundConst

--------------------------------------------------------------------------------

type F2 = Ext2 F
type F3 = Ext3 F

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

--------------------------------------------------------------------------------

sboxByte :: Word8 -> Word8
sboxByte y = rol1 $ y `xor` (rol1 ny .&. rol2 y .&. rol3 y) where
  ny   = complement y
  rol1 = flip rotateL 1
  rol2 = flip rotateL 2
  rol3 = flip rotateL 3

--------------------------------------------------------------------------------

integerToBytesLE :: Integer -> [Word8]
integerToBytesLE = go 32 where
  go :: Int -> Integer -> [Word8]
  go  0  0 = []
  go  0  _ = error "integerToBytesLE: does not fit into 32 bytes"
  go !k !x = fromInteger (x .&. 255) : go (k-1) (shiftR x 8)

integerToBytesBE :: Integer -> [Word8]
integerToBytesBE = reverse . integerToBytesLE

integerFromBytesLE :: [Word8] -> Integer
integerFromBytesLE = go where
  go []      = 0
  go (!x:xs) = fromIntegral x + shiftL (go xs) 8

integerFromBytesBE :: [Word8] -> Integer
integerFromBytesBE = integerFromBytesLE . reverse

fieldToBytes :: F -> [Word8]
fieldToBytes (MkF x) = integerToBytesBE x

fieldFromBytes :: [Word8] -> F
fieldFromBytes = toF . integerFromBytesBE

--------------------------------------------------------------------------------

rotateLeft :: Int -> [a] -> [a]
rotateLeft k xs = drop k xs ++ take k xs

partition :: Int -> [a] -> [[a]]
partition k = go where
  go [] = []
  go xs = take k xs : go (drop k xs)

--------------------------------------------------------------------------------

barsOnly1 :: F -> F
barsOnly1 input = output where
  decomp = fieldToBytes input
  rot    = rotateLeft 16 decomp
  sbox   = map sboxByte rot
  output = fieldFromBytes sbox

barsOnly2 :: Ext2 F -> Ext2 F
barsOnly2 (MkExt2 inp0 inp1) = MkExt2 out0 out1 where
  decomp    = fieldToBytes inp0 ++ fieldToBytes inp1
  rot       = rotateLeft 16 decomp
  sbox      = map sboxByte rot
  [ys0,ys1] = partition 32 sbox
  out0      = fieldFromBytes ys0
  out1      = fieldFromBytes ys1

barsOnly3 :: Ext3 F -> Ext3 F
barsOnly3 (MkExt3 inp0 inp1 inp2) = MkExt3 out0 out1 out2 where
  decomp        = fieldToBytes inp0 ++ fieldToBytes inp1 ++ fieldToBytes inp2
  rot           = rotateLeft 16 decomp
  sbox          = map sboxByte rot
  [ys0,ys1,ys2] = partition 32 sbox
  out0          = fieldFromBytes ys0
  out1          = fieldFromBytes ys1
  out2          = fieldFromBytes ys2

--------------------------------------------------------------------------------

bars1 :: F -> (F,F) -> (F,F)
bars1 rc (l,r) = (l',r') where
  l' = r + barsOnly1 l + rc
  r' = l 

bars2 :: F2 -> (F2,F2) -> (F2,F2)
bars2 rc (l,r) = (l',r') where
  l' = r + barsOnly2 l + rc
  r' = l   

bars3 :: F3 -> (F3,F3) -> (F3,F3)
bars3 rc (l,r) = (l',r') where
  l' = r + barsOnly3 l + rc
  r' = l   

--------------------------------------------------------------------------------

sq1 :: F -> (F,F) -> (F,F)
sq1 rc (l,r) = (l',r') where
  l' = r + invMontMultiplier*l*l + rc
  r' = l 

sq2 :: F2 -> (F2,F2) -> (F2,F2)
sq2 rc (l,r) = (l',r') where
  l' = r + sclF2 invMontMultiplier (l*l) + rc
  r' = l 

sq3 :: F3 -> (F3,F3) -> (F3,F3)
sq3 rc (l,r) = (l',r') where
  l' = r + sclF3 invMontMultiplier (l*l) + rc
  r' = l 
  
--------------------------------------------------------------------------------

lkpKst :: Int -> F
lkpKst k = roundConstF !! k

rc1 :: Int -> F
rc1 k 
  | k == 0 || k == 9 = 0
  | k <  0 || k >  9 = error "rc1: round counter out of range"
  | otherwise        = let i = k-1 in lkpKst i

rc2 :: Int -> F2
rc2 k 
  | k == 0 || k == 9 = 0
  | k <  0 || k >  9 = error "rc2: round counter out of range"
  | otherwise        = let i = k-1 in MkExt2
                         (lkpKst (2*i  ))
                         (lkpKst (2*i+1))

rc3 :: Int -> F3
rc3 k 
  | k == 0 || k == 9 = 0
  | k <  0 || k >  9 = error "rc3: round counter out of range"
  | otherwise        = let i = k-1 in MkExt3
                         (lkpKst (3*i  ))
                         (lkpKst (3*i+1))
                         (lkpKst (3*i+2))

--------------------------------------------------------------------------------

perm1 :: (F,F) -> (F,F)
perm1  
  = sq1   (rc1 9)
  . sq1   (rc1 8)
  . bars1 (rc1 7)
  . bars1 (rc1 6)
  . sq1   (rc1 5)
  . sq1   (rc1 4)
  . bars1 (rc1 3)
  . bars1 (rc1 2)
  . sq1   (rc1 1)
  . sq1   (rc1 0)

perm2 :: (F2,F2) -> (F2,F2)
perm2  
  = sq2   (rc2 9)
  . sq2   (rc2 8)
  . bars2 (rc2 7)
  . bars2 (rc2 6)
  . sq2   (rc2 5)
  . sq2   (rc2 4)
  . bars2 (rc2 3)
  . bars2 (rc2 2)
  . sq2   (rc2 1)
  . sq2   (rc2 0)

perm3 :: (F3,F3) -> (F3,F3)
perm3  
  = sq3   (rc3 9)
  . sq3   (rc3 8)
  . bars3 (rc3 7)
  . bars3 (rc3 6)
  . sq3   (rc3 5)
  . sq3   (rc3 4)
  . bars3 (rc3 3)
  . bars3 (rc3 2)
  . sq3   (rc3 1)
  . sq3   (rc3 0)

--------------------------------------------------------------------------------
