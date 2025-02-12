
{-# LANGUAGE BangPatterns, DataKinds, TypeFamilies, ScopedTypeVariables, TypeApplications #-}
module Skyscraper.Permutation where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Word

import Data.Kind
import Data.Proxy

import BN254
import Skyscraper.RoundConst
import Skyscraper.FieldExt

--------------------------------------------------------------------------------

data SkyWidth
  = Sky2          -- ^ 2 field elements
  | Sky4          -- ^ degree 2 field extension (= 4 field elements)
  | Sky6          -- ^ degree 3 field extension (= 6 field elements)
  deriving (Eq,Show)

--------------------------------------------------------------------------------

type Digest = F

type State sky = (FieldExt sky, FieldExt sky)

stateFromList :: forall (sky :: SkyWidth). SkyscraperImpl sky => Proxy sky -> [F] -> State sky
stateFromList pxy xs = case splitAt (dimension $ Proxy @(FieldExt sky)) xs of
  (us,vs) -> (extFromList us, extFromList vs)

stateToList  :: SkyscraperImpl sky => Proxy sky -> State sky -> [F]
stateToList pxy (x,y) = extToList x ++ extToList y

extractDigest :: SkyscraperImpl sky => Proxy sky -> State sky -> F
extractDigest pxy = head . stateToList pxy

initialState :: SkyscraperImpl sky => Proxy sky -> State sky 
initialState pxy = stateFromList pxy $ replicate (rate pxy) 0 ++ [capacityIV pxy]

--------------------------------------------------------------------------------

class FieldExtension (FieldExt sky) => SkyscraperImpl (sky :: SkyWidth) where

  type FieldExt sky :: Type

  stateWidth :: Proxy sky -> Int
  rate       :: Proxy sky -> Int
  capacity   :: Proxy sky -> Int
  capacityIV :: Proxy sky -> F

  barsOnly   :: Proxy sky -> FieldExt sky -> FieldExt sky
  roundConst :: Proxy sky -> Int -> FieldExt sky

  capacity   pxy = 1
  rate       pxy = stateWidth pxy - capacity pxy
  stateWidth pxy = capacity pxy + rate pxy

--------------------------------------------------------------------------------

bars :: SkyscraperImpl sky => Proxy sky -> FieldExt sky -> State sky -> State sky
bars pxy rc (l,r) = (l',r') where
  l' = r + barsOnly pxy l + rc
  r' = l 

sq :: SkyscraperImpl sky => Proxy sky -> FieldExt sky -> State sky -> State sky
sq pxy rc (l,r) = (l',r') where
  l' = r + scale invMontMultiplier (l*l) + rc
  r' = l 

permute :: SkyscraperImpl sky => Proxy sky -> State sky -> State sky
permute pxy 
  = sq   pxy (roundConst pxy 9)
  . sq   pxy (roundConst pxy 8)
  . bars pxy (roundConst pxy 7)
  . bars pxy (roundConst pxy 6)
  . sq   pxy (roundConst pxy 5)
  . sq   pxy (roundConst pxy 4)
  . bars pxy (roundConst pxy 3)
  . bars pxy (roundConst pxy 2)
  . sq   pxy (roundConst pxy 1)
  . sq   pxy (roundConst pxy 0)

--------------------------------------------------------------------------------

compress :: SkyscraperImpl sky => Proxy sky -> Digest -> Digest -> Digest
compress pxy x y = extractDigest pxy $ permute pxy $ input where
  input = stateFromList pxy $ [x,y] ++ replicate (stateWidth pxy - 2) 0

spongeWithPad :: SkyscraperImpl sky => Proxy sky -> [F] -> Digest
spongeWithPad pxy xs = spongeNoPad pxy (xs ++ [1])

spongeNoPad :: forall (sky :: SkyWidth). SkyscraperImpl sky => Proxy sky -> [F] -> Digest
spongeNoPad _   []   = error "spongeNoPad: empty input"
spongeNoPad pxy list = extractDigest pxy $ go (initialState pxy) list where
  go :: State sky -> [F] -> State sky
  go state []   = state
  go state what = case splitAt (rate pxy) what of
    (this,rest) -> go (permute pxy (mixDataWithState pxy this state)) rest

mixDataWithState :: SkyscraperImpl sky => Proxy sky -> [F] -> State sky -> State sky
mixDataWithState pxy input state
  | m == 0    = error "mixDataWithState: input is empty"
  | m >  r    = error "mixDataWithState: input longer than the rate"
  | otherwise = stateFromList pxy $ mix $ stateToList pxy state
  where
    r = rate pxy
    m = length input
    mix old = zipWith (+) old (input ++ repeat 0)

--------------------------------------------------------------------------------

instance SkyscraperImpl Sky2 where
  type FieldExt Sky2 = F
  stateWidth _ = 2
  capacity   _ = 1
  capacityIV _ = 1
  barsOnly   _ = barsOnly1
  roundConst _ = rc1

instance SkyscraperImpl Sky4 where
  type FieldExt Sky4 = F2
  stateWidth _ = 4
  capacity   _ = 1
  capacityIV _ = 2
  barsOnly   _ = barsOnly2
  roundConst _ = rc2

instance SkyscraperImpl Sky6 where
  type FieldExt Sky6 = F3
  stateWidth _ = 6
  capacity   _ = 1
  capacityIV _ = 3
  barsOnly   _ = barsOnly3
  roundConst _ = rc3

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


