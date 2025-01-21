
{-# LANGUAGE RecordWildCards #-}
module Sponge where

--------------------------------------------------------------------------------

import BN254
import Permutation

--------------------------------------------------------------------------------

data Config = Config 
  { rate    :: Int       -- ^ rate of the sponge
  , width   :: Int       -- ^ width of the permutation
  , nbits   :: Int       -- ^ number of bits in the input elements (eg: 1,8,64,254)
  , padding :: Int       -- ^ padding strategy (255 => no padding; 1 => @10*@ padding)
  }
  deriving Show

-- | @IV := 2^64 + 2^24*padding + 2^16*nbits + 2^8*t + rate@
domainSep :: Config -> F
domainSep (Config{..}) 
  = fromIntegral rate 
  + fromIntegral width   * (2^8 )
  + fromIntegral nbits   * (2^16)
  + fromIntegral padding * (2^24)
  + fromInteger            (2^64)

--------------------------------------------------------------------------------

-- | Sponge construction with rate=2 (capacity=1), zero IV and 10* padding
feltSpongeWithPad :: [F] -> F
feltSpongeWithPad input = go (0,0,civ) (pad_10star input) where
  -- domain separation capacity IV
  civ = domainSep $ Config { rate = 2 , width = 3 , nbits = 254 , padding = 1 }

  go (sx,_ ,_ ) []         = sx
  go (sx,sy,sz) (a:b:rest) = go state' rest where 
    state' = permute (sx+a, sy+b, sz)

pad_10star :: [F] -> [F]
pad_10star = go where
  go (x:y:rest) = x : y : go rest
  go [x]        = [x,1]
  go []         = [1,0]

--------------------------------------------------------------------------------

-- | Sponge construction with rate=2 (capacity=1), zero IV and no padding
feltSpongeWithoutPad :: [F] -> F
feltSpongeWithoutPad input
  | input == []   = error "the combination of empty input and no padding is not allowed!"
  | otherwise     = fst3 $ go (0,0,civ) input where

  -- domain separation capacity IV
  civ = domainSep $ Config { rate = 2 , width = 3 , nbits = 254 , padding = 255 }

  add (x,y) (sx,sy,sz) = (sx+x, sy+y, sz)

  go state []         =                         state
  go state [a]        =     permute $ add (a,0) state
  go state (a:b:rest) = go (permute $ add (a,b) state) rest

--------------------------------------------------------------------------------
