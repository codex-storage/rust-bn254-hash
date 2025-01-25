
{-# LANGUAGE RecordWildCards #-}
module Sponge where

--------------------------------------------------------------------------------

import Data.Word

import BN254
import Hash
import U64

--------------------------------------------------------------------------------

data Config = Config 
  { rate    :: Int        -- ^ rate of the sponge
  , width   :: Int        -- ^ width of the permutation
  , nbits   :: Int        -- ^ number of bits in the input elements (eg: 1,8,64,254)
  , padding :: Int        -- ^ padding strategy (255 => no padding; 1 => @10*@ padding)
  , mblen   :: Maybe Int  -- ^ optional length (makes no padding safe, at least for non-empty inputs)
  }
  deriving Show

-- | @IV := 2^64 + 2^24*padding + 2^16*nbits + 2^8*t + rate@
domainSep :: Config -> F
domainSep (Config{..}) 
  = fromIntegral rate 
  + fromIntegral width   * (2^8 )
  + fromIntegral nbits   * (2^16)
  + fromIntegral padding * (2^24)
  + fromInteger  1       * (2^64)
  + fromIntegral len     * (2^72)
  where
    len = case mblen of 
      Nothing -> 0
      Just l  -> l

--------------------------------------------------------------------------------

-- | Sponge construction with rate=2 (capacity=1)), and 10* padding
feltSpongeWithPad :: Hash -> [F] -> F
feltSpongeWithPad hash input = go (0,0,civ) (felts_pad_10star input) where
  -- domain separation capacity IV
  civ = domainSep $ Config { rate = 2 , width = 3 , nbits = 254 , padding = 1 , mblen = Nothing }

  go (sx,_ ,_ ) []         = sx
  go (sx,sy,sz) (a:b:rest) = go state' rest where 
    state' = permute hash (sx+a, sy+b, sz)

  felts_pad_10star :: [F] -> [F]
  felts_pad_10star = go where
    go (x:y:rest) = x : y : go rest
    go [x]        = [x,1]
    go []         = [1,0]

--------------------------------------------------------------------------------

-- | Sponge construction with rate=2 (capacity=1), and no padding
feltSpongeWithNoPad :: Hash -> [F] -> F
feltSpongeWithNoPad hash input
  | null input  = error "the combination of empty input and no padding is not allowed!"
  | otherwise   = fst3 $ go (0,0,civ) input where

  -- domain separation capacity IV
  civ = domainSep $ Config { rate = 2 , width = 3 , nbits = 254 , padding = 255 , mblen = Just (length input) }

  add (x,y) (sx,sy,sz) = (sx+x, sy+y, sz)

  go state []         =                              state
  go state [a]        =     permute hash $ add (a,0) state
  go state (a:b:rest) = go (permute hash $ add (a,b) state) rest

--------------------------------------------------------------------------------

-- | Sponge construction for 64 bit words with rate=2 (capacity=1), and 10* padding
u64SpongeWithPad :: Hash -> [Word64] -> F
u64SpongeWithPad hash input = go (0,0,civ) (chunk_pad_10star input) where
  -- domain separation capacity IV
  civ = domainSep $ Config { rate = 2 , width = 3 , nbits = 64 , padding = 1 , mblen = Nothing }

  go (sx,_ ,_ ) []          = sx
  go (sx,sy,sz) (this:rest) = go state' rest where 
    (a,b)  = u64sToFelts this
    state' = permute hash (sx+a, sy+b, sz)

  chunk_pad_10star :: [Word64] -> [[Word64]]
  chunk_pad_10star = go where
    go ws = case splitAt 7 ws of
      (this,rest) -> if length this == 7
        then this : go rest
        else [take 7 $ this ++ [1] ++ repeat 0]

--------------------------------------------------------------------------------

-- | Sponge construction for 64 bit words with rate=2 (capacity=1), no padding
u64SpongeWithNoPad :: Hash -> [Word64] -> F
u64SpongeWithNoPad hash input 
  | null input  = error "the combination of empty input and no padding is not allowed!"
  | otherwise   = go (0,0,civ) (chunk_nopad input) 
  where
    -- domain separation capacity IV
    civ = domainSep $ Config { rate = 2 , width = 3 , nbits = 64 , padding = 255 , mblen = Just (length input) }
  
    go (sx,_ ,_ ) []          = sx
    go (sx,sy,sz) (this:rest) = go state' rest where 
      (a,b)  = u64sToFelts this
      state' = permute hash (sx+a, sy+b, sz)

chunk_nopad :: [Word64] -> [[Word64]]
chunk_nopad = go where
  go ws = case splitAt 7 ws of
    (this,rest) -> case rest of
      [] -> [take 7 $ this ++ repeat 0]
      _  -> this : go rest

--------------------------------------------------------------------------------
