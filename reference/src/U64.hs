
{-# LANGUAGE NumericUnderscores #-}
module U64 where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Word

import BN254

--------------------------------------------------------------------------------

u64sToFelts :: [Word64] -> (F,F)
u64sToFelts ws = case ws of
  [w0,w1,w2,w3,w4,w5,w6] -> (x,y) where
    lo = w6 .&. 0xFFFF_FFFF
    hi = shiftR w6 32
    x = fromIntegral w0
      + fromIntegral w1 * (2^64)
      + fromIntegral w2 * (2^128)
      + fromIntegral lo * (2^192)
    y = fromIntegral w3
      + fromIntegral w4 * (2^64)
      + fromIntegral w5 * (2^128)
      + fromIntegral hi * (2^192)
  _ -> error "u64s_to_field: expecting 7 words"

--------------------------------------------------------------------------------

exampleU64s :: [Word64]
exampleU64s =
  [ 0x_1234_5678_9abc_def0 
  , 0x_dead_cafe_9876_0123
  , 0x_fc24_78a3_9d06_8818
  , 0x_365d_9035_1967_7d70
  , 0x_4acb_9086_8bb1_b970
  , 0x_df46_f82e_1f13_59d0
  , 0x_3dbb_3705_9b6e_b13f
  ]

--------------------------------------------------------------------------------