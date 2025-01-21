

-- | The Poseidon2 permutation

module Permutation where

--------------------------------------------------------------------------------

import Data.Word

import RoundConsts
import BN254

--------------------------------------------------------------------------------

type State a = (a,a,a)

--------------------------------------------------------------------------------

sbox :: F -> F
sbox x = x4*x where
  x2 = x *x
  x4 = x2*x2

internalRound :: F -> State F -> State F 
internalRound c (x,y,z) = 
  ( 2*x' +   y +   z 
  ,   x' + 2*y +   z 
  ,   x' +   y + 3*z 
  )
  where
    x' = sbox (x + c) 

externalRound :: State F -> State F -> State F
externalRound (cx,cy,cz) (x,y,z) = (x'+s , y'+s , z'+s) where
  x' = sbox (x + cx)
  y' = sbox (y + cy)
  z' = sbox (z + cz)
  s  = x' + y' + z'

linearLayer :: State F -> State F
linearLayer (x,y,z) = (x+s, y+s, z+s) where s = x+y+z

--------------------------------------------------------------------------------

permute :: State F -> State F
permute 
  = (\state -> foldl (flip externalRound) state finalRoundConsts   )
  . (\state -> foldl (flip internalRound) state internalRoundConsts)
  . (\state -> foldl (flip externalRound) state initialRoundConsts )
  . linearLayer

--------------------------------------------------------------------------------

compress :: (F,F) -> F
compress (x,y) = fst3 $ permute (x,y,0)

keyedCompress :: Word -> (F,F) -> F
keyedCompress key (x,y) = fst3 $ permute (x,y,fromIntegral key)

--------------------------------------------------------------------------------

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x
