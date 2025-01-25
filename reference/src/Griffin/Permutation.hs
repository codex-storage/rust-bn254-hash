
-- | The Griffin permutation for @t=3@

module Griffin.Permutation 
  ( permute 
  ) 
  where

--------------------------------------------------------------------------------

import Data.Array
import Data.List
import Data.Word

import BN254

import Griffin.RoundConsts

--------------------------------------------------------------------------------

type State = (F,F,F)

pow5 :: F -> F
pow5 a = a*a4 where
  a2 = square a
  a4 = square a2

powInv5 :: F -> F
powInv5 a = power a expo_inv

sanityCheckExpoInv =
  [ 13 - pow5 (powInv5 13)
  , 17 - powInv5 (pow5 17)
  ]

sbox :: State -> State
sbox (x,y,z) = (x',y',z') where
  x' = powInv5 x
  y' = pow5    y
  z' = z * (square u + alpha * u + beta)
  u  = x' + y'

addRC :: Int -> State -> State
addRC 0 = id
addRC i = let (a,b,c) = roundConstants!(i-1) in \(x,y,z) -> (x+a, y+b, z+c)

linear :: State -> State
linear (x,y,z) = let s = x+y+z in (x+s, y+s, z+s)

singleRound :: Int -> State -> State
singleRound i = linear . sbox . addRC i

permute :: State -> State
permute input = foldl' (flip singleRound) (linear input) [0..nrounds-1]

-- compress :: F -> F -> F 
-- compress x y = case permute (x,y,0) of (z,_,_) -> z

-- compressPair :: (F,F) -> F 
-- compressPair (x,y) = compress x y

--------------------------------------------------------------------------------

{-
iter :: Int -> (a -> a) -> a -> a
iter n f = go n where
  go  0 !x = x
  go !n !x = go (n-1) (f x)
-}

--------------------------------------------------------------------------------
