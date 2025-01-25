
module Hash where

--------------------------------------------------------------------------------

import BN254

import qualified Poseidon2.Permutation
import qualified Griffin.Permutation

--------------------------------------------------------------------------------

data Hash
  = Poseidon2
  | Griffin
  deriving (Eq,Ord,Show)

--------------------------------------------------------------------------------

permute :: Hash -> (F,F,F) -> (F,F,F)
permute Poseidon2 = Poseidon2.Permutation.permute
permute Griffin   = Griffin.Permutation.permute

--------------------------------------------------------------------------------

compress :: Hash -> (F,F) -> F
compress hash (x,y) = fst3 $ permute hash (x,y,0)

keyedCompress :: Hash -> Word -> (F,F) -> F
keyedCompress hash key (x,y) = fst3 $ permute hash (x,y,fromIntegral key)

--------------------------------------------------------------------------------

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x