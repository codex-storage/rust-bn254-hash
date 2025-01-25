

use ark_ff::prelude::{Zero};
use ark_bn254::Fr as F;

//------------------------------------------------------------------------------

#[derive(Copy, Clone, Debug, PartialEq, Eq)] 
pub struct State 
  { pub x: F
  , pub y: F
  , pub z: F 
} 

// cannot be a constant, because stupid rust people...
pub fn zero_state() -> State {
  State 
    { x: F::zero()
    , y: F::zero()
    , z: F::zero()
    }
}

// used as a "default" input state for testing purposes
pub fn state_012() -> State {
  State 
    { x: F::from(0u64)
    , y: F::from(1u64)
    , z: F::from(2u64)
    }
}

//------------------------------------------------------------------------------
