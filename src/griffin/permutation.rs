
// the Griffin permutation for `t=3`

use ark_ff::{Field};
use ark_ff::prelude::{Zero};
use ark_bn254::Fr as F;

use crate::griffin::constants::*;
use crate::state::*;

//------------------------------------------------------------------------------

/*
const EXPO_D_INV_BE: [u64; 4] = 
  [ 0x26b6a528b427b354u64
  , 0x93736af8679aad17u64
  , 0x535cb9d394945a0du64
  , 0xcfe7f7a98ccccccdu64 
  ];
*/

const EXPO_D_INV_LE: [u64; 4] = 
  [ 0xcfe7f7a98ccccccdu64 
  , 0x535cb9d394945a0du64
  , 0x93736af8679aad17u64
  , 0x26b6a528b427b354u64
  ];

//------------------------------------------------------------------------------

fn pow_5(x: F) -> F {
  let x2: F = x * x;
  let x4: F = x2*x2;
  x*x4
}

fn pow_inv_5(x: F) -> F {
  x.pow( EXPO_D_INV_LE )
}

//------------------------------------------------------------------------------

fn linear_layer(u: &mut State) {
  let s = u.x + u.y + u.z;
  u.x += s;
  u.y += s;
  u.z += s;
}

//------------------------------------------------------------------------------

fn round_fun(j: usize, u: &mut State) {
  if j > 0 {
    let k = 3*(j-1);
    u.x += ROUND_CONST[k+0];
    u.y += ROUND_CONST[k+1];
    u.z += ROUND_CONST[k+2];
  }
  u.x = pow_inv_5(u.x);
  u.y = pow_5    (u.y);
  let t = u.x + u.y;
  let m = t*t + ALPHA_BETA[0]*t + ALPHA_BETA[1];
  u.z *= m;  
  linear_layer(u);
}

//------------------------------------------------------------------------------

pub fn permute_inplace(u: &mut State) {
  linear_layer(u);
  for j in 0..12  { round_fun(j, u); }
}

pub fn permute(u: State) -> State {
  let mut v = u;
  permute_inplace(&mut v);
  v
}

//------------------------------------------------------------------------------

pub fn compress(x: F, y: F) -> F {
  let mut u = State { x: x, y: y, z: F::zero() };
  permute_inplace(&mut u);
  u.x
}

pub fn keyed_compress(key: u64, x: F, y: F) -> F {
  let mut u = State { x: x, y: y, z: F::from(key) };
  permute_inplace(&mut u);
  u.x
}

//------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
  use super::*;
  use ark_std::str::FromStr;

  #[test]
  fn sanity_check_d_inv() {
    let x = F::from( 1234567u64 );
    let y = pow_5    (x);
    let z = pow_inv_5(y);
    assert_eq!(x,z);
    let x = F::from( 7890123u64 );
    let y = pow_inv_5(x);
    let z = pow_5    (y);
    assert_eq!(x,z);
  }


  #[test]
  fn permute_kat() {
    let input    = State { x: F::from(0u64), y: F::from(1u64), z: F::from(2u64) };
    let output   = permute(input);
    // println!("x = {}",output.x);
    // println!("y = {}",output.y);
    // println!("z = {}",output.z);
    let expected = State 
          { x: F::from_str("15862405785128810275837435502653224425290071258167230490599117376332100235254").unwrap()
          , y: F::from_str("13220756517509979517684528785753328587257706928708746278499548208567338458968").unwrap()
          , z: F::from_str("15550532036911446928426039913328049280239190234626561457568755196858003615133").unwrap()
          };
    assert_eq!(output,expected);
  }

}
