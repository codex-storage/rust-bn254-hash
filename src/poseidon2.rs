
//use ark_ff::{Field,PrimeField};
use ark_bn254::Fr as F;

use crate::constants::*;

//------------------------------------------------------------------------------

#[derive(Copy, Clone, Debug, PartialEq, Eq)] 
pub struct State { x: F, y: F, z: F } 

//------------------------------------------------------------------------------

fn sbox(x: F) -> F {
  let x2: F = x * x;
  let x4: F = x2*x2;
  x*x4
}

fn linear_layer(u: &mut State) {
  let s = u.x + u.y + u.z;
  u.x += s;
  u.y += s;
  u.z += s;
}

//------------------------------------------------------------------------------

fn internal_round(j: usize, u: &mut State) {
  let x1 = sbox( u.x + INTERNAL_ROUND_CONST[j] );
  let s  = x1 + u.y + u.z;
  u.x = s + x1 ;
  u.y = s + u.y;
  u.z = s + u.z + u.z;
}

fn external_round(j: usize, u: &mut State) {
  let k  = 3*j;
  let x1 = sbox( u.x + EXTERNAL_ROUND_CONST[k+0] );
  let y1 = sbox( u.y + EXTERNAL_ROUND_CONST[k+1] );
  let z1 = sbox( u.z + EXTERNAL_ROUND_CONST[k+2] );
  let s  = x1 + y1 + z1;
  u.x = s + x1;
  u.y = s + y1;
  u.z = s + z1;
}

//------------------------------------------------------------------------------

pub fn permute_inplace(u: &mut State) {
  linear_layer(u);
  for j in 0..4  { external_round(j, u); }
  for j in 0..56 { internal_round(j, u); }
  for j in 4..8  { external_round(j, u); }
}

pub fn permute(u: State) -> State {
  let mut v = u;
  permute_inplace(&mut v);
  v
}

//------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
  use super::*;
  use ark_std::str::FromStr;

  #[test]
  fn permute_kat() {
    let input    = State { x: F::from(0u64), y: F::from(1u64), z: F::from(2u64) };
    let output   = permute(input);
    println!("x = {}",output.x);
    println!("y = {}",output.y);
    println!("z = {}",output.z);
    let expected = State 
          { x: F::from_str("21882471761025344482456282050943515707267606647948403374880378562101343146243").unwrap()
          , y: F::from_str("9030699330013392132529464674294378792132780497765201297316864012141442630280" ).unwrap()
          , z: F::from_str("9137931384593657624554037900714196568304064431583163402259937475584578975855" ).unwrap()
          };
    assert_eq!(output,expected);
  }
}
