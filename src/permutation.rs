
use ark_ff::prelude::{Zero};
use ark_bn254::Fr as F;

use crate::constants::*;

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

  //
  // the known-answer tests are derived from a Haskell reference implementation.
  //
  // the implementation USED TO BE compatible with <https://github.com/HorizenLabs/poseidon2>
  //
  // UNTIL they replaced the constants for some mysterious reasons in
  // <https://github.com/HorizenLabs/poseidon2/commit/bb476b9ca38198cf5092487283c8b8c5d4317c4e>
  //
  // this other repo: <https://extgit.isec.tugraz.at/krypto/zkfriendlyhashzoo> 
  // still seems to use the old constants, so we are compatible with that one.
  //

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

  #[test]
  fn compress_kats() {
    let output12 = compress( F::from(1u64) , F::from(2u64) );
    let output34 = compress( F::from(3u64) , F::from(4u64) );
    assert_eq!( output12 , F::from_str("19016777872907576614311050386253173408873004339574005400462870923562919648648").unwrap() );
    assert_eq!( output34 , F::from_str("3434170624647484818537756522365043680699152308844695523441164165626957081541") .unwrap() );
  }

  #[test]
  fn keyed_compress_kats() {
    let f101 = F::from(101u64);
    let f102 = F::from(102u64);
    let reference_key0 = compress      (     f101, f102);
    let output_key0    = keyed_compress(0  , f101, f102);
    let output_key666  = keyed_compress(666, f101, f102);
    assert_eq!( output_key0   , reference_key0 );
    assert_eq!( output_key0   , F::from_str("15961426645349780581129468645644877710270210656040701384072931112530046991167").unwrap() );
    assert_eq!( output_key666 , F::from_str("10054688273989230693209556876964057047824107930668002716115163490706030927433").unwrap() );
  }

}
