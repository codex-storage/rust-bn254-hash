
use lazy_static::lazy_static;

use ark_ff::{PrimeField};
use ark_ff::biginteger::{BigInt};
use ark_bn254::Fr as F;

//------------------------------------------------------------------------------

const BIGINT_TWO_TO_64:  BigInt<4> = BigInt( [0,1,0,0] );
const BIGINT_TWO_TO_128: BigInt<4> = BigInt( [0,0,1,0] );
const BIGINT_TWO_TO_192: BigInt<4> = BigInt( [0,0,0,1] );

lazy_static! {
  static ref field_powers_of_two_to_64: [F;3] = 
    [ F::from_bigint(BIGINT_TWO_TO_64 ).unwrap()
    , F::from_bigint(BIGINT_TWO_TO_128).unwrap()
    , F::from_bigint(BIGINT_TWO_TO_192).unwrap()
    ];
}

// we want to consume u64-s (more precisely, Goldilocks field elements, but
// absorbing u64-s is more general). Unfortuntaly, four u64-s just don't
// fit into a BN254 field element, so instead we will pack seven u64-s into
// two field elements (as we are using rate=2).
//
pub fn u64s_to_felts( ws: [u64;7] ) -> (F,F) {
  let hi = ws[6] >> 32;
  let lo = ws[6] & 0xFFFF_FFFF;

  let x =                                F::from(ws[0]) 
        + field_powers_of_two_to_64[0] * F::from(ws[1])
        + field_powers_of_two_to_64[1] * F::from(ws[2])
        + field_powers_of_two_to_64[2] * F::from(lo   );

  let y =                                F::from(ws[3]) 
        + field_powers_of_two_to_64[0] * F::from(ws[4])
        + field_powers_of_two_to_64[1] * F::from(ws[5])
        + field_powers_of_two_to_64[2] * F::from(hi   );

  (x,y)
}

//------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
  use super::*;
  use ark_std::str::FromStr;

   #[test]
  fn u64s_to_felts_kat() {
    let input: [u64; 7] = 
          [ 0x_1234_5678_9abc_def0 
          , 0x_dead_cafe_9876_0123
          , 0x_fc24_78a3_9d06_8818
          , 0x_365d_9035_1967_7d70
          , 0x_4acb_9086_8bb1_b970
          , 0x_df46_f82e_1f13_59d0
          , 0x_3dbb_3705_9b6e_b13f
          ];
    let (x,y) = u64s_to_felts( input );
    assert_eq!( x, F::from_str("16368941413626455547165950020765495089037394581179785942639960710896").unwrap() );
    assert_eq!( y, F::from_str("6501065548289439305465519268972294694155486997049983265694024170864" ).unwrap() );
  }

}
