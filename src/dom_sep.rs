

//
// domain separation.
//
// we use either 
// 
//   IV  := 2^64 + 2^24*padding + 2^16*nbits + 2^8*t + rate
//   IV' := IV + 2^72*input_len
//
// in the capacity position, so the inital state is `(0,0,IV)`
//
// where `nbits` is the number of the bits in the input type:
//   u = 1    -> bit sequence
//   u = 8    -> byte sequence
//   u = 64   -> u64 sequence
//   u = 254  -> BN254 scalar field element sequence 
//
// and padding encodes the padding strategy:
//
//   p = 0    -> no padding (only safe with constant length inputs!)
//   p = 1    -> `10*` padding strategy
//

use ark_bn254::Fr as F;

//------------------------------------------------------------------------------

pub fn domain_separator(rate: usize, state_size: usize, input_bits: usize, padding: usize, mb_len: Option<usize>) -> F {
  let low:  u128 = ( rate + (state_size<<8) + (input_bits<<16) + (padding<<24) ) as u128;
  let domsep  = match mb_len {
    None => {
      let high: u128 = 1 << 64;
      F::from(high + low)
    }
    Some(len) => {
      let high: u128 = (1 + (len<<8) as u128) << 64;
      F::from(high + low)
    }
  };
  // println!("domsep = {:?}", domsep );
  domsep
}

//------------------------------------------------------------------------------

