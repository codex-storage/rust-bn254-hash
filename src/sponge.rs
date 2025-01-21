
use ark_ff::prelude::{Zero,One};
use ark_bn254::Fr as F;

use crate::permutation::*;

//------------------------------------------------------------------------------

//
// domain separation.
// we use 
// 
//   IV := 2^64 + 2^24*padding + 2^16*nbits + 2^8*t + rate
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

fn domain_separator(rate: usize, state_size: usize, input_bits: usize, padding: usize) -> F {
  let low:  u128 = ( rate + (state_size<<8) + (input_bits<<16) + (padding<<24) ) as u128;
  let high: u128 = 1 << 64;
  let domsep = F::from(high + low);
  println!("domsep = {:?}", domsep );
  domsep
}

//------------------------------------------------------------------------------

// hash of field elements with the `10*` padding stategy (rate=2, capacity=1)
pub fn sponge_felts_pad(xs: Vec<F>) -> F {
  let l: usize = xs.len();
  let m: usize = l / 2;
  let mut state: State = zero_state();
  state.z = domain_separator(2, 3, 254, 1);
  for i in 0..m {
    state.x += xs[2*i  ];
    state.y += xs[2*i+1];
    permute_inplace(&mut state);
  }
  if (l & 1) == 0 {
    // even
    state.x += F::one();
    state.y += F::zero();
    permute_inplace(&mut state);
  }
  else {
    // odd
    state.x += xs[2*m];
    state.y += F::one();
    permute_inplace(&mut state);
  }
  state.x  
}

//------------------------------------------------------------------------------

// hash of field elements without padding (rate=2, capacity=1)
pub fn sponge_felts_no_pad(xs: Vec<F>) -> F {
  let l: usize = xs.len();
  assert!( l > 0 , "calling a sponge without padding for an empty input is not allowed");
  let m: usize = l / 2;
  let mut state: State = zero_state();
  state.z = domain_separator(2, 3, 254, 255);
  for i in 0..m {
    state.x += xs[2*i  ];
    state.y += xs[2*i+1];
    permute_inplace(&mut state);
  }
  if (l & 1) == 0 {
    // even
  }
  else {
    // odd
    state.x += xs[2*m];
    permute_inplace(&mut state);
  }
  state.x  
}

//------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
  use super::*;
  use ark_std::str::FromStr;

   #[test]
  fn sponge_felt_pad_kats() {
    let expected_results_rate2: [F; 9] =
      [ F::from_str("19499082269592267598445447545057958665614609297846978854367973542689225942769").unwrap()
      , F::from_str("18575012789231469229884764324941297325518518125159947154666143736134627612926").unwrap()
      , F::from_str("8497041170554791294461959662381834821683188757759653310126334787903454881833" ).unwrap()
      , F::from_str("7477361136953606818741895260704612015724121297282898803513690475311354933324" ).unwrap()
      , F::from_str("124468679175306239235509344657309567209200730228442938605071013597332255858"  ).unwrap()
      , F::from_str("16623067489256565233778087665060282683386099247772442960143578746217625299219").unwrap()
      , F::from_str("11486960019850145257815352297225482939271961443661416961989480881863168607026").unwrap()
      , F::from_str("20420541301992412878354329495337915388337723490334029715201499395107517967097").unwrap()
      , F::from_str("15368493599988308785714434050658408098972196808672741268698522157366881904768").unwrap()
      ];
    for n in 0..9 {
      let mut input: Vec<F> = vec![];
      for i in 0..n { 
        input.push( F::from( (i+1) as u64 ) ); 
      }
      println!("{} -> {:?}",n,input);
      let hash = sponge_felts_pad(input);
      assert_eq!( hash, expected_results_rate2[n] );
    }
  }


  #[test]
  fn sponge_felt_no_pad_kats() {
    let expected_results_rate2: [F; 8] =
      [ F::from_str("12436596435061241139494609254441532101975156984785247438430218269367637564721").unwrap()
      , F::from_str("13776645629657805681320936464215569484592532014724269311132107597767076186080").unwrap()
      , F::from_str("15672651940583329603224775244906975133219543544569309474870924637674069574381").unwrap()
      , F::from_str("18344230412728640899019354443202382658669360934913267412359635786624065034390").unwrap()
      , F::from_str("17677852938281982781904616714585735700373643150759519434566383135318143611879").unwrap()
      , F::from_str("5497863312684999074711437591834873546200107612265319209549992389042679138148" ).unwrap()
      , F::from_str("18463022787715726219296039697387566904347367917209714404058652712879402042682").unwrap()
      , F::from_str("3701269753452043827901155479778455778316871093798312420317871137452978084903" ).unwrap()
      ];
    for n in 1..9 {
      let mut input: Vec<F> = vec![];
      for i in 0..n { 
        input.push( F::from( (i+1) as u64 ) ); 
      }
      println!("{} -> {:?}",n,input);
      let hash = sponge_felts_no_pad(input);
      assert_eq!( hash, expected_results_rate2[n-1] );
    }
  }


}
