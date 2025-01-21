
`rust-poseidon2-bn254`
--------------------

Rust implementation of the Poseidon2 hash function over the BN254 curve's scalar field,
with permutation state width `t=3`. 

We plan to integrate this into Plonky2, to allow an efficient BN254 recursive wrapper.
For this we need some custom features: For example when computing the Fiat-Shamir
challenges, we need a duplex construction which can absorb and squeeze both
BN254 and Goldilocks field elements.

Currently using [arkworks](https://github.com/arkworks-rs) for field arithmetic
(maybe replace it with something more light-weight in the future...)


