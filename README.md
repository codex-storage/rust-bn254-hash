
`rust-bn254-hash`
---------------

Rust implementation of some arithmetic hash functions over the BN254 curve's scalar field,
with permutation state width `t=3`. 

Hash functions implemented:

- [x] Poseidon2
- [x] Griffin
- [x] skyscraper

We plan to integrate this into Plonky2, to allow an efficient BN254 recursive wrapper.
For this we need some custom features: For example when computing the Fiat-Shamir
challenges, we need a duplex construction which can absorb and squeeze both
BN254 and Goldilocks field elements.

Currently using [arkworks](https://github.com/arkworks-rs) for field arithmetic
(maybe replace it with something more light-weight in the future...)

### Performance

Single core, unoptimized, MacBook Pro M2.

1000 permutations:

- 1000 Poseidon2 permutations: 5.24 msec (approx 12MB/sec linear hashing)
- 1000 Griffin permutations: 76.8 msec  (appox 800k/sec linear hashing)

10000 permutations:

- 10000 Poseidon2 permutations: 53 msec
- 10000 Griffin permutations: 762 sec
