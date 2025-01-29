
use criterion::{black_box, criterion_group, criterion_main, Criterion};

use rust_bn254_hash::state::*;
use rust_bn254_hash::hash::*;

//------------------------------------------------------------------------------

fn iterate_perm(h: Hash, n: usize) -> State {
  let mut state = state_012();
  for _i in 0..n {
    permute_inplace(h, &mut state);
  }
  state
}

fn bench_iterated_perm(c: &mut Criterion , h: Hash, n: usize) {
  let msg = format!("{:?} permutation iterated {} times", h, n);
  c.bench_function(&msg, |b| b.iter(|| iterate_perm(h, black_box(n))));
}

//------------------------------------------------------------------------------

fn bench_permutations(c: &mut Criterion) {
  bench_iterated_perm(c, Hash::Poseidon2, 10000);
  bench_iterated_perm(c, Hash::Griffin  , 10000);
  bench_iterated_perm(c, Hash::Skyscraper  , 10000);
}

//------------------------------------------------------------------------------

criterion_group!(benches, bench_permutations);
criterion_main!(benches);

