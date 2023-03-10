[![build](https://github.com/sergv/atomic-counter/actions/workflows/haskell-ci.yaml/badge.svg)](https://github.com/sergv/atomic-counter/actions/workflows/haskell-ci.yaml)

# Synopsis

Mutable cells that hold an integer value and can be safely modified from
multiple threads. Support only few operations: read, write, +, -, and
bitwise and, or, xor and nand.

Good use case is a shared counter that multiple threads increment.

Operations translate to atomic CPU instructions which involve memory
barrier. For limited set of operation this package provides in
concurrent setting they tend to be faster than atomic modify operation
on `IORef`s and other shared var types. Value inside the `Counter`
type is unboxed which contributes to outperforming all other shared
vars because they box the value they store. For integers boxing is
especially bad.

For single-threaded use case this package will likely not outperform
vanilla `IORef`s or `STRef`s (it depends on whether GHC adds memory
barrier for them or not, cf
https://gitlab.haskell.org/ghc/ghc/-/issues/22764, if it does end up
adding the same memory barrier then both this package and `IORef`
should be equal). All in all, operations this package provides will
incur guaranteed memory barrier which will serve no purpose in
single-threaded settings so for that use case this package is probably
not the best. The intended use of this package is in the concurrent
setting where it does seem like a clear winner (please see benchmarks
below).

# Benchmark

### Summary

Depending on number of threads `Counter` from this package can be
up to 10 times faster than next best `TVar` from the `stm` package.

### Details

The benchmark is to spawn N threads each of which will increment the
same counter by 1 for a number of iterations.

Test setup: Intel i5-9600K CPU @ 3.70GHz, 6 cores, no hyperthreading, GHC 9.4.4.

NB `IORef inconsistent` is the benchmark that just reads and writes
`IORef` with the fastest functions the `IORef` supports. It gives
wrong results when multiple threads access `IORef` this way since
these reads and writes are not synchronized. It???s included only for
speed comparison purposes. All others do proper synchronization of
increments between the threads. Current package is shown as `Counter`.

```
$ cabal run bench -- -j1 --timeout 30
All
  Correctness:          OK (15.95s)
    +++ OK, passed 10000 tests.
  Read/write contention with 10 iterations and 1 threads
    Counter:            OK (0.45s)
      814  ns ??  52 ns
    IORef inconsistent: OK (0.61s)
      1.10 ??s ??  70 ns, 1.35x
    IORef atomic:       OK (1.26s)
      1.19 ??s ??  25 ns, 1.46x
    MVar:               OK (0.64s)
      1.18 ??s ??  39 ns, 1.46x
    TMVar:              OK (0.52s)
      914  ns ??  49 ns, 1.12x
    TVar:               OK (0.63s)
      1.14 ??s ??  73 ns, 1.40x
    Addr:               OK (1.63s)
      734  ns ??  56 ns, 0.90x
  Read/write contention with 100 iterations and 1 threads
    Counter:            OK (1.36s)
      1.21 ??s ??  26 ns
    IORef inconsistent: OK (0.27s)
      3.82 ??s ?? 341 ns, 3.15x
    IORef atomic:       OK (0.44s)
      2.96 ??s ?? 261 ns, 2.44x
    MVar:               OK (0.43s)
      3.11 ??s ?? 172 ns, 2.56x
    TMVar:              OK (0.16s)
      4.56 ??s ?? 412 ns, 3.76x
    TVar:               OK (0.64s)
      4.43 ??s ?? 261 ns, 3.65x
    Addr:               OK (0.60s)
      1.09 ??s ??  78 ns, 0.90x
  Read/write contention with 1000 iterations and 1 threads
    Counter:            OK (0.35s)
      4.84 ??s ?? 377 ns
    IORef inconsistent: OK (0.32s)
      18.7 ??s ?? 836 ns, 3.86x
    IORef atomic:       OK (1.35s)
      19.5 ??s ?? 713 ns, 4.02x
    MVar:               OK (0.14s)
      15.7 ??s ?? 1.4 ??s, 3.24x
    TMVar:              OK (0.20s)
      43.8 ??s ?? 3.3 ??s, 9.07x
    TVar:               OK (0.14s)
      31.4 ??s ?? 3.0 ??s, 6.49x
    Addr:               OK (0.37s)
      4.97 ??s ?? 348 ns, 1.03x
  Read/write contention with 10000 iterations and 1 threads
    Counter:            OK (2.95s)
      43.0 ??s ?? 689 ns
    IORef inconsistent: OK (0.20s)
      185  ??s ??  15 ??s, 4.30x
    IORef atomic:       OK (0.44s)
      214  ??s ??  18 ??s, 4.97x
    MVar:               OK (2.63s)
      154  ??s ?? 1.6 ??s, 3.58x
    TMVar:              OK (0.42s)
      407  ??s ??  11 ??s, 9.45x
    TVar:               OK (0.16s)
      302  ??s ??  23 ??s, 7.02x
    Addr:               OK (0.18s)
      42.0 ??s ?? 3.4 ??s, 0.98x
  Read/write contention with 10 iterations and 2 threads
    Counter:            OK (0.23s)
      6.97 ??s ?? 487 ns
    IORef inconsistent: OK (0.48s)
      7.78 ??s ?? 660 ns, 1.12x
    IORef atomic:       OK (0.29s)
      7.84 ??s ?? 662 ns, 1.12x
    MVar:               OK (0.13s)
      7.24 ??s ?? 684 ns, 1.04x
    TMVar:              OK (2.54s)
      9.33 ??s ?? 358 ns, 1.34x
    TVar:               OK (0.14s)
      7.50 ??s ?? 724 ns, 1.08x
    Addr:               OK (0.28s)
      7.75 ??s ?? 470 ns, 1.11x
  Read/write contention with 100 iterations and 2 threads
    Counter:            OK (0.30s)
      8.35 ??s ?? 567 ns
    IORef inconsistent: OK (0.18s)
      12.5 ??s ?? 902 ns, 1.49x
    IORef atomic:       OK (0.17s)
      12.2 ??s ?? 903 ns, 1.47x
    MVar:               OK (0.12s)
      14.6 ??s ?? 1.4 ??s, 1.74x
    TMVar:              OK (0.34s)
      31.9 ??s ?? 1.6 ??s, 3.83x
    TVar:               OK (0.66s)
      28.8 ??s ?? 1.8 ??s, 3.45x
    Addr:               OK (0.14s)
      8.09 ??s ?? 689 ns, 0.97x
  Read/write contention with 1000 iterations and 2 threads
    Counter:            OK (0.12s)
      48.8 ??s ?? 2.8 ??s
    IORef inconsistent: OK (1.14s)
      123  ??s ?? 3.0 ??s, 2.52x
    IORef atomic:       OK (0.15s)
      256  ??s ??  22 ??s, 5.25x
    MVar:               OK (2.40s)
      4.98 ms ?? 337 ??s, 102.10x
    TMVar:              OK (0.29s)
      240  ??s ??  24 ??s, 4.92x
    TVar:               OK (0.17s)
      288  ??s ??  25 ??s, 5.91x
    Addr:               OK (0.23s)
      47.4 ??s ?? 1.9 ??s, 0.97x
  Read/write contention with 10000 iterations and 2 threads
    Counter:            OK (0.18s)
      626  ??s ??  34 ??s
    IORef inconsistent: OK (0.09s)
      1.39 ms ?? 136 ??s, 2.23x
    IORef atomic:       OK (10.19s)
      2.03 ms ?? 1.1 ms, 3.24x
    MVar:               OK (0.82s)
      57.8 ms ?? 2.7 ms, 92.36x
    TMVar:              OK (0.09s)
      2.37 ms ?? 189 ??s, 3.78x
    TVar:               OK (0.21s)
      2.98 ms ?? 281 ??s, 4.76x
    Addr:               OK (0.29s)
      538  ??s ??  22 ??s, 0.86x
  Read/write contention with 10 iterations and 4 threads
    Counter:            OK (0.09s)
      15.6 ??s ?? 1.4 ??s
    IORef inconsistent: OK (0.16s)
      16.0 ??s ?? 776 ns, 1.03x
    IORef atomic:       OK (0.34s)
      16.9 ??s ?? 1.6 ??s, 1.08x
    MVar:               OK (0.18s)
      25.5 ??s ?? 1.4 ??s, 1.64x
    TMVar:              OK (0.17s)
      19.9 ??s ?? 1.2 ??s, 1.27x
    TVar:               OK (0.17s)
      18.1 ??s ?? 1.1 ??s, 1.16x
    Addr:               OK (0.07s)
      14.0 ??s ?? 1.4 ??s, 0.90x
  Read/write contention with 100 iterations and 4 threads
    Counter:            OK (0.20s)
      23.4 ??s ?? 1.6 ??s
    IORef inconsistent: OK (0.09s)
      56.3 ??s ?? 3.3 ??s, 2.41x
    IORef atomic:       OK (0.42s)
      143  ??s ??  10 ??s, 6.11x
    MVar:               OK (0.16s)
      1.19 ms ?? 112 ??s, 51.05x
    TMVar:              OK (0.25s)
      195  ??s ??  19 ??s, 8.36x
    TVar:               OK (0.14s)
      202  ??s ??  14 ??s, 8.65x
    Addr:               OK (0.20s)
      22.5 ??s ?? 1.7 ??s, 0.96x
  Read/write contention with 1000 iterations and 4 threads
    Counter:            OK (0.16s)
      231  ??s ??  18 ??s
    IORef inconsistent: OK (0.04s)
      453  ??s ??  42 ??s, 1.96x
    IORef atomic:       OK (0.47s)
      1.36 ms ??  88 ??s, 5.90x
    MVar:               OK (0.21s)
      12.7 ms ?? 1.0 ms, 54.99x
    TMVar:              OK (0.27s)
      1.77 ms ??  85 ??s, 7.69x
    TVar:               OK (0.15s)
      1.81 ms ?? 110 ??s, 7.87x
    Addr:               OK (0.09s)
      231  ??s ??  13 ??s, 1.00x
  Read/write contention with 10000 iterations and 4 threads
    Counter:            OK (0.04s)
      2.21 ms ?? 197 ??s
    IORef inconsistent: OK (0.08s)
      4.54 ms ?? 299 ??s, 2.05x
    IORef atomic:       OK (5.62s)
      15.0 ms ?? 1.2 ms, 6.80x
    MVar:               OK (1.92s)
      124  ms ?? 671 ??s, 56.22x
    TMVar:              OK (0.17s)
      19.9 ms ?? 1.8 ms, 8.97x
    TVar:               OK (0.34s)
      20.0 ms ?? 503 ??s, 9.04x
    Addr:               OK (0.08s)
      2.30 ms ??  92 ??s, 1.04x
  Read/write contention with 10 iterations and 6 threads
    Counter:            OK (0.21s)
      22.4 ??s ?? 1.9 ??s
    IORef inconsistent: OK (0.12s)
      24.8 ??s ?? 2.0 ??s, 1.11x
    IORef atomic:       OK (0.15s)
      35.9 ??s ?? 2.3 ??s, 1.60x
    MVar:               OK (0.15s)
      76.8 ??s ?? 5.9 ??s, 3.44x
    TMVar:              OK (0.60s)
      33.3 ??s ?? 3.2 ??s, 1.49x
    TVar:               OK (0.53s)
      35.0 ??s ?? 2.6 ??s, 1.57x
    Addr:               OK (0.12s)
      22.9 ??s ?? 2.3 ??s, 1.03x
  Read/write contention with 100 iterations and 6 threads
    Counter:            OK (0.08s)
      50.8 ??s ?? 5.0 ??s
    IORef inconsistent: OK (0.06s)
      99.4 ??s ?? 9.8 ??s, 1.96x
    IORef atomic:       OK (0.09s)
      450  ??s ??  40 ??s, 8.86x
    MVar:               OK (0.25s)
      1.85 ms ??  92 ??s, 36.35x
    TMVar:              OK (0.21s)
      412  ??s ??  30 ??s, 8.11x
    TVar:               OK (0.06s)
      587  ??s ??  49 ??s, 11.56x
    Addr:               OK (0.58s)
      46.8 ??s ?? 2.4 ??s, 0.92x
  Read/write contention with 1000 iterations and 6 threads
    Counter:            OK (0.13s)
      484  ??s ??  21 ??s
    IORef inconsistent: OK (3.26s)
      849  ??s ?? 282 ??s, 1.75x
    IORef atomic:       OK (0.53s)
      3.06 ms ?? 287 ??s, 6.32x
    MVar:               OK (0.30s)
      19.1 ms ?? 1.9 ms, 39.47x
    TMVar:              OK (4.08s)
      4.98 ms ?? 1.1 ms, 10.28x
    TVar:               OK (0.13s)
      4.85 ms ?? 293 ??s, 10.01x
    Addr:               OK (0.11s)
      510  ??s ??  17 ??s, 1.05x
  Read/write contention with 10000 iterations and 6 threads
    Counter:            OK (0.25s)
      4.44 ms ?? 128 ??s
    IORef inconsistent: OK (0.12s)
      9.94 ms ?? 442 ??s, 2.24x
    IORef atomic:       OK (6.67s)
      61.3 ms ??  13 ms, 13.82x
    MVar:               OK (2.85s)
      189  ms ?? 2.5 ms, 42.55x
    TMVar:              OK (0.16s)
      55.3 ms ?? 2.7 ms, 12.45x
    TVar:               OK (0.62s)
      49.7 ms ?? 3.6 ms, 11.18x
    Addr:               OK (0.07s)
      5.06 ms ?? 282 ??s, 1.14x
  Read/write contention with 10 iterations and 8 threads
    Counter:            OK (0.12s)
      25.8 ??s ?? 1.4 ??s
    IORef inconsistent: OK (0.14s)
      29.3 ??s ?? 2.6 ??s, 1.13x
    IORef atomic:       OK (0.21s)
      52.2 ??s ?? 2.2 ??s, 2.02x
    MVar:               OK (0.23s)
      117  ??s ?? 7.8 ??s, 4.54x
    TMVar:              OK (0.09s)
      55.7 ??s ?? 5.5 ??s, 2.16x
    TVar:               OK (1.32s)
      53.4 ??s ?? 561 ns, 2.07x
    Addr:               OK (0.12s)
      26.0 ??s ?? 2.4 ??s, 1.01x
  Read/write contention with 100 iterations and 8 threads
    Counter:            OK (0.10s)
      63.5 ??s ?? 3.5 ??s
    IORef inconsistent: OK (1.16s)
      123  ??s ?? 7.7 ??s, 1.94x
    IORef atomic:       OK (0.26s)
      632  ??s ??  43 ??s, 9.94x
    MVar:               OK (0.30s)
      2.27 ms ?? 222 ??s, 35.73x
    TMVar:              OK (1.15s)
      643  ??s ?? 9.9 ??s, 10.12x
    TVar:               OK (0.29s)
      642  ??s ??  52 ??s, 10.10x
    Addr:               OK (0.10s)
      61.4 ??s ?? 5.2 ??s, 0.97x
  Read/write contention with 1000 iterations and 8 threads
    Counter:            OK (0.15s)
      553  ??s ??  37 ??s
    IORef inconsistent: OK (0.04s)
      1.04 ms ??  85 ??s, 1.88x
    IORef atomic:       OK (5.60s)
      6.58 ms ?? 166 ??s, 11.89x
    MVar:               OK (0.36s)
      23.1 ms ?? 2.1 ms, 41.80x
    TMVar:              OK (0.17s)
      6.55 ms ?? 273 ??s, 11.83x
    TVar:               OK (2.59s)
      6.17 ms ?? 247 ??s, 11.14x
    Addr:               OK (0.08s)
      624  ??s ??  27 ??s, 1.13x
  Read/write contention with 10000 iterations and 8 threads
    Counter:            OK (0.08s)
      5.04 ms ?? 445 ??s
    IORef inconsistent: OK (0.08s)
      11.8 ms ?? 386 ??s, 2.35x
    IORef atomic:       OK (7.71s)
      141  ms ??  12 ms, 27.99x
    MVar:               OK (0.77s)
      255  ms ??  11 ms, 50.70x
    TMVar:              OK (0.80s)
      67.6 ms ?? 280 ??s, 13.42x
    TVar:               OK (0.10s)
      55.6 ms ?? 1.5 ms, 11.03x
    Addr:               OK (0.32s)
      6.25 ms ?? 470 ??s, 1.24x
  Read/write contention with 10 iterations and 12 threads
    Counter:            OK (0.13s)
      29.0 ??s ?? 1.9 ??s
    IORef inconsistent: OK (0.14s)
      34.2 ??s ?? 3.2 ??s, 1.18x
    IORef atomic:       OK (1.01s)
      73.3 ??s ?? 1.4 ??s, 2.53x
    MVar:               OK (0.23s)
      236  ??s ??  20 ??s, 8.15x
    TMVar:              OK (0.44s)
      83.2 ??s ?? 2.8 ??s, 2.87x
    TVar:               OK (0.11s)
      66.2 ??s ?? 3.2 ??s, 2.28x
    Addr:               OK (0.08s)
      29.3 ??s ?? 2.6 ??s, 1.01x
  Read/write contention with 100 iterations and 12 threads
    Counter:            OK (0.24s)
      98.7 ??s ?? 4.9 ??s
    IORef inconsistent: OK (0.21s)
      194  ??s ??  14 ??s, 1.97x
    IORef atomic:       OK (0.43s)
      1.01 ms ??  47 ??s, 10.28x
    MVar:               OK (0.43s)
      3.21 ms ??  92 ??s, 32.54x
    TMVar:              OK (0.93s)
      1.05 ms ??  13 ??s, 10.68x
    TVar:               OK (0.06s)
      1.06 ms ??  64 ??s, 10.71x
    Addr:               OK (0.06s)
      92.2 ??s ?? 8.7 ??s, 0.93x
  Read/write contention with 1000 iterations and 12 threads
    Counter:            OK (0.23s)
      1.00 ms ??  48 ??s
    IORef inconsistent: OK (0.10s)
      1.95 ms ??  45 ??s, 1.94x
    IORef atomic:       OK (0.90s)
      16.7 ms ?? 574 ??s, 16.58x
    MVar:               OK (0.26s)
      36.1 ms ?? 2.8 ms, 35.91x
    TMVar:              OK (0.53s)
      10.8 ms ?? 334 ??s, 10.72x
    TVar:               OK (0.25s)
      9.37 ms ?? 265 ??s, 9.33x
    Addr:               OK (0.10s)
      768  ??s ??  56 ??s, 0.76x
  Read/write contention with 10000 iterations and 12 threads
    Counter:            OK (0.12s)
      8.38 ms ?? 385 ??s
    IORef inconsistent: OK (0.03s)
      14.6 ms ?? 1.4 ms, 1.75x
    IORef atomic:       OK (9.26s)
      282  ms ?? 135 ms, 33.61x
    MVar:               OK (1.09s)
      338  ms ??  11 ms, 40.40x
    TMVar:              OK (0.63s)
      106  ms ?? 2.3 ms, 12.64x
    TVar:               OK (0.31s)
      100  ms ?? 6.2 ms, 11.90x
    Addr:               OK (0.06s)
      9.71 ms ?? 794 ??s, 1.16x
  Read/write contention with 10 iterations and 16 threads
    Counter:            OK (0.09s)
      40.5 ??s ?? 2.8 ??s
    IORef inconsistent: OK (0.09s)
      48.5 ??s ?? 4.6 ??s, 1.20x
    IORef atomic:       OK (0.13s)
      144  ??s ??  10 ??s, 3.56x
    MVar:               OK (0.20s)
      400  ??s ??  36 ??s, 9.86x
    TMVar:              OK (0.59s)
      123  ??s ??  11 ??s, 3.04x
    TVar:               OK (0.60s)
      130  ??s ??  12 ??s, 3.21x
    Addr:               OK (0.09s)
      40.5 ??s ?? 3.5 ??s, 1.00x
  Read/write contention with 100 iterations and 16 threads
    Counter:            OK (0.08s)
      133  ??s ??  11 ??s
    IORef inconsistent: OK (0.07s)
      278  ??s ??  23 ??s, 2.09x
    IORef atomic:       OK (1.94s)
      1.25 ms ??  76 ??s, 9.39x
    MVar:               OK (0.56s)
      4.21 ms ?? 380 ??s, 31.66x
    TMVar:              OK (0.15s)
      1.48 ms ??  43 ??s, 11.12x
    TVar:               OK (0.08s)
      1.40 ms ??  68 ??s, 10.55x
    Addr:               OK (0.16s)
      134  ??s ?? 4.8 ??s, 1.00x
  Read/write contention with 1000 iterations and 16 threads
    Counter:            OK (0.29s)
      1.36 ms ??  11 ??s
    IORef inconsistent: OK (0.14s)
      2.46 ms ??  69 ??s, 1.81x
    IORef atomic:       OK (6.70s)
      15.9 ms ?? 1.7 ms, 11.73x
    MVar:               OK (1.38s)
      44.5 ms ?? 3.2 ms, 32.79x
    TMVar:              OK (0.34s)
      14.2 ms ?? 551 ??s, 10.43x
    TVar:               OK (0.17s)
      14.0 ms ?? 314 ??s, 10.35x
    Addr:               OK (0.07s)
      1.36 ms ?? 101 ??s, 1.00x
  Read/write contention with 10000 iterations and 16 threads
    Counter:            OK (0.16s)
      10.6 ms ?? 651 ??s
    IORef inconsistent: OK (0.31s)
      24.4 ms ?? 1.1 ms, 2.30x
    IORef atomic:       OK (1.23s)
      755  ms ??  16 ms, 71.31x
    MVar:               OK (1.33s)
      446  ms ??  13 ms, 42.13x
    TMVar:              OK (0.08s)
      135  ms ??  12 ms, 12.80x
    TVar:               OK (1.76s)
      150  ms ?? 9.3 ms, 14.18x
    Addr:               OK (0.67s)
      13.3 ms ?? 344 ??s, 1.25x
  Read/write contention with 10 iterations and 20 threads
    Counter:            OK (0.10s)
      48.7 ??s ?? 2.7 ??s
    IORef inconsistent: OK (0.11s)
      57.7 ??s ?? 3.4 ??s, 1.18x
    IORef atomic:       OK (1.28s)
      185  ??s ?? 4.3 ??s, 3.80x
    MVar:               OK (0.14s)
      548  ??s ??  45 ??s, 11.25x
    TMVar:              OK (0.05s)
      160  ??s ??  11 ??s, 3.28x
    TVar:               OK (0.10s)
      168  ??s ?? 6.2 ??s, 3.44x
    Addr:               OK (0.10s)
      48.2 ??s ?? 3.7 ??s, 0.99x
  Read/write contention with 100 iterations and 20 threads
    Counter:            OK (0.10s)
      170  ??s ?? 8.4 ??s
    IORef inconsistent: OK (0.08s)
      329  ??s ??  17 ??s, 1.93x
    IORef atomic:       OK (0.34s)
      1.85 ms ?? 105 ??s, 10.83x
    MVar:               OK (0.36s)
      5.37 ms ?? 306 ??s, 31.52x
    TMVar:              OK (0.19s)
      1.88 ms ?? 120 ??s, 11.01x
    TVar:               OK (0.09s)
      1.82 ms ?? 129 ??s, 10.69x
    Addr:               OK (0.05s)
      165  ??s ??  15 ??s, 0.97x
  Read/write contention with 1000 iterations and 20 threads
    Counter:            OK (0.06s)
      1.28 ms ??  86 ??s
    IORef inconsistent: OK (0.32s)
      3.25 ms ?? 260 ??s, 2.53x
    IORef atomic:       OK (9.51s)
      21.7 ms ?? 2.8 ms, 16.88x
    MVar:               OK (0.88s)
      57.9 ms ?? 686 ??s, 45.07x
    TMVar:              OK (0.22s)
      18.7 ms ?? 1.2 ms, 14.56x
    TVar:               OK (0.22s)
      17.7 ms ?? 692 ??s, 13.78x
    Addr:               OK (0.05s)
      1.71 ms ?? 103 ??s, 1.33x
  Read/write contention with 10000 iterations and 20 threads
    Counter:            OK (0.10s)
      17.1 ms ?? 1.7 ms
    IORef inconsistent: OK (0.10s)
      31.9 ms ?? 1.2 ms, 1.86x
    IORef atomic:       OK (1.72s)
      546  ms ??  27 ms, 31.92x
    MVar:               OK (1.92s)
      626  ms ??  48 ms, 36.57x
    TMVar:              OK (0.56s)
      191  ms ?? 2.1 ms, 11.18x
    TVar:               OK (0.11s)
      181  ms ?? 7.1 ms, 10.55x
    Addr:               OK (0.10s)
      16.4 ms ?? 607 ??s, 0.96x
  Read/write contention with 10 iterations and 32 threads
    Counter:            OK (0.13s)
      69.9 ??s ?? 3.9 ??s
    IORef inconsistent: OK (0.15s)
      87.7 ??s ?? 4.1 ??s, 1.25x
    IORef atomic:       OK (0.50s)
      185  ??s ?? 2.4 ??s, 2.65x
    MVar:               OK (0.13s)
      923  ??s ??  88 ??s, 13.20x
    TMVar:              OK (0.13s)
      273  ??s ??  14 ??s, 3.90x
    TVar:               OK (0.58s)
      308  ??s ??  24 ??s, 4.41x
    Addr:               OK (0.06s)
      68.4 ??s ?? 5.9 ??s, 0.98x
  Read/write contention with 100 iterations and 32 threads
    Counter:            OK (0.08s)
      280  ??s ??  15 ??s
    IORef inconsistent: OK (0.12s)
      500  ??s ??  18 ??s, 1.79x
    IORef atomic:       OK (0.46s)
      1.69 ms ??  28 ??s, 6.05x
    MVar:               OK (0.58s)
      8.82 ms ?? 308 ??s, 31.51x
    TMVar:              OK (0.15s)
      3.01 ms ?? 214 ??s, 10.75x
    TVar:               OK (0.33s)
      3.09 ms ?? 111 ??s, 11.04x
    Addr:               OK (0.04s)
      258  ??s ??  23 ??s, 0.92x
  Read/write contention with 1000 iterations and 32 threads
    Counter:            OK (0.29s)
      2.82 ms ??  44 ??s
    IORef inconsistent: OK (0.14s)
      5.04 ms ?? 128 ??s, 1.79x
    IORef atomic:       OK (3.51s)
      31.6 ms ?? 2.7 ms, 11.22x
    MVar:               OK (1.41s)
      88.8 ms ?? 6.3 ms, 31.49x
    TMVar:              OK (0.08s)
      29.6 ms ?? 2.2 ms, 10.51x
    TVar:               OK (0.09s)
      32.4 ms ?? 3.0 ms, 11.49x
    Addr:               OK (0.08s)
      2.25 ms ?? 154 ??s, 0.80x
  Read/write contention with 10000 iterations and 32 threads
    Counter:            OK (0.16s)
      24.5 ms ?? 1.8 ms
    IORef inconsistent: OK (0.15s)
      51.2 ms ?? 3.3 ms, 2.09x
    IORef atomic:       OK (10.26s)
      1.292 s ?? 657 ms, 52.67x
    MVar:               OK (3.08s)
      976  ms ??  51 ms, 39.78x
    TMVar:              OK (0.41s)
      329  ms ?? 2.9 ms, 13.42x
    TVar:               OK (0.18s)
      311  ms ?? 3.1 ms, 12.69x
    Addr:               OK (0.17s)
      24.8 ms ?? 2.2 ms, 1.01x
  Read/write contention with 10 iterations and 64 threads
    Counter:            OK (0.10s)
      125  ??s ?? 7.6 ??s
    IORef inconsistent: OK (0.06s)
      158  ??s ??  13 ??s, 1.26x
    IORef atomic:       OK (0.10s)
      307  ??s ??  12 ??s, 2.46x
    MVar:               OK (0.12s)
      1.88 ms ?? 179 ??s, 15.03x
    TMVar:              OK (0.25s)
      555  ??s ??  13 ??s, 4.44x
    TVar:               OK (0.14s)
      538  ??s ??  12 ??s, 4.31x
    Addr:               OK (0.06s)
      122  ??s ??  12 ??s, 0.98x
  Read/write contention with 100 iterations and 64 threads
    Counter:            OK (0.07s)
      566  ??s ??  28 ??s
    IORef inconsistent: OK (0.07s)
      1.04 ms ??  44 ??s, 1.83x
    IORef atomic:       OK (0.58s)
      3.64 ms ?? 107 ??s, 6.44x
    MVar:               OK (0.12s)
      16.7 ms ?? 1.5 ms, 29.58x
    TMVar:              OK (0.15s)
      6.21 ms ?? 213 ??s, 10.97x
    TVar:               OK (0.15s)
      6.07 ms ?? 175 ??s, 10.74x
    Addr:               OK (0.07s)
      556  ??s ??  43 ??s, 0.98x
  Read/write contention with 1000 iterations and 64 threads
    Counter:            OK (0.15s)
      5.96 ms ?? 302 ??s
    IORef inconsistent: OK (0.13s)
      10.6 ms ?? 514 ??s, 1.77x
    IORef atomic:       OK (1.27s)
      100  ms ?? 2.7 ms, 16.80x
    MVar:               OK (1.33s)
      188  ms ?? 6.1 ms, 31.47x
    TMVar:              OK (0.18s)
      64.5 ms ?? 1.1 ms, 10.83x
    TVar:               OK (0.18s)
      63.8 ms ?? 4.2 ms, 10.70x
    Addr:               OK (0.07s)
      5.89 ms ?? 510 ??s, 0.99x
  Read/write contention with 10000 iterations and 64 threads
    Counter:            OK (0.17s)
      58.3 ms ?? 4.6 ms
    IORef inconsistent: OK (0.29s)
      105  ms ?? 6.9 ms, 1.79x
    IORef atomic:       OK (11.68s)
      3.061 s ?? 627 ms, 52.46x
    MVar:               OK (6.29s)
      2.002 s ?? 104 ms, 34.31x
    TMVar:              OK (1.73s)
      632  ms ?? 1.0 ms, 10.84x
    TVar:               OK (0.82s)
      654  ms ??  49 ms, 11.21x
    Addr:               OK (0.17s)
      58.7 ms ?? 4.6 ms, 1.01x
  Read/write contention with 10 iterations and 128 threads
    Counter:            OK (0.09s)
      228  ??s ??  22 ??s
    IORef inconsistent: OK (0.11s)
      310  ??s ??  22 ??s, 1.36x
    IORef atomic:       OK (0.09s)
      632  ??s ??  49 ??s, 2.78x
    MVar:               OK (0.12s)
      3.77 ms ?? 372 ??s, 16.54x
    TMVar:              OK (0.04s)
      1.07 ms ??  94 ??s, 4.70x
    TVar:               OK (0.51s)
      1.04 ms ??  20 ??s, 4.55x
    Addr:               OK (0.08s)
      220  ??s ??  14 ??s, 0.97x
  Read/write contention with 100 iterations and 128 threads
    Counter:            OK (0.26s)
      1.15 ms ??  27 ??s
    IORef inconsistent: OK (0.13s)
      2.05 ms ?? 117 ??s, 1.78x
    IORef atomic:       OK (0.35s)
      8.17 ms ?? 736 ??s, 7.09x
    MVar:               OK (1.12s)
      34.8 ms ?? 1.4 ms, 30.25x
    TMVar:              OK (0.15s)
      12.9 ms ?? 761 ??s, 11.16x
    TVar:               OK (0.15s)
      13.3 ms ?? 1.3 ms, 11.54x
    Addr:               OK (0.07s)
      1.12 ms ??  62 ??s, 0.97x
  Read/write contention with 1000 iterations and 128 threads
    Counter:            OK (0.57s)
      11.8 ms ?? 184 ??s
    IORef inconsistent: OK (0.07s)
      17.5 ms ?? 1.6 ms, 1.48x
    IORef atomic:       OK (8.50s)
      266  ms ?? 173 ms, 22.55x
    MVar:               OK (1.12s)
      367  ms ??  17 ms, 31.14x
    TMVar:              OK (0.36s)
      136  ms ??  12 ms, 11.54x
    TVar:               OK (0.72s)
      128  ms ??  12 ms, 10.87x
    Addr:               OK (2.23s)
      11.5 ms ?? 522 ??s, 0.98x
  Read/write contention with 10000 iterations and 128 threads
    Counter:            OK (0.68s)
      116  ms ?? 3.7 ms
    IORef inconsistent: OK (0.27s)
      211  ms ?? 1.8 ms, 1.82x
    IORef atomic:       OK (10.67s)
      6.495 s ?? 1.45 s, 56.11x
    MVar:               OK (12.25s)
      3.909 s ?? 296 ms, 33.77x
    TMVar:              OK (1.63s)
      1.280 s ?? 127 ms, 11.06x
    TVar:               OK (0.72s)
      1.323 s ??  46 ms, 11.43x
    Addr:               OK (0.16s)
      122  ms ?? 7.8 ms, 1.05x

All 309 tests passed (267.00s)

```

# Memory overhead

Each unlifted value of type `Counter` is a singleton mutable array
from GHC primitives under the hood. Thus it occupies at least
`platform integer size` + `array size` + `header` bytes which should
typically be at least 3 machine words. Lifted values may occupy more
depending on optimizations.
