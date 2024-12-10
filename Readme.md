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

Still even is single-threaded scenario the `Counter` can serve as an
efficient mutable integer cell that does not box the integer.

# Benchmark

### Summary

Depending on number of threads `Counter` from this package can be
up to 10 times faster than next best `TVar` from the `stm` package.

### Details

The benchmark is to spawn N threads each of which will increment the
same counter by 1 for a number of iterations.

Test setup: Intel i5-9600K CPU @ 3.70GHz, 6 cores, no hyperthreading, GHC 9.6.1.

NB `IORef inconsistent` is the benchmark that just reads and writes
`IORef` with the fastest functions the `IORef` supports. It gives
wrong results when multiple threads access `IORef` this way since
these reads and writes are not synchronized. It’s included only for
speed comparison purposes. All others do proper synchronization of
increments between the threads. Current package is shown as `Counter`.

```
$ cabal run bench -- -j1 --timeout 300 --stdev 2
All
  Correctness:          OK (13.97s)
    +++ OK, passed 10000 tests.
  Read/write contention with 10 iterations and 1 threads
    Counter:            OK (14.23s)
      839  ns ±  33 ns
    IORef inconsistent: OK (1.08s)
      1.03 μs ±  36 ns, 1.23x
    IORef atomic:       OK (1.30s)
      1.25 μs ±  40 ns, 1.48x
    MVar:               OK (10.53s)
      1.24 μs ±  13 ns, 1.48x
    TMVar:              OK (0.65s)
      1.18 μs ±  30 ns, 1.41x
    TVar:               OK (0.33s)
      1.13 μs ±  45 ns, 1.35x
    Addr:               OK (3.37s)
      818  ns ±  28 ns, 0.97x
  Read/write contention with 100 iterations and 1 threads
    Counter:            OK (1.47s)
      1.35 μs ±  24 ns
    IORef inconsistent: OK (17.27s)
      2.09 μs ±  47 ns, 1.55x
    IORef atomic:       OK (0.45s)
      3.22 μs ± 122 ns, 2.38x
    MVar:               OK (3.15s)
      2.97 μs ±  30 ns, 2.20x
    TMVar:              OK (1.24s)
      4.65 μs ±  64 ns, 3.44x
    TVar:               OK (2.43s)
      4.61 μs ± 158 ns, 3.41x
    Addr:               OK (1.16s)
      1.09 μs ±  22 ns, 0.80x
  Read/write contention with 1000 iterations and 1 threads
    Counter:            OK (0.47s)
      6.48 μs ± 235 ns
    IORef inconsistent: OK (0.78s)
      2.76 μs ±  88 ns, 0.43x
    IORef atomic:       OK (0.60s)
      17.5 μs ± 399 ns, 2.69x
    MVar:               OK (2.09s)
      15.8 μs ± 208 ns, 2.43x
    TMVar:              OK (0.40s)
      48.3 μs ± 1.9 μs, 7.46x
    TVar:               OK (0.55s)
      32.4 μs ± 713 ns, 5.00x
    Addr:               OK (0.34s)
      4.73 μs ± 189 ns, 0.73x
  Read/write contention with 10000 iterations and 1 threads
    Counter:            OK (0.50s)
      58.4 μs ± 2.1 μs
    IORef inconsistent: OK (0.42s)
      23.8 μs ± 829 ns, 0.41x
    IORef atomic:       OK (0.73s)
      168  μs ± 3.8 μs, 2.87x
    MVar:               OK (0.66s)
      157  μs ± 4.6 μs, 2.69x
    TMVar:              OK (2.09s)
      512  μs ± 7.2 μs, 8.78x
    TVar:               OK (1.39s)
      321  μs ± 6.7 μs, 5.50x
    Addr:               OK (47.12s)
      43.8 μs ± 562 ns, 0.75x
  Read/write contention with 10 iterations and 2 threads
    Counter:            OK (1.10s)
      7.85 μs ± 144 ns
    IORef inconsistent: OK (1.93s)
      7.65 μs ± 201 ns, 0.97x
    IORef atomic:       OK (0.54s)
      8.16 μs ± 235 ns, 1.04x
    MVar:               OK (7.81s)
      7.81 μs ±  72 ns, 0.99x
    TMVar:              OK (0.49s)
      7.98 μs ± 292 ns, 1.02x
    TVar:               OK (1.91s)
      7.98 μs ± 181 ns, 1.02x
    Addr:               OK (2.01s)
      7.64 μs ± 213 ns, 0.97x
  Read/write contention with 100 iterations and 2 threads
    Counter:            OK (1.00s)
      8.50 μs ± 290 ns
    IORef inconsistent: OK (8.00s)
      8.13 μs ± 133 ns, 0.96x
    IORef atomic:       OK (2.80s)
      13.3 μs ± 358 ns, 1.56x
    MVar:               OK (4.64s)
      19.2 μs ± 473 ns, 2.25x
    TMVar:              OK (5.52s)
      32.3 μs ± 1.1 μs, 3.80x
    TVar:               OK (83.27s)
      30.5 μs ± 279 ns, 3.59x
    Addr:               OK (4.38s)
      8.71 μs ± 333 ns, 1.03x
  Read/write contention with 1000 iterations and 2 threads
    Counter:            OK (28.57s)
      43.7 μs ± 106 ns
    IORef inconsistent: OK (0.91s)
      17.2 μs ± 669 ns, 0.39x
    IORef atomic:       OK (130.48s)
      223  μs ±  10 μs, 5.09x
    MVar:               OK (1.19s)
      5.11 ms ± 173 μs, 116.88x
    TMVar:              OK (0.33s)
      295  μs ± 7.7 μs, 6.75x
    TVar:               OK (1.44s)
      337  μs ±  10 μs, 7.71x
    Addr:               OK (0.47s)
      49.3 μs ± 915 ns, 1.13x
  Read/write contention with 10000 iterations and 2 threads
    Counter:            OK (0.51s)
      466  μs ±  17 μs
    IORef inconsistent: OK (13.49s)
      190  μs ± 1.2 μs, 0.41x
    IORef atomic:       OK (71.77s)
      1.80 ms ±  33 μs, 3.86x
    MVar:               OK (7.23s)
      60.1 ms ± 505 μs, 128.83x
    TMVar:              OK (1.50s)
      2.39 ms ±  39 μs, 5.13x
    TVar:               OK (108.24s)
      3.13 ms ± 405 μs, 6.70x
    Addr:               OK (0.29s)
      531  μs ±  20 μs, 1.14x
  Read/write contention with 10 iterations and 4 threads
    Counter:            OK (2.33s)
      15.5 μs ± 384 ns
    IORef inconsistent: OK (2.41s)
      15.6 μs ±  52 ns, 1.01x
    IORef atomic:       OK (1.56s)
      19.5 μs ± 487 ns, 1.26x
    MVar:               OK (1.51s)
      28.9 μs ± 647 ns, 1.87x
    TMVar:              OK (2.88s)
      19.9 μs ± 439 ns, 1.29x
    TVar:               OK (0.38s)
      18.2 μs ± 332 ns, 1.18x
    Addr:               OK (0.59s)
      15.3 μs ± 477 ns, 0.99x
  Read/write contention with 100 iterations and 4 threads
    Counter:            OK (1.63s)
      25.3 μs ± 723 ns
    IORef inconsistent: OK (0.35s)
      17.6 μs ± 451 ns, 0.69x
    IORef atomic:       OK (0.46s)
      154  μs ± 2.1 μs, 6.08x
    MVar:               OK (0.62s)
      1.20 ms ±  38 μs, 47.27x
    TMVar:              OK (0.12s)
      191  μs ± 7.4 μs, 7.54x
    TVar:               OK (0.14s)
      221  μs ± 6.6 μs, 8.71x
    Addr:               OK (0.75s)
      21.0 μs ± 228 ns, 0.83x
  Read/write contention with 1000 iterations and 4 threads
    Counter:            OK (0.17s)
      242  μs ± 6.7 μs
    IORef inconsistent: OK (0.15s)
      103  μs ± 2.7 μs, 0.43x
    IORef atomic:       OK (2.28s)
      1.71 ms ±  36 μs, 7.07x
    MVar:               OK (0.41s)
      12.9 ms ± 338 μs, 53.16x
    TMVar:              OK (0.29s)
      2.08 ms ±  65 μs, 8.61x
    TVar:               OK (0.17s)
      2.23 ms ±  73 μs, 9.21x
    Addr:               OK (0.58s)
      234  μs ± 1.9 μs, 0.97x
  Read/write contention with 10000 iterations and 4 threads
    Counter:            OK (0.17s)
      2.24 ms ±  60 μs
    IORef inconsistent: OK (9.34s)
      1.00 ms ±  31 μs, 0.45x
    IORef atomic:       OK (70.87s)
      22.0 ms ± 820 μs, 9.80x
    MVar:               OK (0.74s)
      126  ms ± 1.9 ms, 56.07x
    TMVar:              OK (0.33s)
      19.0 ms ± 670 μs, 8.48x
    TVar:               OK (0.09s)
      20.8 ms ± 732 μs, 9.27x
    Addr:               OK (0.18s)
      2.45 ms ±  97 μs, 1.09x
  Read/write contention with 10 iterations and 6 threads
    Counter:            OK (6.77s)
      23.9 μs ± 813 ns
    IORef inconsistent: OK (0.42s)
      23.4 μs ± 712 ns, 0.98x
    IORef atomic:       OK (0.70s)
      39.4 μs ± 747 ns, 1.65x
    MVar:               OK (1.32s)
      50.0 μs ± 1.0 μs, 2.09x
    TMVar:              OK (0.52s)
      35.3 μs ± 449 ns, 1.47x
    TVar:               OK (3.93s)
      32.6 μs ±  93 ns, 1.37x
    Addr:               OK (0.42s)
      23.4 μs ± 381 ns, 0.98x
  Read/write contention with 100 iterations and 6 threads
    Counter:            OK (1.20s)
      47.8 μs ± 866 ns
    IORef inconsistent: OK (0.24s)
      26.9 μs ± 983 ns, 0.56x
    IORef atomic:       OK (1.46s)
      483  μs ±  17 μs, 10.10x
    MVar:               OK (2.01s)
      1.86 ms ±  13 μs, 38.97x
    TMVar:              OK (0.49s)
      519  μs ± 4.5 μs, 10.86x
    TVar:               OK (14.78s)
      521  μs ±  14 μs, 10.91x
    Addr:               OK (0.56s)
      43.1 μs ± 1.6 μs, 0.90x
  Read/write contention with 1000 iterations and 6 threads
    Counter:            OK (13.77s)
      459  μs ± 8.8 μs
    IORef inconsistent: OK (0.23s)
      207  μs ± 4.1 μs, 0.45x
    IORef atomic:       OK (2.10s)
      3.48 ms ±  70 μs, 7.58x
    MVar:               OK (0.64s)
      19.3 ms ± 555 μs, 42.03x
    TMVar:              OK (1.07s)
      5.55 ms ± 161 μs, 12.08x
    TVar:               OK (0.27s)
      4.85 ms ± 189 μs, 10.56x
    Addr:               OK (27.53s)
      461  μs ± 4.8 μs, 1.00x
  Read/write contention with 10000 iterations and 6 threads
    Counter:            OK (0.52s)
      4.88 ms ± 142 μs
    IORef inconsistent: OK (0.25s)
      2.25 ms ±  24 μs, 0.46x
    IORef atomic:       OK (66.59s)
      75.5 ms ±  11 ms, 15.46x
    MVar:               OK (2.85s)
      193  ms ± 4.2 ms, 39.54x
    TMVar:              OK (0.16s)
      54.9 ms ± 1.6 ms, 11.25x
    TVar:               OK (10.04s)
      47.1 ms ± 339 μs, 9.64x
    Addr:               OK (0.52s)
      4.94 ms ±  89 μs, 1.01x
  Read/write contention with 10 iterations and 8 threads
    Counter:            OK (1.84s)
      27.3 μs ± 1.0 μs
    IORef inconsistent: OK (1.98s)
      27.5 μs ± 1.0 μs, 1.01x
    IORef atomic:       OK (0.91s)
      56.1 μs ± 787 ns, 2.06x
    MVar:               OK (0.69s)
      94.9 μs ± 3.2 μs, 3.48x
    TMVar:              OK (1.35s)
      56.1 μs ± 1.5 μs, 2.06x
    TVar:               OK (0.36s)
      51.7 μs ± 1.5 μs, 1.89x
    Addr:               OK (0.24s)
      26.3 μs ± 672 ns, 0.96x
  Read/write contention with 100 iterations and 8 threads
    Counter:            OK (1.41s)
      63.1 μs ± 1.2 μs
    IORef inconsistent: OK (8.38s)
      33.3 μs ± 503 ns, 0.53x
    IORef atomic:       OK (0.59s)
      709  μs ±  21 μs, 11.25x
    MVar:               OK (1.21s)
      2.31 ms ±  49 μs, 36.65x
    TMVar:              OK (4.67s)
      667  μs ±  18 μs, 10.58x
    TVar:               OK (4.81s)
      690  μs ± 6.5 μs, 10.94x
    Addr:               OK (0.69s)
      59.6 μs ± 1.7 μs, 0.94x
  Read/write contention with 1000 iterations and 8 threads
    Counter:            OK (0.16s)
      583  μs ±  13 μs
    IORef inconsistent: OK (1.13s)
      304  μs ± 3.5 μs, 0.52x
    IORef atomic:       OK (93.87s)
      7.12 ms ± 158 μs, 12.22x
    MVar:               OK (1.54s)
      24.4 ms ± 541 μs, 41.83x
    TMVar:              OK (0.09s)
      6.32 ms ± 241 μs, 10.84x
    TVar:               OK (0.67s)
      6.36 ms ± 221 μs, 10.92x
    Addr:               OK (0.56s)
      540  μs ±  20 μs, 0.93x
  Read/write contention with 10000 iterations and 8 threads
    Counter:            OK (5.13s)
      5.67 ms ±  68 μs
    IORef inconsistent: OK (0.16s)
      3.14 ms ±  69 μs, 0.55x
    IORef atomic:       OK (35.04s)
      148  ms ± 319 μs, 26.14x
    MVar:               OK (1.58s)
      230  ms ± 8.7 ms, 40.61x
    TMVar:              OK (26.99s)
      65.9 ms ± 4.1 ms, 11.62x
    TVar:               OK (3.41s)
      66.6 ms ± 779 μs, 11.75x
    Addr:               OK (0.16s)
      5.84 ms ± 164 μs, 1.03x
  Read/write contention with 10 iterations and 12 threads
    Counter:            OK (0.54s)
      30.5 μs ± 452 ns
    IORef inconsistent: OK (0.29s)
      32.4 μs ± 820 ns, 1.06x
    IORef atomic:       OK (7.93s)
      70.3 μs ± 436 ns, 2.30x
    MVar:               OK (0.44s)
      232  μs ± 8.7 μs, 7.59x
    TMVar:              OK (13.74s)
      80.4 μs ± 2.3 μs, 2.64x
    TVar:               OK (1.73s)
      79.2 μs ± 2.7 μs, 2.60x
    Addr:               OK (0.26s)
      29.6 μs ± 1.0 μs, 0.97x
  Read/write contention with 100 iterations and 12 threads
    Counter:            OK (0.49s)
      98.0 μs ± 1.1 μs
    IORef inconsistent: OK (0.66s)
      48.2 μs ± 561 ns, 0.49x
    IORef atomic:       OK (0.88s)
      1.07 ms ±  39 μs, 10.89x
    MVar:               OK (0.86s)
      3.18 ms ±  61 μs, 32.48x
    TMVar:              OK (0.21s)
      1.00 ms ±  21 μs, 10.19x
    TVar:               OK (0.12s)
      1.06 ms ±  29 μs, 10.77x
    Addr:               OK (0.13s)
      94.0 μs ± 2.7 μs, 0.96x
  Read/write contention with 1000 iterations and 12 threads
    Counter:            OK (6.78s)
      955  μs ± 5.9 μs
    IORef inconsistent: OK (0.22s)
      466  μs ±  16 μs, 0.49x
    IORef atomic:       OK (2.46s)
      11.1 ms ± 109 μs, 11.62x
    MVar:               OK (1.14s)
      34.3 ms ± 923 μs, 35.98x
    TMVar:              OK (8.60s)
      10.7 ms ± 226 μs, 11.22x
    TVar:               OK (4.18s)
      9.82 ms ±  42 μs, 10.29x
    Addr:               OK (0.42s)
      1.00 ms ±  40 μs, 1.05x
  Read/write contention with 10000 iterations and 12 threads
    Counter:            OK (1.99s)
      9.45 ms ± 116 μs
    IORef inconsistent: OK (7.78s)
      4.74 ms ± 7.4 μs, 0.50x
    IORef atomic:       OK (82.99s)
      304  ms ±  66 ms, 32.15x
    MVar:               OK (11.29s)
      343  ms ± 8.3 ms, 36.34x
    TMVar:              OK (0.33s)
      116  ms ± 1.9 ms, 12.23x
    TVar:               OK (10.63s)
      105  ms ± 1.5 ms, 11.15x
    Addr:               OK (3.97s)
      9.47 ms ± 361 μs, 1.00x
  Read/write contention with 10 iterations and 16 threads
    Counter:            OK (0.20s)
      42.1 μs ± 1.3 μs
    IORef inconsistent: OK (0.18s)
      40.5 μs ± 1.4 μs, 0.96x
    IORef atomic:       OK (1.08s)
      153  μs ± 662 ns, 3.63x
    MVar:               OK (1.56s)
      408  μs ± 9.1 μs, 9.69x
    TMVar:              OK (2.43s)
      128  μs ± 1.9 μs, 3.04x
    TVar:               OK (0.16s)
      135  μs ± 5.0 μs, 3.19x
    Addr:               OK (0.32s)
      40.6 μs ± 940 ns, 0.96x
  Read/write contention with 100 iterations and 16 threads
    Counter:            OK (0.32s)
      131  μs ± 2.1 μs
    IORef inconsistent: OK (1.68s)
      75.4 μs ± 1.6 μs, 0.58x
    IORef atomic:       OK (4.05s)
      1.27 ms ±  45 μs, 9.70x
    MVar:               OK (4.53s)
      4.39 ms ±  85 μs, 33.50x
    TMVar:              OK (0.16s)
      1.48 ms ±  40 μs, 11.30x
    TVar:               OK (1.20s)
      1.45 ms ± 5.5 μs, 11.06x
    Addr:               OK (4.85s)
      131  μs ± 1.3 μs, 1.00x
  Read/write contention with 1000 iterations and 16 threads
    Counter:            OK (0.15s)
      1.05 ms ±  32 μs
    IORef inconsistent: OK (0.28s)
      632  μs ±  12 μs, 0.60x
    IORef atomic:       OK (53.85s)
      16.3 ms ± 1.5 ms, 15.58x
    MVar:               OK (2.98s)
      46.6 ms ± 672 μs, 44.57x
    TMVar:              OK (0.09s)
      15.2 ms ± 472 μs, 14.54x
    TVar:               OK (0.36s)
      14.6 ms ± 354 μs, 13.96x
    Addr:               OK (0.15s)
      1.38 ms ±  47 μs, 1.32x
  Read/write contention with 10000 iterations and 16 threads
    Counter:            OK (0.17s)
      13.3 ms ± 301 μs
    IORef inconsistent: OK (0.16s)
      5.73 ms ± 119 μs, 0.43x
    IORef atomic:       OK (117.97s)
      464  ms ±  38 ms, 34.81x
    MVar:               OK (14.95s)
      481  ms ± 4.5 ms, 36.11x
    TMVar:              OK (1.80s)
      155  ms ± 2.7 ms, 11.65x
    TVar:               OK (7.20s)
      146  ms ± 663 μs, 10.93x
    Addr:               OK (5.34s)
      12.8 ms ± 187 μs, 0.96x
  Read/write contention with 10 iterations and 20 threads
    Counter:            OK (0.19s)
      49.3 μs ± 1.9 μs
    IORef inconsistent: OK (0.20s)
      48.2 μs ± 1.7 μs, 0.98x
    IORef atomic:       OK (5.11s)
      188  μs ± 5.2 μs, 3.82x
    MVar:               OK (4.08s)
      536  μs ± 5.4 μs, 10.86x
    TMVar:              OK (1.44s)
      159  μs ± 531 ns, 3.23x
    TVar:               OK (0.37s)
      176  μs ± 3.0 μs, 3.57x
    Addr:               OK (0.82s)
      50.6 μs ± 1.0 μs, 1.03x
  Read/write contention with 100 iterations and 20 threads
    Counter:            OK (0.10s)
      171  μs ± 5.7 μs
    IORef inconsistent: OK (0.25s)
      90.0 μs ± 2.1 μs, 0.52x
    IORef atomic:       OK (9.07s)
      1.52 ms ±  59 μs, 8.87x
    MVar:               OK (2.90s)
      5.61 ms ± 159 μs, 32.73x
    TMVar:              OK (1.57s)
      1.85 ms ±  54 μs, 10.77x
    TVar:               OK (0.19s)
      1.88 ms ±  28 μs, 10.98x
    Addr:               OK (0.18s)
      169  μs ± 3.8 μs, 0.98x
  Read/write contention with 1000 iterations and 20 threads
    Counter:            OK (0.74s)
      1.68 ms ±  65 μs
    IORef inconsistent: OK (0.18s)
      881  μs ±  24 μs, 0.52x
    IORef atomic:       OK (73.70s)
      21.3 ms ± 600 μs, 12.66x
    MVar:               OK (0.94s)
      61.0 ms ± 1.4 ms, 36.31x
    TMVar:              OK (7.19s)
      18.1 ms ± 472 μs, 10.75x
    TVar:               OK (7.26s)
      18.1 ms ± 494 μs, 10.79x
    Addr:               OK (0.09s)
      1.74 ms ±  43 μs, 1.04x
  Read/write contention with 10000 iterations and 20 threads
    Counter:            OK (6.72s)
      16.2 ms ± 174 μs
    IORef inconsistent: OK (6.36s)
      7.73 ms ± 247 μs, 0.48x
    IORef atomic:       OK (21.41s)
      641  ms ± 264 μs, 39.55x
    MVar:               OK (38.82s)
      609  ms ±  21 ms, 37.53x
    TMVar:              OK (4.56s)
      192  ms ± 648 μs, 11.82x
    TVar:               OK (1.13s)
      193  ms ± 7.5 ms, 11.91x
    Addr:               OK (0.43s)
      16.8 ms ± 581 μs, 1.03x
  Read/write contention with 10 iterations and 32 threads
    Counter:            OK (1.01s)
      73.7 μs ± 1.3 μs
    IORef inconsistent: OK (0.51s)
      72.0 μs ± 777 ns, 0.98x
    IORef atomic:       OK (0.24s)
      178  μs ± 3.8 μs, 2.41x
    MVar:               OK (0.91s)
      919  μs ±  17 μs, 12.47x
    TMVar:              OK (2.27s)
      277  μs ± 4.4 μs, 3.76x
    TVar:               OK (0.15s)
      299  μs ±  10 μs, 4.06x
    Addr:               OK (0.50s)
      71.8 μs ± 2.6 μs, 0.97x
  Read/write contention with 100 iterations and 32 threads
    Counter:            OK (0.29s)
      280  μs ± 6.4 μs
    IORef inconsistent: OK (1.44s)
      150  μs ± 3.2 μs, 0.53x
    IORef atomic:       OK (0.25s)
      1.74 ms ±  56 μs, 6.22x
    MVar:               OK (1.19s)
      9.06 ms ± 269 μs, 32.31x
    TMVar:              OK (4.80s)
      2.98 ms ±  36 μs, 10.64x
    TVar:               OK (19.94s)
      3.10 ms ±  27 μs, 11.04x
    Addr:               OK (0.55s)
      272  μs ± 9.4 μs, 0.97x
  Read/write contention with 1000 iterations and 32 threads
    Counter:            OK (2.27s)
      2.69 ms ±  48 μs
    IORef inconsistent: OK (2.18s)
      1.27 ms ±  23 μs, 0.47x
    IORef atomic:       OK (62.21s)
      35.2 ms ± 4.3 ms, 13.08x
    MVar:               OK (5.90s)
      89.9 ms ± 233 μs, 33.44x
    TMVar:              OK (2.96s)
      30.7 ms ± 470 μs, 11.40x
    TVar:               OK (5.98s)
      30.4 ms ± 124 μs, 11.30x
    Addr:               OK (0.29s)
      2.75 ms ±  93 μs, 1.02x
  Read/write contention with 10000 iterations and 32 threads
    Counter:            OK (0.08s)
      28.8 ms ± 1.0 ms
    IORef inconsistent: OK (0.32s)
      12.9 ms ± 351 μs, 0.45x
    IORef atomic:       OK (82.13s)
      1.180 s ±  65 ms, 41.02x
    MVar:               OK (128.00s)
      982  ms ±  14 ms, 34.15x
    TMVar:              OK (0.17s)
      299  ms ± 7.6 ms, 10.41x
    TVar:               OK (7.51s)
      316  ms ±  12 ms, 11.00x
    Addr:               OK (1.36s)
      27.0 ms ± 871 μs, 0.94x
  Read/write contention with 10 iterations and 64 threads
    Counter:            OK (0.84s)
      132  μs ± 3.3 μs
    IORef inconsistent: OK (0.44s)
      132  μs ± 2.4 μs, 1.00x
    IORef atomic:       OK (0.18s)
      323  μs ± 6.5 μs, 2.45x
    MVar:               OK (0.47s)
      1.89 ms ±  74 μs, 14.33x
    TMVar:              OK (4.18s)
      557  μs ± 1.7 μs, 4.22x
    TVar:               OK (2.19s)
      575  μs ± 1.6 μs, 4.36x
    Addr:               OK (0.19s)
      125  μs ± 2.8 μs, 0.95x
  Read/write contention with 100 iterations and 64 threads
    Counter:            OK (0.28s)
      568  μs ±  13 μs
    IORef inconsistent: OK (0.32s)
      284  μs ± 2.9 μs, 0.50x
    IORef atomic:       OK (0.21s)
      2.62 ms ±  95 μs, 4.62x
    MVar:               OK (2.37s)
      18.1 ms ± 688 μs, 31.81x
    TMVar:              OK (0.15s)
      6.24 ms ± 173 μs, 10.98x
    TVar:               OK (5.00s)
      6.30 ms ± 162 μs, 11.08x
    Addr:               OK (2.06s)
      555  μs ± 7.4 μs, 0.98x
  Read/write contention with 1000 iterations and 64 threads
    Counter:            OK (0.59s)
      5.80 ms ± 111 μs
    IORef inconsistent: OK (2.10s)
      2.49 ms ±  96 μs, 0.43x
    IORef atomic:       OK (2.36s)
      75.8 ms ± 396 μs, 13.06x
    MVar:               OK (2.88s)
      188  ms ± 4.8 ms, 32.41x
    TMVar:              OK (5.95s)
      61.7 ms ± 176 μs, 10.63x
    TVar:               OK (1.52s)
      63.8 ms ± 2.4 ms, 11.00x
    Addr:               OK (0.28s)
      5.52 ms ± 167 μs, 0.95x
  Read/write contention with 10000 iterations and 64 threads
    Counter:            OK (2.74s)
      56.6 ms ± 2.2 ms
    IORef inconsistent: OK (10.26s)
      26.0 ms ± 746 μs, 0.46x
    IORef atomic:       OK (100.70s)
      3.018 s ± 328 ms, 53.35x
    MVar:               OK (30.56s)
      1.994 s ±  16 ms, 35.25x
    TMVar:              OK (0.36s)
      665  ms ± 3.4 ms, 11.76x
    TVar:               OK (1.79s)
      653  ms ± 2.4 ms, 11.55x
    Addr:               OK (11.06s)
      57.0 ms ± 546 μs, 1.01x
  Read/write contention with 10 iterations and 128 threads
    Counter:            OK (0.67s)
      240  μs ± 5.9 μs
    IORef inconsistent: OK (1.43s)
      247  μs ± 8.5 μs, 1.03x
    IORef atomic:       OK (1.38s)
      642  μs ± 6.2 μs, 2.68x
    MVar:               OK (1.92s)
      3.83 ms ±  34 μs, 15.94x
    TMVar:              OK (16.12s)
      1.11 ms ±  23 μs, 4.63x
    TVar:               OK (4.27s)
      1.15 ms ±  36 μs, 4.78x
    Addr:               OK (0.16s)
      230  μs ± 6.9 μs, 0.96x
  Read/write contention with 100 iterations and 128 threads
    Counter:            OK (0.26s)
      1.15 ms ±  42 μs
    IORef inconsistent: OK (0.31s)
      560  μs ±  18 μs, 0.49x
    IORef atomic:       OK (0.31s)
      7.80 ms ± 274 μs, 6.81x
    MVar:               OK (0.55s)
      36.2 ms ± 1.2 ms, 31.54x
    TMVar:              OK (19.40s)
      12.4 ms ±  20 μs, 10.78x
    TVar:               OK (0.62s)
      13.1 ms ± 411 μs, 11.39x
    Addr:               OK (0.13s)
      1.12 ms ±  24 μs, 0.98x
  Read/write contention with 1000 iterations and 128 threads
    Counter:            OK (0.15s)
      8.93 ms ± 264 μs
    IORef inconsistent: OK (0.52s)
      5.28 ms ± 104 μs, 0.59x
    IORef atomic:       OK (80.71s)
      310  ms ±  57 ms, 34.68x
    MVar:               OK (5.86s)
      378  ms ± 3.1 ms, 42.38x
    TMVar:              OK (1.51s)
      131  ms ± 3.5 ms, 14.62x
    TVar:               OK (3.07s)
      129  ms ± 981 μs, 14.44x
    Addr:               OK (4.47s)
      11.3 ms ± 139 μs, 1.27x
  Read/write contention with 10000 iterations and 128 threads
    Counter:            OK (1.37s)
      112  ms ± 3.3 ms
    IORef inconsistent: OK (5.13s)
      53.3 ms ± 571 μs, 0.47x
    IORef atomic:       OK (11.14s)
      6.952 s ±  54 ms, 61.84x
    MVar:               OK (12.70s)
      4.075 s ± 154 ms, 36.25x
    TMVar:              OK (3.51s)
      1.268 s ± 5.7 ms, 11.27x
    TVar:               OK (0.71s)
      1.306 s ±  50 ms, 11.62x
    Addr:               OK (11.18s)
      117  ms ± 1.7 ms, 1.04x

All 309 tests passed (2397.33s)
```

# Memory overhead

In pure Haskell (i.e. with `no-cmm` flag enabled) each unlifted value of
type `Counter` is a singleton mutable array from GHC primitives under
the hood. Thus it occupies at least `platform integer size` + `array
size` + `header` bytes which should typically be at least 3 machine
words. Lifted values may occupy more depending on optimizations.

By default CMM will be enabled which should save one word of overhead
because there would be no array any more hence no need to store
trivial size.
