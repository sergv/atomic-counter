[![build](https://github.com/sergv/atomic-counter/actions/workflows/haskell-ci.yaml/badge.svg)](https://github.com/sergv/atomic-counter/actions/workflows/haskell-ci.yaml)

# Synopsis

Mutable cells that hold an integer value and can be safely modified from
multiple threads. Support only few operations: read, write, +, -, and
bitwise and, or, xor and nand.

Good use case is a shared counter that multiple threads increment.

Operations translate to atomic CPU instructions which make these cells
way faster that even vanilla `IORef`. Value inside the cell is unboxed
which contributes to outperforming all other shared vars because they
box the value they store. For integers boxing is especially bad.

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
these reads and writes are not synchronized. It’s included only for
speed comparison purposes. All others do proper synchronization of
increments between the threads. Current package is shown as `Counter`.

```
$ cabal run bench -- -j1 --timeout 30
All
  Correctness:          OK (15.95s)
    +++ OK, passed 10000 tests.
  Read/write contention with 10 iterations and 1 threads
    Counter:            OK (0.45s)
      814  ns ±  52 ns
    IORef inconsistent: OK (0.61s)
      1.10 μs ±  70 ns, 1.35x
    IORef atomic:       OK (1.26s)
      1.19 μs ±  25 ns, 1.46x
    MVar:               OK (0.64s)
      1.18 μs ±  39 ns, 1.46x
    TMVar:              OK (0.52s)
      914  ns ±  49 ns, 1.12x
    TVar:               OK (0.63s)
      1.14 μs ±  73 ns, 1.40x
    Addr:               OK (1.63s)
      734  ns ±  56 ns, 0.90x
  Read/write contention with 100 iterations and 1 threads
    Counter:            OK (1.36s)
      1.21 μs ±  26 ns
    IORef inconsistent: OK (0.27s)
      3.82 μs ± 341 ns, 3.15x
    IORef atomic:       OK (0.44s)
      2.96 μs ± 261 ns, 2.44x
    MVar:               OK (0.43s)
      3.11 μs ± 172 ns, 2.56x
    TMVar:              OK (0.16s)
      4.56 μs ± 412 ns, 3.76x
    TVar:               OK (0.64s)
      4.43 μs ± 261 ns, 3.65x
    Addr:               OK (0.60s)
      1.09 μs ±  78 ns, 0.90x
  Read/write contention with 1000 iterations and 1 threads
    Counter:            OK (0.35s)
      4.84 μs ± 377 ns
    IORef inconsistent: OK (0.32s)
      18.7 μs ± 836 ns, 3.86x
    IORef atomic:       OK (1.35s)
      19.5 μs ± 713 ns, 4.02x
    MVar:               OK (0.14s)
      15.7 μs ± 1.4 μs, 3.24x
    TMVar:              OK (0.20s)
      43.8 μs ± 3.3 μs, 9.07x
    TVar:               OK (0.14s)
      31.4 μs ± 3.0 μs, 6.49x
    Addr:               OK (0.37s)
      4.97 μs ± 348 ns, 1.03x
  Read/write contention with 10000 iterations and 1 threads
    Counter:            OK (2.95s)
      43.0 μs ± 689 ns
    IORef inconsistent: OK (0.20s)
      185  μs ±  15 μs, 4.30x
    IORef atomic:       OK (0.44s)
      214  μs ±  18 μs, 4.97x
    MVar:               OK (2.63s)
      154  μs ± 1.6 μs, 3.58x
    TMVar:              OK (0.42s)
      407  μs ±  11 μs, 9.45x
    TVar:               OK (0.16s)
      302  μs ±  23 μs, 7.02x
    Addr:               OK (0.18s)
      42.0 μs ± 3.4 μs, 0.98x
  Read/write contention with 10 iterations and 2 threads
    Counter:            OK (0.23s)
      6.97 μs ± 487 ns
    IORef inconsistent: OK (0.48s)
      7.78 μs ± 660 ns, 1.12x
    IORef atomic:       OK (0.29s)
      7.84 μs ± 662 ns, 1.12x
    MVar:               OK (0.13s)
      7.24 μs ± 684 ns, 1.04x
    TMVar:              OK (2.54s)
      9.33 μs ± 358 ns, 1.34x
    TVar:               OK (0.14s)
      7.50 μs ± 724 ns, 1.08x
    Addr:               OK (0.28s)
      7.75 μs ± 470 ns, 1.11x
  Read/write contention with 100 iterations and 2 threads
    Counter:            OK (0.30s)
      8.35 μs ± 567 ns
    IORef inconsistent: OK (0.18s)
      12.5 μs ± 902 ns, 1.49x
    IORef atomic:       OK (0.17s)
      12.2 μs ± 903 ns, 1.47x
    MVar:               OK (0.12s)
      14.6 μs ± 1.4 μs, 1.74x
    TMVar:              OK (0.34s)
      31.9 μs ± 1.6 μs, 3.83x
    TVar:               OK (0.66s)
      28.8 μs ± 1.8 μs, 3.45x
    Addr:               OK (0.14s)
      8.09 μs ± 689 ns, 0.97x
  Read/write contention with 1000 iterations and 2 threads
    Counter:            OK (0.12s)
      48.8 μs ± 2.8 μs
    IORef inconsistent: OK (1.14s)
      123  μs ± 3.0 μs, 2.52x
    IORef atomic:       OK (0.15s)
      256  μs ±  22 μs, 5.25x
    MVar:               OK (2.40s)
      4.98 ms ± 337 μs, 102.10x
    TMVar:              OK (0.29s)
      240  μs ±  24 μs, 4.92x
    TVar:               OK (0.17s)
      288  μs ±  25 μs, 5.91x
    Addr:               OK (0.23s)
      47.4 μs ± 1.9 μs, 0.97x
  Read/write contention with 10000 iterations and 2 threads
    Counter:            OK (0.18s)
      626  μs ±  34 μs
    IORef inconsistent: OK (0.09s)
      1.39 ms ± 136 μs, 2.23x
    IORef atomic:       OK (10.19s)
      2.03 ms ± 1.1 ms, 3.24x
    MVar:               OK (0.82s)
      57.8 ms ± 2.7 ms, 92.36x
    TMVar:              OK (0.09s)
      2.37 ms ± 189 μs, 3.78x
    TVar:               OK (0.21s)
      2.98 ms ± 281 μs, 4.76x
    Addr:               OK (0.29s)
      538  μs ±  22 μs, 0.86x
  Read/write contention with 10 iterations and 4 threads
    Counter:            OK (0.09s)
      15.6 μs ± 1.4 μs
    IORef inconsistent: OK (0.16s)
      16.0 μs ± 776 ns, 1.03x
    IORef atomic:       OK (0.34s)
      16.9 μs ± 1.6 μs, 1.08x
    MVar:               OK (0.18s)
      25.5 μs ± 1.4 μs, 1.64x
    TMVar:              OK (0.17s)
      19.9 μs ± 1.2 μs, 1.27x
    TVar:               OK (0.17s)
      18.1 μs ± 1.1 μs, 1.16x
    Addr:               OK (0.07s)
      14.0 μs ± 1.4 μs, 0.90x
  Read/write contention with 100 iterations and 4 threads
    Counter:            OK (0.20s)
      23.4 μs ± 1.6 μs
    IORef inconsistent: OK (0.09s)
      56.3 μs ± 3.3 μs, 2.41x
    IORef atomic:       OK (0.42s)
      143  μs ±  10 μs, 6.11x
    MVar:               OK (0.16s)
      1.19 ms ± 112 μs, 51.05x
    TMVar:              OK (0.25s)
      195  μs ±  19 μs, 8.36x
    TVar:               OK (0.14s)
      202  μs ±  14 μs, 8.65x
    Addr:               OK (0.20s)
      22.5 μs ± 1.7 μs, 0.96x
  Read/write contention with 1000 iterations and 4 threads
    Counter:            OK (0.16s)
      231  μs ±  18 μs
    IORef inconsistent: OK (0.04s)
      453  μs ±  42 μs, 1.96x
    IORef atomic:       OK (0.47s)
      1.36 ms ±  88 μs, 5.90x
    MVar:               OK (0.21s)
      12.7 ms ± 1.0 ms, 54.99x
    TMVar:              OK (0.27s)
      1.77 ms ±  85 μs, 7.69x
    TVar:               OK (0.15s)
      1.81 ms ± 110 μs, 7.87x
    Addr:               OK (0.09s)
      231  μs ±  13 μs, 1.00x
  Read/write contention with 10000 iterations and 4 threads
    Counter:            OK (0.04s)
      2.21 ms ± 197 μs
    IORef inconsistent: OK (0.08s)
      4.54 ms ± 299 μs, 2.05x
    IORef atomic:       OK (5.62s)
      15.0 ms ± 1.2 ms, 6.80x
    MVar:               OK (1.92s)
      124  ms ± 671 μs, 56.22x
    TMVar:              OK (0.17s)
      19.9 ms ± 1.8 ms, 8.97x
    TVar:               OK (0.34s)
      20.0 ms ± 503 μs, 9.04x
    Addr:               OK (0.08s)
      2.30 ms ±  92 μs, 1.04x
  Read/write contention with 10 iterations and 6 threads
    Counter:            OK (0.21s)
      22.4 μs ± 1.9 μs
    IORef inconsistent: OK (0.12s)
      24.8 μs ± 2.0 μs, 1.11x
    IORef atomic:       OK (0.15s)
      35.9 μs ± 2.3 μs, 1.60x
    MVar:               OK (0.15s)
      76.8 μs ± 5.9 μs, 3.44x
    TMVar:              OK (0.60s)
      33.3 μs ± 3.2 μs, 1.49x
    TVar:               OK (0.53s)
      35.0 μs ± 2.6 μs, 1.57x
    Addr:               OK (0.12s)
      22.9 μs ± 2.3 μs, 1.03x
  Read/write contention with 100 iterations and 6 threads
    Counter:            OK (0.08s)
      50.8 μs ± 5.0 μs
    IORef inconsistent: OK (0.06s)
      99.4 μs ± 9.8 μs, 1.96x
    IORef atomic:       OK (0.09s)
      450  μs ±  40 μs, 8.86x
    MVar:               OK (0.25s)
      1.85 ms ±  92 μs, 36.35x
    TMVar:              OK (0.21s)
      412  μs ±  30 μs, 8.11x
    TVar:               OK (0.06s)
      587  μs ±  49 μs, 11.56x
    Addr:               OK (0.58s)
      46.8 μs ± 2.4 μs, 0.92x
  Read/write contention with 1000 iterations and 6 threads
    Counter:            OK (0.13s)
      484  μs ±  21 μs
    IORef inconsistent: OK (3.26s)
      849  μs ± 282 μs, 1.75x
    IORef atomic:       OK (0.53s)
      3.06 ms ± 287 μs, 6.32x
    MVar:               OK (0.30s)
      19.1 ms ± 1.9 ms, 39.47x
    TMVar:              OK (4.08s)
      4.98 ms ± 1.1 ms, 10.28x
    TVar:               OK (0.13s)
      4.85 ms ± 293 μs, 10.01x
    Addr:               OK (0.11s)
      510  μs ±  17 μs, 1.05x
  Read/write contention with 10000 iterations and 6 threads
    Counter:            OK (0.25s)
      4.44 ms ± 128 μs
    IORef inconsistent: OK (0.12s)
      9.94 ms ± 442 μs, 2.24x
    IORef atomic:       OK (6.67s)
      61.3 ms ±  13 ms, 13.82x
    MVar:               OK (2.85s)
      189  ms ± 2.5 ms, 42.55x
    TMVar:              OK (0.16s)
      55.3 ms ± 2.7 ms, 12.45x
    TVar:               OK (0.62s)
      49.7 ms ± 3.6 ms, 11.18x
    Addr:               OK (0.07s)
      5.06 ms ± 282 μs, 1.14x
  Read/write contention with 10 iterations and 8 threads
    Counter:            OK (0.12s)
      25.8 μs ± 1.4 μs
    IORef inconsistent: OK (0.14s)
      29.3 μs ± 2.6 μs, 1.13x
    IORef atomic:       OK (0.21s)
      52.2 μs ± 2.2 μs, 2.02x
    MVar:               OK (0.23s)
      117  μs ± 7.8 μs, 4.54x
    TMVar:              OK (0.09s)
      55.7 μs ± 5.5 μs, 2.16x
    TVar:               OK (1.32s)
      53.4 μs ± 561 ns, 2.07x
    Addr:               OK (0.12s)
      26.0 μs ± 2.4 μs, 1.01x
  Read/write contention with 100 iterations and 8 threads
    Counter:            OK (0.10s)
      63.5 μs ± 3.5 μs
    IORef inconsistent: OK (1.16s)
      123  μs ± 7.7 μs, 1.94x
    IORef atomic:       OK (0.26s)
      632  μs ±  43 μs, 9.94x
    MVar:               OK (0.30s)
      2.27 ms ± 222 μs, 35.73x
    TMVar:              OK (1.15s)
      643  μs ± 9.9 μs, 10.12x
    TVar:               OK (0.29s)
      642  μs ±  52 μs, 10.10x
    Addr:               OK (0.10s)
      61.4 μs ± 5.2 μs, 0.97x
  Read/write contention with 1000 iterations and 8 threads
    Counter:            OK (0.15s)
      553  μs ±  37 μs
    IORef inconsistent: OK (0.04s)
      1.04 ms ±  85 μs, 1.88x
    IORef atomic:       OK (5.60s)
      6.58 ms ± 166 μs, 11.89x
    MVar:               OK (0.36s)
      23.1 ms ± 2.1 ms, 41.80x
    TMVar:              OK (0.17s)
      6.55 ms ± 273 μs, 11.83x
    TVar:               OK (2.59s)
      6.17 ms ± 247 μs, 11.14x
    Addr:               OK (0.08s)
      624  μs ±  27 μs, 1.13x
  Read/write contention with 10000 iterations and 8 threads
    Counter:            OK (0.08s)
      5.04 ms ± 445 μs
    IORef inconsistent: OK (0.08s)
      11.8 ms ± 386 μs, 2.35x
    IORef atomic:       OK (7.71s)
      141  ms ±  12 ms, 27.99x
    MVar:               OK (0.77s)
      255  ms ±  11 ms, 50.70x
    TMVar:              OK (0.80s)
      67.6 ms ± 280 μs, 13.42x
    TVar:               OK (0.10s)
      55.6 ms ± 1.5 ms, 11.03x
    Addr:               OK (0.32s)
      6.25 ms ± 470 μs, 1.24x
  Read/write contention with 10 iterations and 12 threads
    Counter:            OK (0.13s)
      29.0 μs ± 1.9 μs
    IORef inconsistent: OK (0.14s)
      34.2 μs ± 3.2 μs, 1.18x
    IORef atomic:       OK (1.01s)
      73.3 μs ± 1.4 μs, 2.53x
    MVar:               OK (0.23s)
      236  μs ±  20 μs, 8.15x
    TMVar:              OK (0.44s)
      83.2 μs ± 2.8 μs, 2.87x
    TVar:               OK (0.11s)
      66.2 μs ± 3.2 μs, 2.28x
    Addr:               OK (0.08s)
      29.3 μs ± 2.6 μs, 1.01x
  Read/write contention with 100 iterations and 12 threads
    Counter:            OK (0.24s)
      98.7 μs ± 4.9 μs
    IORef inconsistent: OK (0.21s)
      194  μs ±  14 μs, 1.97x
    IORef atomic:       OK (0.43s)
      1.01 ms ±  47 μs, 10.28x
    MVar:               OK (0.43s)
      3.21 ms ±  92 μs, 32.54x
    TMVar:              OK (0.93s)
      1.05 ms ±  13 μs, 10.68x
    TVar:               OK (0.06s)
      1.06 ms ±  64 μs, 10.71x
    Addr:               OK (0.06s)
      92.2 μs ± 8.7 μs, 0.93x
  Read/write contention with 1000 iterations and 12 threads
    Counter:            OK (0.23s)
      1.00 ms ±  48 μs
    IORef inconsistent: OK (0.10s)
      1.95 ms ±  45 μs, 1.94x
    IORef atomic:       OK (0.90s)
      16.7 ms ± 574 μs, 16.58x
    MVar:               OK (0.26s)
      36.1 ms ± 2.8 ms, 35.91x
    TMVar:              OK (0.53s)
      10.8 ms ± 334 μs, 10.72x
    TVar:               OK (0.25s)
      9.37 ms ± 265 μs, 9.33x
    Addr:               OK (0.10s)
      768  μs ±  56 μs, 0.76x
  Read/write contention with 10000 iterations and 12 threads
    Counter:            OK (0.12s)
      8.38 ms ± 385 μs
    IORef inconsistent: OK (0.03s)
      14.6 ms ± 1.4 ms, 1.75x
    IORef atomic:       OK (9.26s)
      282  ms ± 135 ms, 33.61x
    MVar:               OK (1.09s)
      338  ms ±  11 ms, 40.40x
    TMVar:              OK (0.63s)
      106  ms ± 2.3 ms, 12.64x
    TVar:               OK (0.31s)
      100  ms ± 6.2 ms, 11.90x
    Addr:               OK (0.06s)
      9.71 ms ± 794 μs, 1.16x
  Read/write contention with 10 iterations and 16 threads
    Counter:            OK (0.09s)
      40.5 μs ± 2.8 μs
    IORef inconsistent: OK (0.09s)
      48.5 μs ± 4.6 μs, 1.20x
    IORef atomic:       OK (0.13s)
      144  μs ±  10 μs, 3.56x
    MVar:               OK (0.20s)
      400  μs ±  36 μs, 9.86x
    TMVar:              OK (0.59s)
      123  μs ±  11 μs, 3.04x
    TVar:               OK (0.60s)
      130  μs ±  12 μs, 3.21x
    Addr:               OK (0.09s)
      40.5 μs ± 3.5 μs, 1.00x
  Read/write contention with 100 iterations and 16 threads
    Counter:            OK (0.08s)
      133  μs ±  11 μs
    IORef inconsistent: OK (0.07s)
      278  μs ±  23 μs, 2.09x
    IORef atomic:       OK (1.94s)
      1.25 ms ±  76 μs, 9.39x
    MVar:               OK (0.56s)
      4.21 ms ± 380 μs, 31.66x
    TMVar:              OK (0.15s)
      1.48 ms ±  43 μs, 11.12x
    TVar:               OK (0.08s)
      1.40 ms ±  68 μs, 10.55x
    Addr:               OK (0.16s)
      134  μs ± 4.8 μs, 1.00x
  Read/write contention with 1000 iterations and 16 threads
    Counter:            OK (0.29s)
      1.36 ms ±  11 μs
    IORef inconsistent: OK (0.14s)
      2.46 ms ±  69 μs, 1.81x
    IORef atomic:       OK (6.70s)
      15.9 ms ± 1.7 ms, 11.73x
    MVar:               OK (1.38s)
      44.5 ms ± 3.2 ms, 32.79x
    TMVar:              OK (0.34s)
      14.2 ms ± 551 μs, 10.43x
    TVar:               OK (0.17s)
      14.0 ms ± 314 μs, 10.35x
    Addr:               OK (0.07s)
      1.36 ms ± 101 μs, 1.00x
  Read/write contention with 10000 iterations and 16 threads
    Counter:            OK (0.16s)
      10.6 ms ± 651 μs
    IORef inconsistent: OK (0.31s)
      24.4 ms ± 1.1 ms, 2.30x
    IORef atomic:       OK (1.23s)
      755  ms ±  16 ms, 71.31x
    MVar:               OK (1.33s)
      446  ms ±  13 ms, 42.13x
    TMVar:              OK (0.08s)
      135  ms ±  12 ms, 12.80x
    TVar:               OK (1.76s)
      150  ms ± 9.3 ms, 14.18x
    Addr:               OK (0.67s)
      13.3 ms ± 344 μs, 1.25x
  Read/write contention with 10 iterations and 20 threads
    Counter:            OK (0.10s)
      48.7 μs ± 2.7 μs
    IORef inconsistent: OK (0.11s)
      57.7 μs ± 3.4 μs, 1.18x
    IORef atomic:       OK (1.28s)
      185  μs ± 4.3 μs, 3.80x
    MVar:               OK (0.14s)
      548  μs ±  45 μs, 11.25x
    TMVar:              OK (0.05s)
      160  μs ±  11 μs, 3.28x
    TVar:               OK (0.10s)
      168  μs ± 6.2 μs, 3.44x
    Addr:               OK (0.10s)
      48.2 μs ± 3.7 μs, 0.99x
  Read/write contention with 100 iterations and 20 threads
    Counter:            OK (0.10s)
      170  μs ± 8.4 μs
    IORef inconsistent: OK (0.08s)
      329  μs ±  17 μs, 1.93x
    IORef atomic:       OK (0.34s)
      1.85 ms ± 105 μs, 10.83x
    MVar:               OK (0.36s)
      5.37 ms ± 306 μs, 31.52x
    TMVar:              OK (0.19s)
      1.88 ms ± 120 μs, 11.01x
    TVar:               OK (0.09s)
      1.82 ms ± 129 μs, 10.69x
    Addr:               OK (0.05s)
      165  μs ±  15 μs, 0.97x
  Read/write contention with 1000 iterations and 20 threads
    Counter:            OK (0.06s)
      1.28 ms ±  86 μs
    IORef inconsistent: OK (0.32s)
      3.25 ms ± 260 μs, 2.53x
    IORef atomic:       OK (9.51s)
      21.7 ms ± 2.8 ms, 16.88x
    MVar:               OK (0.88s)
      57.9 ms ± 686 μs, 45.07x
    TMVar:              OK (0.22s)
      18.7 ms ± 1.2 ms, 14.56x
    TVar:               OK (0.22s)
      17.7 ms ± 692 μs, 13.78x
    Addr:               OK (0.05s)
      1.71 ms ± 103 μs, 1.33x
  Read/write contention with 10000 iterations and 20 threads
    Counter:            OK (0.10s)
      17.1 ms ± 1.7 ms
    IORef inconsistent: OK (0.10s)
      31.9 ms ± 1.2 ms, 1.86x
    IORef atomic:       OK (1.72s)
      546  ms ±  27 ms, 31.92x
    MVar:               OK (1.92s)
      626  ms ±  48 ms, 36.57x
    TMVar:              OK (0.56s)
      191  ms ± 2.1 ms, 11.18x
    TVar:               OK (0.11s)
      181  ms ± 7.1 ms, 10.55x
    Addr:               OK (0.10s)
      16.4 ms ± 607 μs, 0.96x
  Read/write contention with 10 iterations and 32 threads
    Counter:            OK (0.13s)
      69.9 μs ± 3.9 μs
    IORef inconsistent: OK (0.15s)
      87.7 μs ± 4.1 μs, 1.25x
    IORef atomic:       OK (0.50s)
      185  μs ± 2.4 μs, 2.65x
    MVar:               OK (0.13s)
      923  μs ±  88 μs, 13.20x
    TMVar:              OK (0.13s)
      273  μs ±  14 μs, 3.90x
    TVar:               OK (0.58s)
      308  μs ±  24 μs, 4.41x
    Addr:               OK (0.06s)
      68.4 μs ± 5.9 μs, 0.98x
  Read/write contention with 100 iterations and 32 threads
    Counter:            OK (0.08s)
      280  μs ±  15 μs
    IORef inconsistent: OK (0.12s)
      500  μs ±  18 μs, 1.79x
    IORef atomic:       OK (0.46s)
      1.69 ms ±  28 μs, 6.05x
    MVar:               OK (0.58s)
      8.82 ms ± 308 μs, 31.51x
    TMVar:              OK (0.15s)
      3.01 ms ± 214 μs, 10.75x
    TVar:               OK (0.33s)
      3.09 ms ± 111 μs, 11.04x
    Addr:               OK (0.04s)
      258  μs ±  23 μs, 0.92x
  Read/write contention with 1000 iterations and 32 threads
    Counter:            OK (0.29s)
      2.82 ms ±  44 μs
    IORef inconsistent: OK (0.14s)
      5.04 ms ± 128 μs, 1.79x
    IORef atomic:       OK (3.51s)
      31.6 ms ± 2.7 ms, 11.22x
    MVar:               OK (1.41s)
      88.8 ms ± 6.3 ms, 31.49x
    TMVar:              OK (0.08s)
      29.6 ms ± 2.2 ms, 10.51x
    TVar:               OK (0.09s)
      32.4 ms ± 3.0 ms, 11.49x
    Addr:               OK (0.08s)
      2.25 ms ± 154 μs, 0.80x
  Read/write contention with 10000 iterations and 32 threads
    Counter:            OK (0.16s)
      24.5 ms ± 1.8 ms
    IORef inconsistent: OK (0.15s)
      51.2 ms ± 3.3 ms, 2.09x
    IORef atomic:       OK (10.26s)
      1.292 s ± 657 ms, 52.67x
    MVar:               OK (3.08s)
      976  ms ±  51 ms, 39.78x
    TMVar:              OK (0.41s)
      329  ms ± 2.9 ms, 13.42x
    TVar:               OK (0.18s)
      311  ms ± 3.1 ms, 12.69x
    Addr:               OK (0.17s)
      24.8 ms ± 2.2 ms, 1.01x
  Read/write contention with 10 iterations and 64 threads
    Counter:            OK (0.10s)
      125  μs ± 7.6 μs
    IORef inconsistent: OK (0.06s)
      158  μs ±  13 μs, 1.26x
    IORef atomic:       OK (0.10s)
      307  μs ±  12 μs, 2.46x
    MVar:               OK (0.12s)
      1.88 ms ± 179 μs, 15.03x
    TMVar:              OK (0.25s)
      555  μs ±  13 μs, 4.44x
    TVar:               OK (0.14s)
      538  μs ±  12 μs, 4.31x
    Addr:               OK (0.06s)
      122  μs ±  12 μs, 0.98x
  Read/write contention with 100 iterations and 64 threads
    Counter:            OK (0.07s)
      566  μs ±  28 μs
    IORef inconsistent: OK (0.07s)
      1.04 ms ±  44 μs, 1.83x
    IORef atomic:       OK (0.58s)
      3.64 ms ± 107 μs, 6.44x
    MVar:               OK (0.12s)
      16.7 ms ± 1.5 ms, 29.58x
    TMVar:              OK (0.15s)
      6.21 ms ± 213 μs, 10.97x
    TVar:               OK (0.15s)
      6.07 ms ± 175 μs, 10.74x
    Addr:               OK (0.07s)
      556  μs ±  43 μs, 0.98x
  Read/write contention with 1000 iterations and 64 threads
    Counter:            OK (0.15s)
      5.96 ms ± 302 μs
    IORef inconsistent: OK (0.13s)
      10.6 ms ± 514 μs, 1.77x
    IORef atomic:       OK (1.27s)
      100  ms ± 2.7 ms, 16.80x
    MVar:               OK (1.33s)
      188  ms ± 6.1 ms, 31.47x
    TMVar:              OK (0.18s)
      64.5 ms ± 1.1 ms, 10.83x
    TVar:               OK (0.18s)
      63.8 ms ± 4.2 ms, 10.70x
    Addr:               OK (0.07s)
      5.89 ms ± 510 μs, 0.99x
  Read/write contention with 10000 iterations and 64 threads
    Counter:            OK (0.17s)
      58.3 ms ± 4.6 ms
    IORef inconsistent: OK (0.29s)
      105  ms ± 6.9 ms, 1.79x
    IORef atomic:       OK (11.68s)
      3.061 s ± 627 ms, 52.46x
    MVar:               OK (6.29s)
      2.002 s ± 104 ms, 34.31x
    TMVar:              OK (1.73s)
      632  ms ± 1.0 ms, 10.84x
    TVar:               OK (0.82s)
      654  ms ±  49 ms, 11.21x
    Addr:               OK (0.17s)
      58.7 ms ± 4.6 ms, 1.01x
  Read/write contention with 10 iterations and 128 threads
    Counter:            OK (0.09s)
      228  μs ±  22 μs
    IORef inconsistent: OK (0.11s)
      310  μs ±  22 μs, 1.36x
    IORef atomic:       OK (0.09s)
      632  μs ±  49 μs, 2.78x
    MVar:               OK (0.12s)
      3.77 ms ± 372 μs, 16.54x
    TMVar:              OK (0.04s)
      1.07 ms ±  94 μs, 4.70x
    TVar:               OK (0.51s)
      1.04 ms ±  20 μs, 4.55x
    Addr:               OK (0.08s)
      220  μs ±  14 μs, 0.97x
  Read/write contention with 100 iterations and 128 threads
    Counter:            OK (0.26s)
      1.15 ms ±  27 μs
    IORef inconsistent: OK (0.13s)
      2.05 ms ± 117 μs, 1.78x
    IORef atomic:       OK (0.35s)
      8.17 ms ± 736 μs, 7.09x
    MVar:               OK (1.12s)
      34.8 ms ± 1.4 ms, 30.25x
    TMVar:              OK (0.15s)
      12.9 ms ± 761 μs, 11.16x
    TVar:               OK (0.15s)
      13.3 ms ± 1.3 ms, 11.54x
    Addr:               OK (0.07s)
      1.12 ms ±  62 μs, 0.97x
  Read/write contention with 1000 iterations and 128 threads
    Counter:            OK (0.57s)
      11.8 ms ± 184 μs
    IORef inconsistent: OK (0.07s)
      17.5 ms ± 1.6 ms, 1.48x
    IORef atomic:       OK (8.50s)
      266  ms ± 173 ms, 22.55x
    MVar:               OK (1.12s)
      367  ms ±  17 ms, 31.14x
    TMVar:              OK (0.36s)
      136  ms ±  12 ms, 11.54x
    TVar:               OK (0.72s)
      128  ms ±  12 ms, 10.87x
    Addr:               OK (2.23s)
      11.5 ms ± 522 μs, 0.98x
  Read/write contention with 10000 iterations and 128 threads
    Counter:            OK (0.68s)
      116  ms ± 3.7 ms
    IORef inconsistent: OK (0.27s)
      211  ms ± 1.8 ms, 1.82x
    IORef atomic:       OK (10.67s)
      6.495 s ± 1.45 s, 56.11x
    MVar:               OK (12.25s)
      3.909 s ± 296 ms, 33.77x
    TMVar:              OK (1.63s)
      1.280 s ± 127 ms, 11.06x
    TVar:               OK (0.72s)
      1.323 s ±  46 ms, 11.43x
    Addr:               OK (0.16s)
      122  ms ± 7.8 ms, 1.05x

All 309 tests passed (267.00s)

```

# Memory overhead

Each unlifted value of type `Counter` is a singleton mutable array
from GHC primitives under the hood. Thus it occupies at least
`platform integer size` + `array size` + `header` bytes which should
typically be at least 3 machine words. Lifted values may occupy more
depending on optimizations.
