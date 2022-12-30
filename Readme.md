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
$ cabal run --builddir /tmp/dist bench -- -j1 --timeout 30
All
  Correctness:          OK (15.52s)
    +++ OK, passed 10000 tests.
  Read/write contention with 10 iterations and 1 threads
    Counter:            OK (0.42s)
      785  ns ±  27 ns
    IORef inconsistent: OK (0.16s)
      1.12 μs ±  95 ns
    IORef atomic:       OK (0.16s)
      1.08 μs ±  99 ns
    MVar:               OK (0.62s)
      1.15 μs ±  40 ns
    TMVar:              OK (0.50s)
      896  ns ±  31 ns
    TVar:               OK (0.15s)
      1.01 μs ±  92 ns
    Addr:               OK (0.74s)
      679  ns ±  48 ns
  Read/write contention with 100 iterations and 1 threads
    Counter:            OK (0.15s)
      1.01 μs ±  89 ns
    IORef inconsistent: OK (0.23s)
      3.38 μs ± 198 ns
    IORef atomic:       OK (0.44s)
      3.03 μs ±  91 ns
    MVar:               OK (0.24s)
      3.35 μs ± 167 ns
    TMVar:              OK (0.17s)
      4.64 μs ± 328 ns
    TVar:               OK (0.14s)
      4.09 μs ± 372 ns
    Addr:               OK (0.57s)
      1.04 μs ±  43 ns
  Read/write contention with 1000 iterations and 1 threads
    Counter:            OK (0.17s)
      4.80 μs ± 389 ns
    IORef inconsistent: OK (0.16s)
      18.4 μs ± 1.4 μs
    IORef atomic:       OK (0.19s)
      23.3 μs ± 2.1 μs
    MVar:               OK (0.13s)
      15.6 μs ± 1.3 μs
    TMVar:              OK (0.21s)
      49.5 μs ± 4.8 μs
    TVar:               OK (0.16s)
      38.3 μs ± 3.3 μs
    Addr:               OK (0.22s)
      5.88 μs ± 369 ns
  Read/write contention with 10000 iterations and 1 threads
    Counter:            OK (0.18s)
      41.8 μs ± 2.7 μs
    IORef inconsistent: OK (0.23s)
      224  μs ±  21 μs
    IORef atomic:       OK (0.18s)
      169  μs ±  15 μs
    MVar:               OK (0.20s)
      187  μs ±  15 μs
    TMVar:              OK (0.21s)
      409  μs ±  25 μs
    TVar:               OK (0.16s)
      304  μs ±  25 μs
    Addr:               OK (0.22s)
      52.7 μs ± 4.1 μs
  Read/write contention with 10 iterations and 2 threads
    Counter:            OK (0.11s)
      7.05 μs ± 667 ns
    IORef inconsistent: OK (0.22s)
      7.43 μs ± 599 ns
    IORef atomic:       OK (0.22s)
      7.19 μs ± 719 ns
    MVar:               OK (0.47s)
      7.42 μs ± 671 ns
    TMVar:              OK (0.24s)
      8.01 μs ± 773 ns
    TVar:               OK (0.22s)
      7.45 μs ± 585 ns
    Addr:               OK (0.23s)
      6.99 μs ± 601 ns
  Read/write contention with 100 iterations and 2 threads
    Counter:            OK (0.16s)
      8.88 μs ± 749 ns
    IORef inconsistent: OK (0.16s)
      12.1 μs ± 975 ns
    IORef atomic:       OK (0.16s)
      12.1 μs ± 736 ns
    MVar:               OK (0.13s)
      17.6 μs ± 1.7 μs
    TMVar:              OK (0.18s)
      29.7 μs ± 2.2 μs
    TVar:               OK (0.14s)
      24.8 μs ± 2.0 μs
    Addr:               OK (0.29s)
      8.52 μs ± 555 ns
  Read/write contention with 1000 iterations and 2 threads
    Counter:            OK (0.85s)
      42.8 μs ± 1.2 μs
    IORef inconsistent: OK (4.85s)
      140  μs ± 2.1 μs
    IORef atomic:       OK (0.30s)
      277  μs ±  12 μs
    MVar:               OK (0.33s)
      5.29 ms ± 224 μs
    TMVar:              OK (0.64s)
      270  μs ±  16 μs
    TVar:               OK (0.64s)
      306  μs ±  18 μs
    Addr:               OK (0.23s)
      47.4 μs ± 2.1 μs
  Read/write contention with 10000 iterations and 2 threads
    Counter:            OK (0.11s)
      298  μs ±  28 μs
    IORef inconsistent: OK (0.08s)
      1.01 ms ±  99 μs
    IORef atomic:       OK (0.99s)
      1.26 ms ±  19 μs
    MVar:               OK (0.45s)
      62.7 ms ± 3.9 ms
    TMVar:              OK (0.42s)
      2.96 ms ± 257 μs
    TVar:               OK (6.90s)
      3.26 ms ± 149 μs
    Addr:               OK (0.08s)
      527  μs ±  45 μs
  Read/write contention with 10 iterations and 4 threads
    Counter:            OK (0.16s)
      15.0 μs ± 820 ns
    IORef inconsistent: OK (0.17s)
      16.4 μs ± 946 ns
    IORef atomic:       OK (0.38s)
      19.2 μs ± 937 ns
    MVar:               OK (0.20s)
      27.5 μs ± 2.4 μs
    TMVar:              OK (0.10s)
      20.2 μs ± 1.3 μs
    TVar:               OK (0.19s)
      18.2 μs ± 781 ns
    Addr:               OK (0.09s)
      15.5 μs ± 1.4 μs
  Read/write contention with 100 iterations and 4 threads
    Counter:            OK (0.40s)
      23.1 μs ± 1.8 μs
    IORef inconsistent: OK (0.09s)
      56.3 μs ± 4.7 μs
    IORef atomic:       OK (0.24s)
      157  μs ± 3.9 μs
    MVar:               OK (0.16s)
      1.20 ms ± 115 μs
    TMVar:              OK (0.07s)
      193  μs ±  19 μs
    TVar:               OK (0.13s)
      214  μs ± 7.9 μs
    Addr:               OK (0.39s)
      22.3 μs ± 491 ns
  Read/write contention with 1000 iterations and 4 threads
    Counter:            OK (0.17s)
      242  μs ±  13 μs
    IORef inconsistent: OK (0.07s)
      485  μs ±  32 μs
    IORef atomic:       OK (2.02s)
      1.51 ms ±  54 μs
    MVar:               OK (0.19s)
      12.4 ms ± 1.2 ms
    TMVar:              OK (0.14s)
      2.01 ms ±  76 μs
    TVar:               OK (0.07s)
      1.92 ms ±  99 μs
    Addr:               OK (0.08s)
      233  μs ±  12 μs
  Read/write contention with 10000 iterations and 4 threads
    Counter:            OK (0.17s)
      2.44 ms ±  82 μs
    IORef inconsistent: OK (0.08s)
      4.49 ms ± 291 μs
    IORef atomic:       OK (0.84s)
      18.3 ms ± 941 μs
    MVar:               OK (0.39s)
      125  ms ±  12 ms
    TMVar:              OK (0.16s)
      18.7 ms ± 1.8 ms
    TVar:               OK (0.08s)
      18.8 ms ± 1.3 ms
    Addr:               OK (0.67s)
      2.27 ms ±  80 μs
  Read/write contention with 10 iterations and 6 threads
    Counter:            OK (0.23s)
      24.1 μs ± 1.7 μs
    IORef inconsistent: OK (0.25s)
      26.1 μs ± 1.4 μs
    IORef atomic:       OK (0.18s)
      41.1 μs ± 2.2 μs
    MVar:               OK (0.32s)
      46.8 μs ± 3.2 μs
    TMVar:              OK (0.53s)
      33.8 μs ± 2.5 μs
    TVar:               OK (0.07s)
      32.5 μs ± 2.7 μs
    Addr:               OK (0.12s)
      24.5 μs ± 2.4 μs
  Read/write contention with 100 iterations and 6 threads
    Counter:            OK (0.29s)
      40.2 μs ± 3.1 μs
    IORef inconsistent: OK (0.12s)
      100  μs ± 5.9 μs
    IORef atomic:       OK (0.14s)
      352  μs ±  30 μs
    MVar:               OK (0.12s)
      1.82 ms ± 168 μs
    TMVar:              OK (0.92s)
      544  μs ±  43 μs
    TVar:               OK (0.23s)
      561  μs ±  28 μs
    Addr:               OK (0.30s)
      38.9 μs ± 2.1 μs
  Read/write contention with 1000 iterations and 6 threads
    Counter:            OK (0.87s)
      477  μs ± 4.3 μs
    IORef inconsistent: OK (0.20s)
      1.01 ms ±  61 μs
    IORef atomic:       OK (0.85s)
      3.05 ms ± 154 μs
    MVar:               OK (0.62s)
      19.8 ms ± 1.5 ms
    TMVar:              OK (0.06s)
      4.65 ms ± 428 μs
    TVar:               OK (0.25s)
      4.78 ms ± 117 μs
    Addr:               OK (0.22s)
      477  μs ±  32 μs
  Read/write contention with 10000 iterations and 6 threads
    Counter:            OK (2.01s)
      4.82 ms ± 131 μs
    IORef inconsistent: OK (0.12s)
      9.87 ms ± 230 μs
    IORef atomic:       OK (6.61s)
      66.0 ms ±  24 ms
    MVar:               OK (1.31s)
      194  ms ± 3.5 ms
    TMVar:              OK (0.63s)
      52.3 ms ± 290 μs
    TVar:               OK (0.07s)
      48.5 ms ± 4.5 ms
    Addr:               OK (0.03s)
      4.82 ms ± 479 μs
  Read/write contention with 10 iterations and 8 threads
    Counter:            OK (0.13s)
      26.3 μs ± 2.1 μs
    IORef inconsistent: OK (0.16s)
      28.8 μs ± 1.8 μs
    IORef atomic:       OK (0.25s)
      56.2 μs ± 5.2 μs
    MVar:               OK (0.71s)
      95.8 μs ± 9.1 μs
    TMVar:              OK (0.19s)
      52.3 μs ± 1.8 μs
    TVar:               OK (0.18s)
      47.9 μs ± 2.0 μs
    Addr:               OK (0.13s)
      26.1 μs ± 1.9 μs
  Read/write contention with 100 iterations and 8 threads
    Counter:            OK (0.05s)
      60.9 μs ± 5.5 μs
    IORef inconsistent: OK (0.58s)
      120  μs ± 2.4 μs
    IORef atomic:       OK (0.62s)
      706  μs ± 9.6 μs
    MVar:               OK (0.16s)
      2.26 ms ± 209 μs
    TMVar:              OK (0.07s)
      571  μs ±  53 μs
    TVar:               OK (0.28s)
      556  μs ± 6.8 μs
    Addr:               OK (0.10s)
      58.8 μs ± 5.0 μs
  Read/write contention with 1000 iterations and 8 threads
    Counter:            OK (0.28s)
      588  μs ±  17 μs
    IORef inconsistent: OK (1.07s)
      1.15 ms ± 8.3 μs
    IORef atomic:       OK (5.86s)
      6.91 ms ± 986 μs
    MVar:               OK (0.71s)
      22.9 ms ± 842 μs
    TMVar:              OK (0.16s)
      6.10 ms ± 542 μs
    TVar:               OK (0.09s)
      5.76 ms ± 457 μs
    Addr:               OK (0.07s)
      624  μs ±  49 μs
  Read/write contention with 10000 iterations and 8 threads
    Counter:            OK (0.65s)
      5.99 ms ± 259 μs
    IORef inconsistent: OK (0.08s)
      11.4 ms ± 403 μs
    IORef atomic:       OK (9.50s)
      160  ms ±  43 ms
    MVar:               OK (0.74s)
      246  ms ±  21 ms
    TMVar:              OK (0.04s)
      50.4 ms ± 3.0 ms
    TVar:               OK (0.79s)
      58.5 ms ± 1.3 ms
    Addr:               OK (0.17s)
      5.51 ms ± 226 μs
  Read/write contention with 10 iterations and 12 threads
    Counter:            OK (0.14s)
      29.8 μs ± 2.8 μs
    IORef inconsistent: OK (0.08s)
      34.5 μs ± 2.8 μs
    IORef atomic:       OK (0.27s)
      71.0 μs ± 3.3 μs
    MVar:               OK (0.23s)
      229  μs ±  15 μs
    TMVar:              OK (0.87s)
      81.1 μs ± 697 ns
    TVar:               OK (0.21s)
      76.2 μs ± 5.6 μs
    Addr:               OK (0.14s)
      29.3 μs ± 2.3 μs
  Read/write contention with 100 iterations and 12 threads
    Counter:            OK (0.25s)
      92.8 μs ± 7.8 μs
    IORef inconsistent: OK (0.06s)
      179  μs ±  16 μs
    IORef atomic:       OK (0.43s)
      976  μs ±  30 μs
    MVar:               OK (0.43s)
      3.07 ms ± 172 μs
    TMVar:              OK (0.11s)
      946  μs ±  44 μs
    TVar:               OK (0.24s)
      1.05 ms ±  43 μs
    Addr:               OK (0.24s)
      90.4 μs ± 4.5 μs
  Read/write contention with 1000 iterations and 12 threads
    Counter:            OK (0.11s)
      1.01 ms ±  37 μs
    IORef inconsistent: OK (0.10s)
      1.76 ms ± 174 μs
    IORef atomic:       OK (2.73s)
      11.9 ms ± 434 μs
    MVar:               OK (0.28s)
      36.3 ms ± 2.7 ms
    TMVar:              OK (0.07s)
      10.6 ms ± 819 μs
    TVar:               OK (0.25s)
      9.24 ms ± 596 μs
    Addr:               OK (0.84s)
      902  μs ±  61 μs
  Read/write contention with 10000 iterations and 12 threads
    Counter:            OK (0.25s)
      9.15 ms ± 853 μs
    IORef inconsistent: OK (0.12s)
      18.7 ms ± 904 μs
    IORef atomic:       OK (2.28s)
      292  ms ±  19 ms
    MVar:               OK (5.38s)
      350  ms ± 4.1 ms
    TMVar:              OK (2.57s)
      104  ms ± 5.6 ms
    TVar:               OK (0.62s)
      104  ms ± 3.0 ms
    Addr:               OK (0.12s)
      9.65 ms ± 799 μs
  Read/write contention with 10 iterations and 16 threads
    Counter:            OK (0.09s)
      40.4 μs ± 2.8 μs
    IORef inconsistent: OK (0.11s)
      47.7 μs ± 2.9 μs
    IORef atomic:       OK (0.29s)
      133  μs ± 8.6 μs
    MVar:               OK (0.76s)
      390  μs ±  13 μs
    TMVar:              OK (0.08s)
      98.5 μs ± 6.9 μs
    TVar:               OK (1.18s)
      125  μs ±  10 μs
    Addr:               OK (0.09s)
      40.6 μs ± 2.7 μs
  Read/write contention with 100 iterations and 16 threads
    Counter:            OK (0.08s)
      133  μs ± 6.5 μs
    IORef inconsistent: OK (0.14s)
      275  μs ± 9.8 μs
    IORef atomic:       OK (1.04s)
      1.28 ms ±  31 μs
    MVar:               OK (0.57s)
      4.34 ms ± 330 μs
    TMVar:              OK (0.60s)
      1.39 ms ± 8.3 μs
    TVar:               OK (0.16s)
      1.40 ms ±  73 μs
    Addr:               OK (0.16s)
      130  μs ±  12 μs
  Read/write contention with 1000 iterations and 16 threads
    Counter:            OK (0.30s)
      1.31 ms ±  82 μs
    IORef inconsistent: OK (0.14s)
      2.51 ms ±  56 μs
    IORef atomic:       OK (3.03s)
      14.6 ms ± 1.4 ms
    MVar:               OK (0.71s)
      46.5 ms ± 3.9 ms
    TMVar:              OK (0.08s)
      13.8 ms ± 721 μs
    TVar:               OK (0.69s)
      12.8 ms ± 806 μs
    Addr:               OK (0.29s)
      1.33 ms ±  23 μs
  Read/write contention with 10000 iterations and 16 threads
    Counter:            OK (0.16s)
      12.9 ms ± 1.1 ms
    IORef inconsistent: OK (0.15s)
      25.5 ms ± 2.2 ms
    IORef atomic:       OK (8.01s)
      515  ms ±  51 ms
    MVar:               OK (1.57s)
      501  ms ±  31 ms
    TMVar:              OK (0.88s)
      149  ms ±  11 ms
    TVar:               OK (1.72s)
      145  ms ± 1.1 ms
    Addr:               OK (0.08s)
      11.4 ms ± 480 μs
  Read/write contention with 10 iterations and 20 threads
    Counter:            OK (0.11s)
      50.2 μs ± 4.9 μs
    IORef inconsistent: OK (0.10s)
      57.2 μs ± 3.8 μs
    IORef atomic:       OK (0.35s)
      192  μs ±  17 μs
    MVar:               OK (0.26s)
      533  μs ±  29 μs
    TMVar:              OK (0.10s)
      162  μs ±  14 μs
    TVar:               OK (0.05s)
      166  μs ±  11 μs
    Addr:               OK (0.09s)
      48.5 μs ± 3.3 μs
  Read/write contention with 100 iterations and 20 threads
    Counter:            OK (0.05s)
      170  μs ±  14 μs
    IORef inconsistent: OK (0.16s)
      334  μs ±  11 μs
    IORef atomic:       OK (0.53s)
      1.44 ms ± 112 μs
    MVar:               OK (0.35s)
      5.29 ms ± 217 μs
    TMVar:              OK (0.05s)
      1.90 ms ± 114 μs
    TVar:               OK (0.05s)
      1.75 ms ± 111 μs
    Addr:               OK (0.05s)
      131  μs ±  11 μs
  Read/write contention with 1000 iterations and 20 threads
    Counter:            OK (0.18s)
      1.73 ms ±  38 μs
    IORef inconsistent: OK (0.04s)
      3.06 ms ± 305 μs
    IORef atomic:       OK (0.44s)
      17.4 ms ± 797 μs
    MVar:               OK (1.80s)
      56.3 ms ± 1.3 ms
    TMVar:              OK (0.44s)
      18.7 ms ± 761 μs
    TVar:               OK (0.05s)
      18.3 ms ± 1.8 ms
    Addr:               OK (0.05s)
      1.72 ms ± 103 μs
  Read/write contention with 10000 iterations and 20 threads
    Counter:            OK (0.05s)
      17.6 ms ± 1.2 ms
    IORef inconsistent: OK (0.04s)
      31.2 ms ± 1.8 ms
    IORef atomic:       OK (10.54s)
      659  ms ± 346 ms
    MVar:               OK (1.96s)
      627  ms ±  46 ms
    TMVar:              OK (1.10s)
      192  ms ± 9.0 ms
    TVar:               OK (1.09s)
      183  ms ±  16 ms
    Addr:               OK (0.21s)
      17.0 ms ± 260 μs
  Read/write contention with 10 iterations and 32 threads
    Counter:            OK (0.12s)
      70.1 μs ± 2.7 μs
    IORef inconsistent: OK (0.08s)
      86.8 μs ± 7.5 μs
    IORef atomic:       OK (0.95s)
      183  μs ±  12 μs
    MVar:               OK (0.23s)
      913  μs ±  46 μs
    TMVar:              OK (0.07s)
      281  μs ±  15 μs
    TVar:               OK (0.07s)
      283  μs ±  20 μs
    Addr:               OK (0.12s)
      69.4 μs ± 3.2 μs
  Read/write contention with 100 iterations and 32 threads
    Counter:            OK (0.05s)
      280  μs ±  27 μs
    IORef inconsistent: OK (0.06s)
      487  μs ±  37 μs
    IORef atomic:       OK (0.07s)
      1.89 ms ± 121 μs
    MVar:               OK (0.57s)
      8.50 ms ± 703 μs
    TMVar:              OK (0.04s)
      3.04 ms ± 280 μs
    TVar:               OK (0.07s)
      2.95 ms ± 175 μs
    Addr:               OK (0.04s)
      277  μs ±  26 μs
  Read/write contention with 1000 iterations and 32 threads
    Counter:            OK (0.08s)
      2.75 ms ± 198 μs
    IORef inconsistent: OK (0.03s)
      5.20 ms ± 388 μs
    IORef atomic:       OK (0.39s)
      22.5 ms ± 809 μs
    MVar:               OK (0.68s)
      92.3 ms ± 4.3 ms
    TMVar:              OK (0.18s)
      31.4 ms ± 2.2 ms
    TVar:               OK (0.09s)
      30.0 ms ± 2.5 ms
    Addr:               OK (0.04s)
      2.85 ms ± 243 μs
  Read/write contention with 10000 iterations and 32 threads
    Counter:            OK (0.08s)
      28.1 ms ± 708 μs
    IORef inconsistent: OK (0.07s)
      53.6 ms ± 1.4 ms
    IORef atomic:       OK (9.80s)
      1.223 s ± 410 ms
    MVar:               OK (7.32s)
      1.009 s ±  37 ms
    TMVar:              OK (0.17s)
      296  ms ±  23 ms
    TVar:               OK (1.76s)
      302  ms ± 5.6 ms
    Addr:               OK (0.08s)
      29.1 ms ± 2.6 ms
  Read/write contention with 10 iterations and 64 threads
    Counter:            OK (0.05s)
      127  μs ±  12 μs
    IORef inconsistent: OK (0.12s)
      158  μs ± 5.8 μs
    IORef atomic:       OK (0.05s)
      320  μs ±  29 μs
    MVar:               OK (0.23s)
      1.86 ms ± 145 μs
    TMVar:              OK (1.06s)
      529  μs ±  41 μs
    TVar:               OK (0.26s)
      569  μs ±  24 μs
    Addr:               OK (0.06s)
      125  μs ±  11 μs
  Read/write contention with 100 iterations and 64 threads
    Counter:            OK (0.04s)
      564  μs ±  47 μs
    IORef inconsistent: OK (0.04s)
      1.03 ms ±  98 μs
    IORef atomic:       OK (0.31s)
      3.96 ms ± 240 μs
    MVar:               OK (1.15s)
      17.6 ms ± 1.3 ms
    TMVar:              OK (0.30s)
      6.30 ms ± 476 μs
    TVar:               OK (0.30s)
      6.28 ms ±  66 μs
    Addr:               OK (0.06s)
      560  μs ±  24 μs
  Read/write contention with 1000 iterations and 64 threads
    Counter:            OK (0.14s)
      4.93 ms ±  91 μs
    IORef inconsistent: OK (0.03s)
      8.83 ms ± 711 μs
    IORef atomic:       OK (6.68s)
      110  ms ±  21 ms
    MVar:               OK (1.34s)
      188  ms ± 7.7 ms
    TMVar:              OK (0.36s)
      64.3 ms ± 4.0 ms
    TVar:               OK (0.08s)
      64.3 ms ± 2.3 ms
    Addr:               OK (0.04s)
      5.85 ms ± 457 μs
  Read/write contention with 10000 iterations and 64 threads
    Counter:            OK (0.08s)
      59.1 ms ± 2.2 ms
    IORef inconsistent: OK (0.14s)
      102  ms ± 5.3 ms
    IORef atomic:       OK (11.44s)
      2.924 s ± 306 ms
    MVar:               OK (6.17s)
      1.982 s ±  34 ms
    TMVar:              OK (0.35s)
      657  ms ± 5.3 ms
    TVar:               OK (3.62s)
      631  ms ±  83 ms
    Addr:               OK (0.03s)
      47.7 ms ± 3.7 ms
  Read/write contention with 10 iterations and 128 threads
    Counter:            OK (0.08s)
      232  μs ±  11 μs
    IORef inconsistent: OK (0.12s)
      322  μs ±  11 μs
    IORef atomic:       OK (0.36s)
      636  μs ±  14 μs
    MVar:               OK (0.23s)
      3.65 ms ± 264 μs
    TMVar:              OK (1.04s)
      1.07 ms ±  59 μs
    TVar:               OK (0.07s)
      1.09 ms ±  97 μs
    Addr:               OK (0.05s)
      227  μs ±  22 μs
  Read/write contention with 100 iterations and 128 threads
    Counter:            OK (0.04s)
      1.16 ms ±  85 μs
    IORef inconsistent: OK (0.03s)
      2.08 ms ± 182 μs
    IORef atomic:       OK (0.07s)
      5.79 ms ± 393 μs
    MVar:               OK (1.14s)
      35.6 ms ± 1.1 ms
    TMVar:              OK (0.62s)
      13.1 ms ± 417 μs
    TVar:               OK (0.07s)
      10.4 ms ± 412 μs
    Addr:               OK (0.13s)
      1.10 ms ±  95 μs
  Read/write contention with 1000 iterations and 128 threads
    Counter:            OK (0.04s)
      11.7 ms ± 1.2 ms
    IORef inconsistent: OK (0.06s)
      21.8 ms ± 2.0 ms
    IORef atomic:       OK (10.69s)
      318  ms ±  82 ms
    MVar:               OK (5.87s)
      373  ms ± 8.3 ms
    TMVar:              OK (0.36s)
      133  ms ± 4.1 ms
    TVar:               OK (0.17s)
      128  ms ± 8.6 ms
    Addr:               OK (0.07s)
      10.0 ms ± 504 μs
  Read/write contention with 10000 iterations and 128 threads
    Counter:            OK (0.33s)
      119  ms ± 8.4 ms
    IORef inconsistent: OK (0.28s)
      210  ms ±  11 ms
    IORef atomic:       OK (11.27s)
      6.930 s ± 818 ms
    MVar:               OK (12.48s)
      3.977 s ±  43 ms
    TMVar:              OK (3.52s)
      1.316 s ±  11 ms
    TVar:               OK (1.60s)
      1.291 s ± 126 ms
    Addr:               OK (0.67s)
      118  ms ± 3.0 ms

All 309 tests passed (264.24s)
```

# Memory overhead

Each unlifted value of type `Counter` is a singleton mutable array
from GHC primitives under the hood. Thus it occupies at least
`platform integer size` + `array size` + `header` bytes which should
typically be at least 3 machine words. Lifted values may occupy more
depending on optimizations.
