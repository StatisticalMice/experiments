# Comparison of Python/Pandas/Numba and Julia

Generating 1_000_000 random samples from the standard distribution
* Python 30.1 ms
* Julia   9.676 ms (4.67% GC)

Calculate the rolling mean with window size 10
* Python 31.3 ms
* Julia  12.291 ms (3.67% GC)

Calculate the rolling mean with window size 10 (via user-defined function)
* Python 6320 ms 
* Python  202 ms (with Numba)
* Julia    12.730 ms (3.74% GC)
