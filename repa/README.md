To run the benchmark(s)

    make benches/BenchSklansky
    benches/BenchSklansky +RTS $(RTSOPTS)

Repa Sklansky is sloooow:
80 times `scanl1` on 30000 `Int`'s for `(+)`
(good parallelisation on 2 HECS, down to 40 times).
