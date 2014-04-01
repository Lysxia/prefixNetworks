To run the benchmark(s)

    make benches/BenchSklansky
    benches/BenchSklansky +RTS $(RTSOPTS)

Repa Sklansky is sloooow:
140 times `scanl1` on 36000 `Int`'s for `(+)`.
