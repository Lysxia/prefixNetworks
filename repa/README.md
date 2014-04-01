To run the benchmark(s)

    make benches/BenchSklansky
    benches/BenchSklansky +RTS $(RTSOPTS)

Repa Sklansky is sloooow:
80 times `scanl1` on 30,000 `Int`'s for `(+)`
(good parallelisation on 2 HECS, down to 40 times).

Sklansky for n elements uses approx. (n log n)/2 operations,
for n =(approx.) 30,000, that's approx. 200,000 operations,
*much more* than 29,999 in a sequential setting.

From these calculations, there also seems to be a huge overhead
due (among other reasons) to the fact
that repa is copying half of the arrays every time, which is bad.

