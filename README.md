Composition:

_Core file_, `WSO.hs`, implementation of prefix networks described in \[1\]
(reference inside)

`Memo.hs`, generic memoization, credit to Koen Claessen

`Numbers.hs`, code measuring various statistics (cur. widths and depths)
of produced networks

`Pretty.hs`, circuit pretty printer

`example/`.

`Makefile`.

To generate *HTML* documentation for `WSO` as `doc/WSO.html`:

    make

`example/ExampleTikz.hs` illustrates the use of the function
`Pretty.renderNetFile`,
generating a file `example/net.tex`.
It is then included inside `example/example.tex`
to produce `example/example.pdf`.

---

To do:

- Print (wide) networks vertically

