example.pdf: exampleTikz.tex exampleTikz.tex
	pdflatex example.tex

%.tex: %.hs
	ghc -i.. --make $<
	./$*

