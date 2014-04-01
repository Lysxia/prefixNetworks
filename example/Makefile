example.pdf: net.tex example.tex
	pdflatex example.tex

net.tex: ExampleTikz
	./ExampleTikz

ExampleTikz: ExampleTikz.hs
	ghc -i.. --make ExampleTikz.hs
