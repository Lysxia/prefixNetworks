TEX_TO_PDF=rubber --pdf -s -f -q -I figures

FIG=sklanskyrec \
		openSlice \
		make make1 \
		lafi64 make8 make16 make32 make64 ppf64 nets64
TEX=$(FIG:%=figures/%) table
DOC=hsnet
DOCTEX=$(DOC).tex
ARCHIVE=$(DOC).tar.gz
LHS=Construct ../PNet
SRC=$(TEX:%=%.tex) $(LHS:%=%.lhs) $(DOCTEX)

$(DOC).pdf: $(SRC)
	$(TEX_TO_PDF) $(DOCTEX)

archive: $(SRC)
	tar czf $(ARCHIVE) $(SRC) Makefile

clean:
	rm -f *.nav *.aux *.log *.blg *.bbl *.out

.PHONY: archive
