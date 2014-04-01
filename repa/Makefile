GHC_OPTIONS=-threaded \
            -funfolding-use-threshold1000 \
            -funfolding-keeness-factor1000 \
						-eventlog

HC=ghc $(GHC_OPTIONS)

%.o: %.hs
	$(HC) $<

benches/% : benches/%.hs Scans.o
	$(HC) --make $<

