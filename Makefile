all: build

.PHONY: all build test

build: dist/setup-config 
	@/bin/echo -e "CABAL\tbuild"
	cabal build

check: test

test: dist/setup-config 
	@/bin/echo -e "CABAL\ttest"
	cabal test

dist/setup-config: napm.cabal 
	cabal configure \
		--enable-tests \
		--disable-benchmarks \
		-v0 2>/dev/null || /bin/echo -e "CABAL\tinstall --only-dependencies" && cabal install --only-dependencies --enable-tests --disable-benchmarks
	@/bin/echo -e "CABAL\tconfigure"
	cabal configure \
		--enable-tests \
		--disable-benchmarks \
		--disable-library-profiling \
		--disable-executable-profiling


format: $(SOURCES)
	stylish-haskell -i $^

clean:
	@/bin/echo -e "CABAL\tclean"
	-cabal clean >/dev/null
	@/bin/echo -e "RM\ttemporary files"
	-rm -f tags
	-rm -f *.prof
	-rm -f lib/Package.hs

doc:
	cabal haddock

install:
	cabal install
