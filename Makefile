
include make/build.mk

.PHONY: bin/shimmer
bin/shimmer :
	@$(GHC) -XStrict -XBangPatterns -XPatternSynonyms -XOverloadedStrings \
		-XScopedTypeVariables -XDeriveAnyClass -XParallelListComp \
		-XPatternGuards \
		-O2 -o bin/shimmer -isrc/shimmer src/shimmer/Main.hs \
		-package haskeline

.PHONY: test
test :
	@$(GHC) -O2 -fobject-code -isrc/shimmer -isrc/shimmer-test src/shimmer-test/Main.hs -e tests
