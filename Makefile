
include make/build.mk

.PHONY: test
test :
	$(GHC) -O2 -fobject-code -isrc/shimmer -isrc/shimmer-test src/shimmer-test/Main.hs -e tests

.PHONY: bin/shimmer
bin/shimmer :
	$(GHC) -O2 -o bin/shimmer -isrc/shimmer src/shimmer/Main.hs
