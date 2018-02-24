
.PHONY: test
test :
	ghc -fobject-code -isrc/shimmer -isrc/shimmer-test src/shimmer-test/Main.hs -e tests