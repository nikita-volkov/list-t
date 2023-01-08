all: format test

test:
	cabal test -j +RTS -A128m -n2m -N -RTS -fwarn-incomplete-patterns --builddir dist/htf-test

docs:
	cabal haddock --enable-documentation --builddir dist/docs

format:
	path=list-t.cabal && cabal-fmt -c --indent 2 $$path || cabal-fmt -i $$path
	ormolu --mode inplace -c $$(find . -name "*.hs" -path "./library/*" && find . -name "*.hs" -path "./tests/*")

clean:
	rm -rf dist
