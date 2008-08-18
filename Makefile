
all: build

clean:
	runhaskell Setup.hs clean

configure: Setup.hs rdf4h.cabal *.hs
	runhaskell Setup.hs configure --user --prefix=${HOME} \
		--docdir=dist/doc \
		--haddock-options="-v \
		--source-module=http://protempore.net/rdf4h/doc/src/%{MODULE/./-}.html"

build: configure
	runhaskell Setup.hs build

haddock: configure build
	runhaskell Setup.hs haddock --hyperlink-source

install:  configure build
	runhaskell Setup.hs install

# The test function compiles, so no need to depend on configure or compile.
test:
	runhaskell Setup.hs test
