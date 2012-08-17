
all: build

clean:
	runhaskell Setup clean

configure: Setup.hs rdf4h.cabal *.hs
	runhaskell Setup configure -fno-warn-unused-do-bind --user --prefix=${HOME} \
		--docdir=dist/doc \
		--haddock-options="-v \
		--source-module=http://protempore.net/rdf4h/doc/src/%{MODULE/./-}.html" \
	    --enable-tests

build: configure
	runhaskell Setup build

haddock: configure build
	runhaskell Setup haddock --hyperlink-source

install:  configure build
	runhaskell Setup install

sdist :: configure build
	runhaskell Setup sdist
# The test function compiles, so no need to depend on configure or compile.
test:
	runhaskell Setup test
