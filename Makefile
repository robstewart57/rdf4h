
all: haddock

clean:
	runhaskell Setup.hs clean

configure: Setup.hs rdf4h.cabal *.hs
	runhaskell Setup.hs configure --user --prefix=${HOME}
#          --haddock-args="--source-base=http://protempore.net/rdf4h/ \
#          --source-module=http://protempore.net/rdf4h/%M.hs \
#          --read-interface=http://www.haskell.org/ghc/docs/latest/html/libraries/base,/usr/share/doc/ghc-6.6.1/html/libraries/base/base.haddock"

build: configure
	runhaskell Setup.hs build

haddock: configure build
	runhaskell Setup.hs haddock
#	find dist/doc/html -name '*.html' -exec \
#		sed -i -r 's_/usr/share/doc/ghc-[^/]+/html/libraries/_http://www.haskell.org/ghc/docs/latest/html/libraries/_g' {} \;

install:  configure build
	runhaskell Setup.hs install
