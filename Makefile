
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


#=======================================
# Auxiliary tasks

mwget = wget

test/w3c/fetch:
	mkdir -p data/w3c/turtle && \
	  mkdir -p data/w3c/n3 && \
	  mkdir -p data/w3c/n4 && \
	  mkdir -p data/w3c/rdf-mt && \
	  mkdir -p data/w3c/rdf-xml && \
	  mkdir -p data/w3c/trig ; \
	  cd data/w3c/turtle && \
	    $(mwget) "http://www.w3.org/2013/TurtleTests/TESTS.tar.gz" && \
	    tar -xzf TESTS.tar.gz && rm TESTS.tar.gz ; \
	  cd ../n3 && \
	    $(mwget) "http://www.w3.org/2013/N-TriplesTests/TESTS.tar.gz" && \
	    tar -xzf TESTS.tar.gz && rm TESTS.tar.gz ; \
	  cd ../n4 && \
	    $(mwget) "http://www.w3.org/2013/NQuadsTests/TESTS.tar.gz" && \
	    tar -xzf TESTS.tar.gz && rm TESTS.tar.gz ; \
	  cd ../rdf-mt && \
	    $(mwget) "http://www.w3.org/2013/rdf-mt-tests/TESTS.tar.gz" && \
	    tar -xzf TESTS.tar.gz && rm TESTS.tar.gz ; \
	  cd ../rdf-xml && \
	    $(mwget) "http://www.w3.org/2013/RDFXMLTests/TESTS.tar.gz" && \
	    tar -xzf TESTS.tar.gz && rm TESTS.tar.gz ; \
	  cd ../trig && \
	    $(mwget) "http://www.w3.org/2013/TriGTests/TESTS.tar.gz" && \
	    tar -xzf TESTS.tar.gz && rm TESTS.tar.gz ; \
	  cd ../../..
