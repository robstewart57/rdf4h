
all: build

clean:
	cabal clean

configure:
	cabal configure

configure-tests:
	cabal configure --enable-tests

build: configure
	cabal build

build-tests: configure-tests
	cabal build

haddock: configure build
	cabal haddock --hyperlink-source

install:
	cabal install

sdist: configure build
	cabal sdist

test: build-tests
	cabal test


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
