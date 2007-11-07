#!/bin/sh

make clean
make build

dist/build/ttlparse/ttlparse http://www.w3.org/2001/sw/DataAccess/df1/tests/ http://www.w3.org/2001/sw/DataAccess/df1/tests/test-00.ttl data/ttl/conformance/test-00.ttl

