#!/bin/sh

pushd $(dirname $0) $>/dev/null
ln -sf www/index.xhtml
ln -sf dist/doc/html doc
ln -sf dist/rdf4h*.tar.gz
popd &>/dev/null
