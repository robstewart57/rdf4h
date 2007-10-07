#!/bin/sh

if [ "$(pwd)" != "/home/calvins/rdf4h" ]
then
    echo "Must run from rdf4h directory."
    exit 1
fi
ln -sf ./www/index.xhtml
ln -sf ./dist/doc/html/ doc
pushd www &> /dev/null
ln -sf ../dist/rdf4h*.tar.gz 
popd &> /dev/null
