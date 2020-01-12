#!/bin/bash

pushd () {
    command pushd "$@" > /dev/null
}

popd () {
    command popd "$@" > /dev/null
}

# Parameter: the folder 
bindings_lib=$1

if [ "$bindings_lib" == "" ]; then
    echo "Error: missing argument \$bindings_lib (the name one of the folders inside bindings/)"
    exit 1
fi

# Configure the build if it's the first time
if [ ! -d "./dist" ]
then
    runhaskell Setup.hs configure --user
fi

# Build and register the compiled library
runhaskell Setup.hs build
runhaskell Setup.hs install

pushd bindings
    rm -rf $bindings_lib/GI $bindings_lib/dist
    cabal new-run genBuildInfo $bindings_lib
    pushd $bindings_lib
        cabal v1-build
    popd
popd
# cp ./base-ocaml/ocaml/* "./bindings/$bindings_lib/GI/Gtk/Objects/"
cp ./base-ocaml/c/* "./bindings/$bindings_lib/GI/Gtk/Objects/"
cp -r ./base-ocaml/tools "./bindings/$bindings_lib/GI/"