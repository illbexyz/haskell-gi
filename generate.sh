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

# If the bindings haven't 
if [ ! -d "./dist/$bindings_lib/dist" ]
then
    pushd bindings
        rm -rf $bindings_lib/GI $bindings_lib/dist
        cabal new-run genBuildInfo $bindings_lib
        pushd $bindings_lib
            cabal v1-install
        popd
    popd
fi

# pushd bindings/$bindings_lib
#     rm -rf GI dist; cabal v1-build
# popd