#!/bin/bash

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf xxHash
mkdir -p xxHash

(cd $TMP && git clone https://github.com/Cyan4973/xxHash.git)

SRC=$TMP/xxHash

cp -v $SRC/LICENSE xxHash

cp -v $SRC/xxhash.{c,h} xxHash

git add -A .
