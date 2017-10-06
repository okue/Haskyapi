#! /usr/bin/env sh

cd `dirname $0`
cd src

make
cd ..
mkdir -p bin
cp src/bin/haskyapictl bin/

