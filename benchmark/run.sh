#!/usr/bin/env bash
# benchmark boilerplate shamelessly taken from [hdgarrood/purescript-sequences](https://github.com/hdgarrood/purescript-sequences/blob/master/benchmarks/run.sh)

set -e

# cheat to put Benchmark.Main in the pulp source path
rm -rf src/tmp
mkdir -p src/tmp
ln -s ../../benchmark/Main.purs src/tmp/Benchmark.Main.purs
ln -s ../../benchmark/Main.js src/tmp/Benchmark.Main.js

pulp build

mkdir -p tmp
NODE_PATH=./output browserify benchmark/run.js --node -o tmp/benchmarks.js
node tmp/benchmarks.js
