#!/usr/bin/env bash
# benchmark boilerplate shamelessly taken from [hdgarrood/purescript-sequences](https://github.com/hdgarrood/purescript-sequences/blob/master/benchmarks/run.sh)

set -e

# cheat to put Benchmark.Main in the pulp source path
rm -rf src/Benchmark
mkdir -p src/Benchmark
ln -s ../../benchmark/Main.purs src/Benchmark/Main.purs
ln -s ../../benchmark/Main.js src/Benchmark/Main.js

pulp build

mkdir -p tmp
pulp build --main Benchmark.Main --to tmp/benchmark.js
node tmp/benchmark.js
