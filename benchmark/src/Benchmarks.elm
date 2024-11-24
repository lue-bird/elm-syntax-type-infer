module Benchmarks exposing (benchmarks)

import Benchmark
import Benchmark.Alternative


benchmarks : Benchmark.Benchmark
benchmarks =
    Benchmark.describe "elm-syntax-type-infer"
        []
