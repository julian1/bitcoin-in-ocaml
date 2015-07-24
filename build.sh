#!/bin/bash -x

# rm -rf ./_build 

corebuild -I src  -package pgocaml,cryptokit,zarith,lwt,lwt.preemptive,lwt.unix,lwt.syntax  test/runner.byte

