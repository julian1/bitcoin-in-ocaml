#!/bin/bash -x

# rm -rf ./_build 

corebuild -no-links -I src  -package pgocaml,cryptokit,zarith,lwt,lwt.preemptive,lwt.unix,lwt.syntax  test/runner.byte

# ./_build/test/runner.byte

corebuild  -no-links  -I src  -package microecc,pgocaml,cryptokit,zarith,lwt,lwt.preemptive,lwt.unix,lwt.syntax  src/client.byte src/client2.byte src/client3.byte src/client4.byte

