#!/bin/bash -x

# rm -rf ./_build 

# corebuild -no-links -I src  -package pgocaml,cryptokit,zarith,lwt,lwt.preemptive,lwt.unix,lwt.syntax  test/runner.byte || exit

# ./_build/test/runner.byte

# to generate annot file to check tail-recursion 
# corebuild -cflag -annot -no-links  -I src  \

corebuild  -no-links  -I src  \
  -package microecc,pgocaml,cryptokit,zarith,lwt,lwt.preemptive,lwt.unix,lwt.syntax  \
 test/runner.byte  src/client.byte src/client2.byte src/client3.byte src/client4.byte src/client5.byte

