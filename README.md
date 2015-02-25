
refs

https://en.bitcoin.it/wiki/Protocol_documentation
https://en.bitcoin.it/wiki/Base58Check_encoding
https://en.bitcoin.it/wiki/Script

---
lwt
opam install zarith

eg,

corebuild  -package zarith,sha,lwt,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax address.byte 

