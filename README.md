
refs

https://en.bitcoin.it/wiki/Protocol_documentation
https://en.bitcoin.it/wiki/Base58Check_encoding
https://en.bitcoin.it/wiki/Script
https://en.bitcoin.it/wiki/Technical_background_of_Bitcoin_addresses

http://docs.camlcity.org/docs/godipkg/4.00/godi-zarith/doc/godi-zarith/html/Z.html
https://ocaml.janestreet.com/ocaml-core/109.13.00/doc/core/String.html

---
<<<<<<< HEAD
lwt  (for io)
opam install zarith  (for base58 rem/div)
opam install cryptokit  (for sha256, ripemd-160 )
=======
lwt
opam install zarith
>>>>>>> devel

eg,

corebuild  -package zarith,sha,lwt,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax address.byte 
---

<<<<<<< HEAD

=======
Consider using Z values instead of Int64 ...

issue how big are ocaml strings - are they large enough to hold everything?

val of_string_base : int -> string -> t
	parses a number in a base - can specify...

Ok, we should be able to load a bin string using only left-shifts ok.
and avoid going through the string.
>>>>>>> devel
