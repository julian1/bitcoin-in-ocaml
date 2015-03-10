


corebuild    -package microecc,cryptokit,zarith,lwt,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax -I src tests/test_runner.byte


# single path
./test_runner.byte -only-test 'suite:1'

important - think it's possible to nest as much as we like

-----

- ok, i think we need the ability to encode a tx, in order to create the tx_copy from the tx_new. 
- alternatively, we have to change the decoder to give us the positions of the script so we can do the 
	edit for signature verification. 


---
what's next?
	- get rid of the strrev and decs  and incorporate into the decs

	- done - decode blocks, 
	- leveldb, or postgres alt interface (consider lwt)
	- merkle tree
	- difficulty extract
	- encode the public key... to show all tx inputs and outputs for pay to script. if bytes length is correct.
		- should change the format tx 

	- microecc secp256k1, to support script engine.

  - we should orientate around actual blocks -  actually test_data
    - for decoder checking etc.
  - hmmn

--

refs

https://en.bitcoin.it/wiki/Protocol_documentation
https://en.bitcoin.it/wiki/Base58Check_encoding
https://en.bitcoin.it/wiki/Script
https://en.bitcoin.it/wiki/Technical_background_of_Bitcoin_addresses
http://bitcoin.stackexchange.com/questions/12554/why-the-signature-is-always-65-13232-bytes-long

https://en.bitcoin.it/wiki/OP_CHECKSIG
https://bitcointalk.org/index.php?topic=29416.0

http://docs.camlcity.org/docs/godipkg/4.00/godi-zarith/doc/godi-zarith/html/Z.html
https://ocaml.janestreet.com/ocaml-core/109.13.00/doc/core/String.html

---
lwt
opam install zarith

eg,

corebuild  -package zarith,sha,lwt,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax address.byte 
---

Consider using Z values instead of Int64 ...

issue how big are ocaml strings - are they large enough to hold everything?

val of_string_base : int -> string -> t
	parses a number in a base - can specify...

Ok, we should be able to load a bin string using only left-shifts ok.
and avoid going through the string.
