# ocamlJson

A toy JSON parser for Ocaml using dune, ocamlyacc and ocamllex.

### Usage

Json.parse takes a string and returns a JSON value. JSON values are a variant of Number, Bool, String, Null, Array, or Object, covering all possible JSON values.

_this is a toy and shouldn't be used in production._

Not all string escape sequences, unicode escapes or exponent numbers are supported.

### Tests

There are test cases for a lot of simple JSON, but nothing exhaustic.

`$ dune runtest`
