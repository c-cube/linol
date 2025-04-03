Uutf — Non-blocking streaming Unicode codec for OCaml
=====================================================

**Warning.** You are encouraged not to use this library.

- As of OCaml 4.14, both UTF encoding and decoding are available
  in the standard library, see the `String` and `Buffer` modules.
- If you are looking for a stream abstraction compatible with
  effect based concurrency look into [`bytesrw`] package.

---

Uutf is a non-blocking streaming codec to decode and encode the UTF-8,
UTF-16, UTF-16LE and UTF-16BE encoding schemes. It can efficiently
work character by character without blocking on IO. Decoders perform
character position tracking and support newline normalization.

Functions are also provided to fold over the characters of UTF encoded
OCaml string values and to directly encode characters in OCaml
Buffer.t values.

Uutf has no dependency and is distributed under the ISC license.

Home page: <http://erratique.ch/software/uutf>

[`bytesrw`]: https://erratique.ch/software/bytesrw


## Installation

Uutf can be installed with `opam`:

    opam install uutf

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation can be consulted [online] or via `odig doc uutf`.

Questions are welcome but better asked on the [OCaml forum] than on the
issue tracker.

[online]: http://erratique.ch/software/uutf/doc/
[OCaml forum]: https://discuss.ocaml.org/
