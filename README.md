# ezgzip - Simple gzip (de)compression library

![CI checks](https://github.com/hcarty/ezgzip/workflows/CI%20checks/badge.svg)

ezgzip is a simple interface focused on `string -> string` zlib and gzip
(de)compression.

Documentation is available
[here](https://hcarty.github.io/ezgzip/ezgzip/index.html).

An example illustrating how to gzip compress and then decompress a string:
```ocaml
open Rresult

let () =
  let original = "Hello world" in
  let compressed = Ezgzip.compress original in
  let decompressed = R.get_ok (Ezgzip.decompress compressed) in
  assert (original = decompressed)
```

This library currently uses the zlib bindings provided by
[camlzip](https://github.com/xavierleroy/camlzip).  The gzip header/footer code
is based on the
[upstream specification](http://www.gzip.org/zlib/rfc-gzip.html#specification).

## Compile

```
make
```

## Load an interactive environment for testing

```
make repl
```

The library will be available from this environment.

## Run tests

```
make test
```

## Run benchmarks

```
make benchmark
```

## Build documentation

```
make doc
```

## Upload documentation to github pages

*NOTE:* This requires push permissions to the repository...

```
make gh-pages
```
