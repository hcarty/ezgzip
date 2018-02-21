# ezgzip - Simple gzip (de)compression library

Documentation is available
[here](https://hcarty.github.io/ezgzip/ezgzip/index.html).

```ocaml
open Rresult

let () =
  let original = "Hello world" in
  let compressed = Ezgzip.compress original in
  let decompressed = R.get_ok (Ezgzip.decompress compressed) in
  assert (original = decompressed)
```

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
