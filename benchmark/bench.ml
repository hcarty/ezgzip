let blob = String.init 1_000_000 (fun i -> Char.chr (i mod 256))

let compressed_blob = Ezgzip.compress blob

let () =
  ignore (Benchmark.throughput1 3 ~name:"Ezgzip.compress" Ezgzip.compress blob) ;
  ignore
    (Benchmark.throughput1 3 ~name:"Ezgzip.decompress" Ezgzip.decompress
       compressed_blob) ;
  print_newline ()
