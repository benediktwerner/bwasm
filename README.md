# bwasm

This is basically just a thin wrapper around the [parity-wasm](https://github.com/paritytech/parity-wasm) WebAssembly parser
that provides slightly higher-level information. It mainly combines all the information about functions
into a single struct and combines imports and values defined in the module.

[Documentation](https://docs.rs/bwasm)


## Usage

Add to Cargo.toml

```toml
[dependencies]
bwasm = "0.1"
```

and then

```rust

let module = Module::from_file("some_file.wasm").unwrap();

let func = module.func(0);  // Get function with index 0 (panics if it doesn't exist)

println!("{}", func.name());
println!("{}", func.params().len());
println!("{}", func);
```


# License

`bwasm` is primarily distributed under the terms of both the MIT
license and the Apache License (Version 2.0), at your choice.

See LICENSE-APACHE, and LICENSE-MIT for details.


### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in bwasm by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
