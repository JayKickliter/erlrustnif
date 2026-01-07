# {{name}}

An Erlang NIF (Native Implemented Function) project with Rust using Rustler.

This project combines Erlang/OTP with Rust for high-performance native functions.

## Requirements

Before building, ensure you have:

- **Erlang/OTP**: 27 or later
  ```bash
  erl -eval 'erlang:display(erlang:system_info(otp_release))' -noshell
  ```

- **Rust**: Stable toolchain
  ```bash
  rustc --version
  cargo --version
  ```

- **rebar3**: 3.24.0 or later
  ```bash
  rebar3 --version
  ```

### Installation Commands (macOS with Homebrew)

```bash
# Install Erlang
brew install erlang

# Install Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Install rebar3
brew install rebar3
```

### Installation Commands (Ubuntu/Debian)

```bash
# Install Erlang
sudo apt-get install erlang

# Install Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Install rebar3
sudo apt-get install rebar3
```

## Building

### Compile the Project

```bash
rebar3 compile
```

This will:
1. Build the Rust code in `native/` using Cargo
2. Copy the compiled native library to `priv/`
3. Compile the Erlang code

### Run Tests

```bash
rebar3 eunit
```

### Run All Checks

```bash
make ci
```

This runs:
- Dialyzer (Erlang static type checker)
- EUnit tests
- Code formatting checks (erlfmt)
- Documentation generation (edoc)
- Rust linting (clippy)
- Rust formatting checks (rustfmt)

### Format Code

```bash
make format
```

Formats both Erlang and Rust code.

### Clean Build Artifacts

```bash
make clean
```


## Adding NIFs

### 1. Implement in Rust (`native/lib.rs`)

```rust
#[rustler::nif]
fn add(a: i64, b: i64) -> i64 {
    a + b
}
```

### 2. Add Erlang Stub (`src/{{name}}.erl`)

```erlang
-spec add(integer(), integer()) -> integer().
add(A, B) ->
    add_nif(A, B).

-spec add_nif(integer(), integer()) -> integer().
add_nif(_, _) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).
```

### 3. Add Test (`test/{{name}}_test.erl`)

```erlang
add_test() ->
    ?assertEqual(7, {{name}}:add(3, 4)),
    ok.
```

### 4. Build and Test

```bash
rebar3 compile
rebar3 eunit
```

## Adding Rust Dependencies

Edit `native/Cargo.toml` and add your dependencies:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
```

Then rebuild with `rebar3 compile`.

## Debugging

### View Erlang Shell with Loaded NIFs

```bash
rebar3 shell
```

Then in the shell:
```erlang
1> {{name}}:hello().
ok
```

### Rust Build Logs

```bash
rebar3 compile -v
```

### Check NIF Loading Errors

If NIFs fail to load, check:
1. Library name matches: `priv/lib{{name}}-native.so`
2. Platform-specific issues (dylib vs so vs dll)
3. Missing dependencies in Rust code
4. Erlang version compatibility

## Performance Considerations

- NIFs run in the Erlang VM's scheduler
- Keep NIF execution time short (milliseconds)
- Use `enif_threaded_nif` for long-running operations
- Avoid memory allocations in hot paths

## CI/CD

This project includes GitHub Actions workflows that:
- Build on macOS and Linux
- Run tests and static analysis
- Check code formatting
- Generate documentation
- Run Rust linting

See `.github/workflows/ci.yml` for configuration.

## License

Dual licensed under Apache License 2.0 or MIT license at your option.

## Resources

- [Rustler Documentation](https://rustler.rs/)
- [Erlang NIF Manual](https://www.erlang.org/doc/man/erl_nif.html)
- [rebar3 Documentation](https://www.rebar3.org/)
- [Cargo Documentation](https://doc.rust-lang.org/cargo/)
