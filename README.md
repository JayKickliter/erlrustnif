# Erlang NIF Template

A reusable template for building Erlang NIF (Native Implemented Function) projects using Rust and Rustler.

This template provides a complete, production-ready scaffolding with minimal boilerplate code that compiles out of the box.

## Using This Template

This template uses `erlrustniftemplate` as a placeholder name. To use it for your project:

1. Clone or copy this repository to your new project location
2. Find and replace all occurrences of `erlrustniftemplate` with your actual project name:
   - In file names: `src/erlrustniftemplate.erl` → `src/yourname.erl`
   - In file names: `src/erlrustniftemplate.app.src` → `src/yourname.app.src`
   - In file names: `test/erlrustniftemplate_test.erl` → `test/yourname_test.erl`
   - In file contents: All references to `erlrustniftemplate` in code
3. Add your Rust dependencies to `Cargo.toml`
4. Implement your NIFs in `native/nifs.rs` (or split across multiple files)
5. Write your Erlang API in `src/yourname.erl`
6. Add tests in `test/`

## Building

### Requirements

- Erlang/OTP 27+
- Rust (stable toolchain)
- rebar3

### Commands

```bash
# Build the project
rebar3 compile

# Run tests
rebar3 eunit

# Format code
make format

# Run all checks (dialyzer, tests, format, docs, clippy)
make ci

# Clean build artifacts
make clean
```

## Structure

- `native/` - Rust/Rustler NIF implementation
- `src/` - Erlang application and modules
- `test/` - EUnit tests
- `priv/` - Compiled shared library (generated)
- `.cargo/` - Cargo build configuration (platform-specific flags)
- `.github/workflows/` - CI/CD configuration

## Development

The template includes:

- Rust: Minimal NIF that returns `:ok` to verify compilation
- Erlang: NIF loader that handles `.so` discovery and loading
- Tests: Simple EUnit test to verify the NIF loads correctly
- CI: GitHub Actions workflows for Linux and macOS

## Important Notes

- The `rustler::init!()` macro in `native/lib.rs` must match your Erlang module name
- The NIF loader in the Erlang module automatically finds the compiled `.so` file in `priv/`
- The build process uses cargo pre-hooks in `rebar.config` to integrate Rust compilation

## License

Apache-2.0
