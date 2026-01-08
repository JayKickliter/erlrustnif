# erlrustnif

A [rebar3](https://www.rebar3.org/) template for building [Erlang
NIF](https://www.erlang.org/doc/man/erl_nif.html) (Native Implemented
Function) projects with Rust using [Rustler](https://rustler.rs/).

Scaffold Erlang/Rust hybrid projects with a single `rebar3` command.

## Installation

### Option 1: global rebar3 plugin

Add the following to your global `rebar3` configuration at `~/.config/rebar3/rebar.config`:

```erlang
{plugins, [
    {erlrustnif, {git, "https://github.com/JayKickliter/erlang-rust-nif-template.git", {branch, "main"}}}
]}.
```

### Option 2: manual installation

Copy the template directory to your `rebar3` templates location:

```bash
cp -r priv/templates/erlrustnif ~/.config/rebar3/templates/
```

## Creating a new project

```bash
rebar3 new erlrustnif myproject
cd myproject
rebar3 compile
rebar3 eunit
```

## What You Get

Each generated project includes:

- Erlang module with NIF loader and public API
- Rust NIF implementation using Rustler framework
- EUnit tests
- GitHub Actions workflows for macOS and Linux
- Automatic Rust compilation via `rebar3`
- Code quality tools: dialyzer, erlfmt, clippy, rustfmt

## Key Features

Rust crates are named `{{name}}-native` to avoid naming collisions in
workspaces.

Automatically detects the platform (Linux, macOS, Windows) and
generates appropriate library extensions.

Pre-compile hooks automatically build Rust code with `cargo build
--release`, copy the compiled library to `priv/` with `.so` extension,
and load the NIF on module initialization.

The Erlang module automatically discovers the compiled native library
from multiple locations, working both in development and when
installed as a package.

Includes testing, code quality checks, CI/CD pipelines, and
documentation generation.

## Requirements

Generated projects require:

- Erlang/OTP: 27 or later
- Rust: Stable toolchain
- `rebar3`: 3.24.0 or later

## License

Dual licensed under [Apache License 2.0](LICENSE-APACHE) or [MIT
license](LICENSE-MIT) at your option.
