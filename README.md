# config

[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/filipegoncalves/rust-config?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge) [![Build Status](https://travis-ci.org/filipegoncalves/rust-config.svg?branch=master)](https://travis-ci.org/filipegoncalves/rust-config) [![Crates.io](https://img.shields.io/crates/v/config.svg)](https://crates.io/crates/config)

# Description
A Rust library to read and parse configuration files.

The idea is to make it very similar to [libconfig](http://www.hyperrealm.com/libconfig/), with a few extra additions / tweaks.

This is still under heavy development. As of this writing, the library is still very basic and can only read / load a configuration. It also includes a rudimentary set of methods to browse the loaded data.

# Supported rust versions

As of 0.1.0, the library is compatible with both the nightly and the beta channels.

# Installation
`config` is on [crates.io](https://crates.io/crates/config). It can be included in a project using Cargo by adding this to `Cargo.toml`:

```toml
[dependencies]
config = "~0.1.0"
```

# Getting started
Updated documentation can be found [here](http://filipegoncalves.github.io/rust-config/config/). The documentation includes small sample use cases, and a full specification of the input format.

Another good example can be found in the integration tests directory (`tests/`).

# TODO

## Features
- [X] Allow single and multi-line comments in configurations
- [ ] Add `#include` support to include other configuration files
- [X] Automatically concatenate blank-separated string literals in the configuration. Useful for settings with big strings
- [ ] Export a public API to manipulate a configuration in runtime and possibly write it to a file
- [ ] Add more escape sequences possibilities in string literals (make it similar to what Rust supports)

## Parser
- [X] Figure out why the parser returns an error on a blanks-only configuration
- [X] Use [nom](https://github.com/Geal/nom) to generate the parser instead of rust-peg. Rust-peg uses features that will not be available in beta.

## Misc
- [X] Refactor misc types (`Setting`, `SettingsList`, etc) into a separate, independent module
- [X] Write tests that are expected to fail
- [X] Add missing documentation for undocumented code
- [X] Document `parser::ParseErr`
- [X] Write misc documentation with a high level description of the module and its features
- [X] Write integration tests
- [X] Document when, why and how `parse()` returns `Err`
- [ ] `hex` and `hex64` literals support?
- [ ] Add option to indicate the conf. file encoding
- [X] Enforce the rules for arrays. Arrays are homogeneous and can only hold scalar values

# Contributing
Contributions will be greatly appreciated. Feel free to reach out on Gitter or IRC. I'm Fill on irc.mozilla.org. You can find me on #rust and #rust-config.

I am relatively new to Rust, and as such, there is probably a lot of room for improvement on the library design and code quality. I started this project to learn the language. So, feel free to fix anything that you think may be wrong.
