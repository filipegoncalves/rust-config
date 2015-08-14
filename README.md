# config

[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/filipegoncalves/rust-config?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge) [![Build Status](https://travis-ci.org/filipegoncalves/rust-config.svg?branch=master)](https://travis-ci.org/filipegoncalves/rust-config) [![Crates.io](https://img.shields.io/crates/v/config.svg)](https://crates.io/crates/config)

# Description
A Rust library to read and parse configuration files.

The idea is to make it very similar to [libconfig](http://www.hyperrealm.com/libconfig/), with a few extra additions / tweaks.

This is still under heavy development. As of this writing, the library is still very basic and can only read / load a configuration. It also includes a rudimentary set of methods to browse the loaded data.

# Supported rust versions

As of 0.1.0, the library is compatible with both nightly and beta channels.

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
- [ ] Add `#include` support to include other configuration files
- [ ] Export a public API to manipulate a configuration in runtime and possibly write it to a file
- [ ] Add more escape sequences possibilities in string literals (make it similar to what Rust supports)

## Misc
- [ ] `hex` and `hex64` literals support?
- [ ] Add option to indicate the conf. file encoding

# Contributing
Contributions will be greatly appreciated. Feel free to reach out on Gitter or IRC. I'm Fill on irc.mozilla.org. You can find me on #rust and #rust-config.

I am relatively new to Rust, and as such, there is probably a lot of room for improvement on the library design and code quality. I started this project to learn the language. So, feel free to fix anything that you think may be wrong.
