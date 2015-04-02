# config [![Build Status](https://travis-ci.org/filipegoncalves/rust-config.svg?branch=master)](https://travis-ci.org/filipegoncalves/rust-config)

A soon-to-be Rust library to read and parse configuration files.

The idea is to make it very similar to [libconfig](http://www.hyperrealm.com/libconfig/), with a few extra additions / tweaks.

This is still under heavy development and should be considered **unstable**. I will publish a first version on [crates](https://crates.io)
as soon as the public API is designed and mostly implemented.

# TODO

## Features
- [ ] Allow single and multi-line comments in configurations
- [ ] Add `#include` support to include other configuration files. Not trivial because of the way the parser works
- [ ] Automatically concatenate blank-separated string literals in the configuration. Useful for settings with big strings

## Parser
- [ ] Fix an annoying bug where the parser won't accept `1E6` as a valid `Floating32` value
- [ ] Figure out why the parser returns an error on a blanks-only configuration
- [ ] Consider splitting the parser into lexer + syntax analyser (much like we would in C with flex + byacc)

## Misc
- [ ] Refactor misc types (`Setting`, `SettingsList`, etc) into a separate, independent module.
- [ ] Write tests that are expected to fail
- [ ] Add missing documentation for undocumented code
- [ ] Document `parser::ParseErr`
- [ ] Write misc documentation with a high level description of the module and its features
- [ ] Write integration tests
- [ ] Document when, why and how `parse()` returns `Err`
- [ ] `hex` and `hex64` literals support
- [ ] Add option to indicate the conf. file encoding
