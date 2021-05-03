# Fennel Conditions

Implementation of condition system inspired by Common Lisp.

This library provides several macros that implement resumable exception model for Fennel language.
It is based on the idea of leveraging tables to implement pseudo-dynamic scope.

This library is a work in progress.
Any updates up to 0.1.0 can be considered breaking changes.

## Installation

Clone this repository into your project:

    git clone https://gitlab.com/andreyorst/fennel-conditions.git

## Usage

This library provides it's public API in two files: `init.fnl` and `macros.fnl`.
`init.fnl` module defines functions `error`, `warn`, `signal`, `invoke-restart`, and `make-condition`.
`macros.fnl` module provides `restart-case`, `handler-bind`, `handler-case`, `cerror`, `define-condition`, `ignore-errors` macros.

Every condition derives from `:fennel-conditions/condition` type.
Errors derive from both `:fennel-conditions/condition` and `:fennel-conditions/error`.
Warnings derive from both `:fennel-conditions/condition` and `:fennel-conditions/warning`.

Each macro calls internal API functions, which means that `impl/condition-system.fnl` must be included in order to use this library in resulting Lua application.
See [`--require-as-include`](https://fennel-lang.org/reference#include) flag in compiler options.

## Documentation

Documentation is auto-generated with [Fenneldoc](https://gitlab.com/andreyorst/fenneldoc) and can be found [here](https://gitlab.com/andreyorst/fennel-conditions/-/tree/master/doc).

## Examples

Feel free to read [Wiki](https://gitlab.com/andreyorst/fennel-conditions/-/wikis/home) for usage examples.

## Contributing

Please do.
You can report issues or feature request at [project's Gitlab repository](https://gitlab.com/andreyorst/fennel-conditions).
Consider reading [contribution guidelines](https://gitlab.com/andreyorst/fennel-conditions/-/blob/master/CONTRIBUTING.md) beforehand.

<!--  LocalWords:  Lua Lua's Gitlab
 -->
