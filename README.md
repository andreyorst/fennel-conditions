# Fennel Conditions

Implementation of condition system inspired by Common Lisp.

This library provides several macros that implement resumable exception model for Fennel language.
It is based on the idea of leveraging tables to implement stacks of pseudo-dynamic scopes.

> **Note:** This is not a one to one port of condition system from Common Lisp, rather an adaptation of the idea in a way that is meaningful for Fennel and Lua.
> Macro and function names are the same as in Common Lisp, and run-time semantics should be similar, but the syntax is slightly altered to match Fennel's own syntax.
> There is no implementation of `block`, `tagbody`, `go`, `return-from`, and possibly other constructs from Common Lisp, on which condition system is built on.
> For example there's no `restart-bind`, only `restart-case` is implemented.
> Instead Lua's [protected-call system](https://www.lua.org/pil/8.4.html) is used, and the dynamic scope stack is implemented with plain Lua tables, which also makes it possible to handle Lua errors as conditions.

This library is a work in progress.
Any updates up to 0.1.0 can be considered breaking changes.

## Installation

Clone this repository into your project:

    git clone https://gitlab.com/andreyorst/fennel-conditions.git

## Usage

This library provides it's public API in two files: `init.fnl` and `macros.fnl`.
`init.fnl` module defines functions `error`, `warn`, `signal`, `invoke-restart`, and `make-condition`.
`macros.fnl` module provides `restart-case`, `handler-bind`, `handler-case`, `cerror`, `define-condition`, `ignore-errors`, and `unwind-protect` macros.

Every condition derives from `:fennel-conditions/condition` type.
Errors automatically derive from both `:fennel-conditions/condition` and `:fennel-conditions/error`.
Warnings automatically derive from both `:fennel-conditions/condition` and `:fennel-conditions/warning`.

Each macro calls internal API functions, which means that `impl/condition-system.fnl` must be included in order to use this library in resulting Lua application.
See [`--require-as-include`](https://fennel-lang.org/reference#include) flag in compiler options.

### Note on the `_G.error`

While this library provides it's own `error` function, library itself still internally uses the `_G.error` (i.e. original Lua `error`) function to transfer control.
Therefore it is not recommended to override `_G.error` to something that does not transfer control flow.
You should absolutely not set `_G.error` to the `error` function provided by this library, as it will create infinite loop.

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
