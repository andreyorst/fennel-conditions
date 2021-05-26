# Fennel Conditions

Common Lisp inspired condition system for Fennel language.

This library implements thread safe resumable exception model for Fennel language.
It is based on the idea of leveraging tables to implement stacks of pseudo-dynamic scopes.

**Note:** This is not a one-to-one port of Common Lisp's condition system, rather an adaptation of the idea in a way that is meaningful for Fennel and Lua runtime.
There is no implementation of `block`, `tagbody`, `go`, `return-from`, and possibly other constructs from Common Lisp, on which condition system is built on.
There's no `restart-bind`, only `restart-case` is implemented.
Macros and function names are mostly the same as in Common Lisp, and run-time semantics should be similar, but the syntax is slightly altered to match Fennel's own syntax.

This library is a work in progress.
All updates up to 0.1.0 can be considered breaking.

## Installation

Clone this repository into your project:

    git clone https://gitlab.com/andreyorst/fennel-conditions.git

## Usage

This library provides it's public API in two files: `init.fnl` and `macros.fnl`.
`init.fnl` module defines functions `error`, `warn`, `signal`, `invoke-restart`, and `make-condition`.
`macros.fnl` module provides `restart-case`, `handler-bind`, `handler-case`, `cerror`, `define-condition`, `ignore-errors`, and `unwind-protect` macros.

Every condition derives from `fennel-conditions/condition` type.
Errors automatically derive from both `fennel-conditions/condition` and `fennel-conditions/error`.
Warnings automatically derive from both `fennel-conditions/condition` and `fennel-conditions/warning`.

Each macro calls internal API functions, which means that `impl/condition-system.fnl` must be included in order to use this library in resulting Lua application.
See [`--require-as-include`](https://fennel-lang.org/reference#include) flag in compiler options.

### Note on the `_G.error` and `pcall`

While this library provides resumable exception model, it is still based on combination of `pcall` and `error`.
Each macro that handles conditional situations wraps its body in `pcall`, and ensures that conditions are handled.
Unhandled conditions bubble up the dynamic scope, and are converted to Lua errors if no parent dynamic scope is found, thus can be caught with `pcall`.
But this style should be avoided, and appropriate handlers should be registered with `handler-case` or `handler-bind`.

This library provides it's own `error` function meant as a replacement to Lua's `error`, but the library itself still internally uses the `_G.error` (i.e. original Lua `error`) function to transfer control.
Therefore it is strongly recommended to avoid overriding `_G.error`.
You should absolutely not set `_G.error` to the `error` function provided by this library, as it will create infinite loops.

## Documentation

Documentation is auto-generated with [Fenneldoc](https://gitlab.com/andreyorst/fenneldoc) and can be found [here](https://gitlab.com/andreyorst/fennel-conditions/-/tree/master/doc).

## Examples

Feel free to read the [Wiki](https://gitlab.com/andreyorst/fennel-conditions/-/wikis/home) for usage examples.

## Contributing

Please do.
You can report issues or feature request at [project's Gitlab repository](https://gitlab.com/andreyorst/fennel-conditions).
Consider reading [contribution guidelines](https://gitlab.com/andreyorst/fennel-conditions/-/blob/master/CONTRIBUTING.md) beforehand.

<!--  LocalWords:  Lua Lua's Gitlab Unhandled
 -->
