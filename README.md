# Fennel Conditions

Common Lisp inspired condition system for Fennel language.

This library implements a thread-safe resumable exception model for the Fennel language and Lua runtime.
It is based on the idea of leveraging tables to implement thread-local dynamic scopes.

## Installation

Clone this repository into your project:

    git clone https://gitlab.com/andreyorst/fennel-conditions.git conditions

## Usage

This library provides it's public API in two files: `init.fnl` and `init-macros.fnl`.
`init.fnl` module defines functions:

- `error`,
- `warn`,
- `signal`,
- `find-restart`,
- `invoke-restart`,
- `invoke-debugger`,
- `continue`,
- `make-condition`.

And exports predefined condition objects `Condition`, `Warning`, and `Error`.

Every condition derives from the `Condition` type.
Errors automatically derive from both `Condition` and `Error`.
Warnings automatically derive from both `Condition` and `Warning`.

`init-macros.fnl` module provides the following macros:

- `restart-case`,
- `handler-bind`,
- `handler-case`,
- `cerror`,
- `define-condition`,
- `ignore-errors`,
- `unwind-protect`.

Each macro calls internal API functions, which means that `impl/condition-system.fnl` must be included in order to use this library in the resulting Lua application.
See [`--require-as-include`](https://fennel-lang.org/reference#include) flag in compiler options.

### Differences from Common Lisp

- This is not a one-to-one port of Common Lisp's condition system, rather an adaptation of the idea in a way that is meaningful for the Fennel language, and the Lua runtime.
- There is no implementation of `block`, `tagbody`, `go`, `return-from`, and possibly other constructs from Common Lisp, on which the condition system is built.
  These constructs may be added in the future, but it is not a high priority, as Fennel doesn't support early returns, and the primary use of this library is error handling.
- There's no `restart-bind`, only `restart-case` is implemented.
- Condition inheritance is simpler, and many default condition types are left out for the user to define, like `simple-error`.
- Macros and function names are mostly the same as in Common Lisp, and runtime semantics should be similar, but the syntax is slightly altered to match Fennel's own syntax.

### Note on the `_G.error` and `pcall`

While this library provides a resumable exception model, it is still internally based on a combination of `pcall` and `error`.
Each macro that handles conditional situations wraps its body in `pcall`, and ensures that conditions are handled.
Unhandled conditions bubble up the dynamic scope stack, and are converted to Lua errors if no parent dynamic scope is found, thus can be caught with `pcall`.
But this style should be avoided, and appropriate handlers should be registered with `handler-case` or `handler-bind`.

This library provides its own `error` function meant as a replacement to Lua's `error`.
The library caches the original `error` function, and uses it internally, and it's not recommended to override `_G.error`.
Since the library reads global value of `error` on initialization, the reloading of the library should be avoided, in case `_G.error` was set to `error` provided by the library.

## Documentation

Documentation is auto-generated with [Fenneldoc](https://gitlab.com/andreyorst/fenneldoc) and can be found [here](https://gitlab.com/andreyorst/fennel-conditions/-/tree/master/doc).

## Examples

Feel free to read the [Wiki](https://gitlab.com/andreyorst/fennel-conditions/-/wikis/home) for usage examples.

## Contributing

Please do.
You can report issues or feature requests at [project's Gitlab repository](https://gitlab.com/andreyorst/fennel-conditions).
Consider reading [contribution guidelines](https://gitlab.com/andreyorst/fennel-conditions/-/blob/master/CONTRIBUTING.md) beforehand.

<!--  LocalWords:  Lua Lua's Gitlab Unhandled
 -->
