# Fennel Conditions

Implementation of condition system inspired by Common Lisp.

This library provides several macros that implement resumable exception model for Fennel language.
It is based on the idea of leveraging tables to implement pseudo-dynamic scope.


## Installation

Clone this repository into your project:

    git clone https://gitlab.com/andreyorst/fennel-conditions.git


## Usage

This library provides it's public API in terms of a set of macros.
Each macro calls internal API functions, which means that `impl/condition-system.fnl` must be compiled in order to use this library in resulting Lua application without Fennel.


## Documentation

Documentation is auto-generated with [Fenneldoc](https://gitlab.com/andreyorst/fenneldoc) and can be found [here](https://gitlab.com/andreyorst/fennel-condition/-/tree/master/doc).


## Examples

Feel free to read [Wiki](https://gitlab.com/andreyorst/fennel-conditions/-/wikis/home) for usage examples.


## Contributing

Please do.
You can report issues or feature request at [project's Gitlab repository](https://gitlab.com/andreyorst/fennel-conditions).
Consider reading [contribution guidelines](https://gitlab.com/andreyorst/fennel-conditions/-/blob/master/CONTRIBUTING.md) beforehand.

<!--  LocalWords:  Lua Lua's Gitlab
 -->
