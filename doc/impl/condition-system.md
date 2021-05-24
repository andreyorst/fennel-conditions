# Condition-system.fnl (v0.0.11-dev)
Condition system for Fennel language.

This module is library's private API that provides functions meant for
internal use only.  For public API docs see
[fennel-conditions.md](../fennel-conditions.md) and
[macros.md](../macros.md)

**Table of contents**

- [`dynamic-scope`](#dynamic-scope)
- [`raise`](#raise)
- [`handle`](#handle)
- [`invoke-restart`](#invoke-restart)
- [`compose-error-message`](#compose-error-message)
- [`find-handler`](#find-handler)
- [`invoke-debugger`](#invoke-debugger)
- [`pack`](#pack)
- [`unpack`](#unpack)

## `dynamic-scope`
Dynamic scope for the condition system.

Dynamic scope is a maintained table where handlers and restarts are
stored thread-locally.  Thread name is obtained with
`coroutine.running` call and each thread holds a table with the
following keys `:handlers`, `:restarts`, and `:current-context`.
Handlers and restarts itselves are tables.

## `raise`
Function signature:

```
(raise condition-type condition-object)
```

Raises `condition-object` as a condition of `condition-type`.
`condition-object` must not be `nil`.

## `handle`
Function signature:

```
(handle condition-object type* ?scope)
```

Handle the `condition-object` of `type*` and optional `?scope`.

Finds the `condition-object` handler in the dynamic scope.  If found,
calls the handler, and returns a table with `:state` set to
`:handled`, and `:handler` bound to an anonymous function that calls
the restart.

## `invoke-restart`
Function signature:

```
(invoke-restart restart-name ...)
```

Searches for `restart-name` in the dynamic scope and invokes the
restart with given arguments.  Always throws error, as
[`invoke-restart`](#invoke-restart) must transfer control flow out of the handler.  If
restart is found, calls the restart function and returns a table with
`:state` set to `:restarted`, and `:restart` bound to the restart
function.

## `compose-error-message`
Function signature:

```
(compose-error-message condition-object)
```

Composes message for `condition-object` based on it's name and data
stored within the object.

### Examples
Conditions without data produce short messages:

``` fennel
(define-condition simple-error)
(assert-eq
 "condition simple-error was raised"
 (compose-error-message simple-error))
```

Conditions with data produce extended messages:

``` fennel
(define-condition simple-error)
(assert-eq
 "condition simple-error was raised with the following arguments: 1, 2, 3"
 (compose-error-message (make-condition simple-error 1 2 3)))
```

## `find-handler`
Function signature:

```
(find-handler condition-object type* scope)
```

Finds the `condition-object` handler of `type*` in dynamic scope
`scope`.  If `condition-object` is a table with `type` key equal to
`:condition` searches handler based on condition object's inheritance.
If anything else, searches handler by object reference.

## `invoke-debugger`
Function signature:

```
(invoke-debugger condition-object level)
```

Invokes interactive debugger for given `condition-object`.  Accepts
optional `level`, indicating current debugger depth.

Restarts in the menu are ordered by their definition order and dynamic
scope depth.  Restarts can be called by their number in the menu or
the name in square brackets.  For example, if `restart-case` defines
two restarts `:a` and `:b` and outer `restart-case` bounds restarts
`:a` and `:c` the following menu will be printed:

```
Debugger was invoked on unhandled condition:
1: [a    ] a
2: [b    ] b
3:         a
4: [c    ] c
5: [throw] Throw condition as a Lua error
debugger>>
```

If restart function has a docstring, it is printed after the square
brackets.  If no docstring is found, restart name is printed.

If restart accepts any arguments, a second prompt will be entered when
restart is chosen from the menu.  Above the prompt a hint with
argument names will be displayed.  In this prompt arguments to the
restart are provided as expressions separated by space:

```
Provide inputs for some-restart (args: [some-arg other-arg]) (^D to cancel)
debugger:some-restart>> (+ 1 2 3) {:some :table}
```

Debugger doesn't know anything about the environment, or variables, so
in this prompt only fully realized values can be used.

If an error happens during restart call, debug level increases, and new
`cancel` restart is added to the menu, that allows returning to
previous debug level.

## `pack`
Portable `table.pack` implementation.

## `unpack`
Function signature:

```
(unpack tbl)
```

Automatically try to query `tbl` for it's size `n` and unpack whole
thing.


---

Copyright (C) 2021 Andrey Listopadov

License: [MIT](https://gitlab.com/andreyorst/fennel-conditions/-/raw/master/LICENSE)


<!-- Generated with Fenneldoc v0.1.5
     https://gitlab.com/andreyorst/fenneldoc -->
