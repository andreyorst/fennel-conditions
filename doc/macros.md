# Macros.fnl (v0.1.0-rc3)
Condition system for Fennel language.

This module provides a set of macros, that implement Common
Lisp-inspired condition system for Fennel language.

**Table of contents**

- [`cerror`](#cerror)
- [`handler-case`](#handler-case)
- [`handler-bind`](#handler-bind)
- [`restart-case`](#restart-case)
- [`define-condition`](#define-condition)
- [`ignore-errors`](#ignore-errors)
- [`unwind-protect`](#unwind-protect)

## `cerror`
Function signature:

```
(cerror continue-description condition-object ...)
```

Raise `condition-object` as an error with auto-bound continue restart, described by `continue-description`.

Similarly to `error`, [`cerror`](#cerror) raises condition as an error, but
automatically binds the `continue` restart, which can be used either
with the `continue` function in the handler, or in the interactive
debugger.  The `continue-description` must be a string, describing
what will happen if `continue` restart is invoked.

### Examples

Convert `x` to positive value if it is negative:

``` fennel
(fn sqrt [x]
  (var x x)
  (when (< x 0)
    (cerror "convert x to positive value" :neg-sqrt)
    (set x (- x)))
  (math.sqrt x))

(handler-bind [:neg-sqrt (fn [] (continue))]
  (assert-eq 2 (sqrt -4)))
```

## `handler-case`
Function signature:

```
(handler-case expr ...)
```

Condition handling similar to try/catch.

Accepts expression `expr` and handlers that can be used to handle
conditions, raised from within the expression or any subsequent
expressions.  Provides the facility to catch named conditions raised
with `signal`, `warn` and `error` functions.  If any condition is
raised, before propagating condition to error, a handler is searched.
If handler is bound for this condition, it is executed, and the result
of [`handler-case`](#handler-case) expression will be the result of the handler.

Handlers are defined as lists, where the first object represents the
condition to handle, and the rest is fn-tail - a sequential table of
function arguments, followed by the function body.

### Examples

Handling `error` condition:

``` fennel
(assert-eq 42 (handler-case (error :error-condition)
                (:error-condition [] 42)))
```

## `handler-bind`
Function signature:

```
(handler-bind binding-vec ...)
```

Bind handlers to conditions.

`binding-vec` is a sequential table of conditions and their respecting
handlers followed by the body expression.  Each handler should be a
function of at least one argument - the condition being handled.
Other arguments are optional, and can be used inside the handler.

If body expression raises a condition, and a handler is bound for this
condition, the handler is invoked.  If no handler were bound for
condition, handlers are searched up the dynamic scope. If no handler
found, condition is thrown as a Lua error.

If invoked handler exits normally, a condition is re-raised.  To
prevent re-raising, use `invoke-restart` function.

### Examples

Handlers executed but their return values are not used:

``` fennel
(var res nil)

(assert-not
 (handler-bind [:signal-condition (fn [] (set res "signal") 10)
                :error-condition (fn [] (set res "error") 20)]
   (signal :error-condition)))

(assert-eq res :error)
```

To provide a return value use either [`handler-case`](#handler-case) or a combination
of [`restart-case`](#restart-case) and `invoke-restart`.

## `restart-case`
Function signature:

```
(restart-case expr ...)
```

Resumable condition restart point.

Accepts expression `expr` and restarts that can be used when handling
conditions thrown from within the expression.  Similarly to
[`handler-case`](#handler-case) restarts are lists with restart name, and an fn-tail.

If expression or any of it's subsequent expressions raises a
condition, it will be possible to return into the [`restart-case`](#restart-case), and
execute one of the provided restarts.  Restarts can be executed with
the `invoke-restart` function from handlers bound with [`handler-bind`](#handler-bind).
Restart names are always strings.

### Examples

Specifying two restarts for `:signal-condition`:

``` fennel
(restart-case (signal :signal-condition)
  (:some-restart [] :body)
  (:some-other-restart [] :other-body))
```

## `define-condition`
Function signature:

```
(define-condition condition-symbol ...)
```

Create base condition object with `condition-symbol` from which
conditions will be derived with `make-condition`.  Accepts additional
`:parent` and `:name` key value pairs.  If no `:name` specified, uses
`condition-symbol`'s `tostring` representation.  If no `:parent` given
uses `Condition` object as a parent.

### Examples

Altering condition's printable name:

``` fennel
(define-condition dbz :name :divide-by-zero)
```

Creating `math-error` condition with parent set to `Error` condition,
and `divide-by-zero` condition with parent set to `math-error`, and handling it:

``` fennel
(define-condition math-error :parent Error)
(define-condition divide-by-zero :parent math-error)

(assert-eq :ok (handler-case (error divide-by-zero)
                 (math-error [] :ok)))
```

## `ignore-errors`
Function signature:

```
(ignore-errors ...)
```

Ignore all conditions of type error.  If error condition was raised,
returns nil and condition as values.  If no error conditions were
raised, returns the resulting values normally.  Lua errors can be
handled with this macro.

### Examples

Condition of type error is ignored:

``` fennel
(local result [])

(local (res condition)
  (ignore-errors
    (table.insert result 1)
    (table.insert result 2)
    (error :some-error)
    (table.insert result 3)))

(assert-not res)
(assert-eq :some-error condition)
(assert-eq [1 2] result)
```

## `unwind-protect`
Function signature:

```
(unwind-protect expr ...)
```

Runs `expr` in protected call, and runs all other forms as cleanup
forms before returning value, whether `expr` returned normally or
error occurred.  Similar to try/finally without a catch.

### Examples

``` fennel
(local result [])

(ignore-errors
  (unwind-protect
      (/ 1 nil)
    (table.insert result 1)
    (table.insert result 2)))

(assert-eq [1 2] result)
```


---

Copyright (C) 2021 Andrey Listopadov

License: [MIT](https://gitlab.com/andreyorst/fennel-conditions/-/raw/master/LICENSE)


<!-- Generated with Fenneldoc v0.1.5
     https://gitlab.com/andreyorst/fenneldoc -->
