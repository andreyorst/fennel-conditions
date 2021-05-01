# Macros.fnl (v0.0.3)
Condition system for Fennel language.

This module provides a set of macros, that implement Common
Lisp-inspired condition system for Fennel language.

Because public API is macros only, it is not possible to use this
library from pure Lua. This library depends on Fennel compiler, but it
is embedded in the internal API, and can be packaged together with the
library.

**Table of contents**

- [`cerror`](#cerror)
- [`define-condition`](#define-condition)
- [`handler-bind`](#handler-bind)
- [`handler-case`](#handler-case)
- [`restart-case`](#restart-case)

## `cerror`
Function signature:

```
(cerror continue-description condition-object ...)
```

Raise `condition-object` as an error with continue restart described by `continue-description`.

Similarly to `error`, [`cerror`](#cerror) raises condition as an error, but
automatically binds the `continue` restart, which can be used either
with the `continue` function in the handler, or in the interactive
debugger.  The `continue-description` is a string, describing what
will happen if `continue` restart is invoked.

### Examples
Convert `x` to positive value if it is negative:

``` fennel
(fn sqrt [x]
  (var x x)
  (when (< x 0)
    (cerror "convert x to positive value" :neg-sqrt x)
    (set x (- x)))
  (math.sqrt x))

(handler-bind [:neg-sqrt (fn [] (continue))]
  (sqrt -4))
```

## `define-condition`
Function signature:

```
(define-condition condition-symbol ...)
```

Create base condition object with `condition-symbol` from which
conditions will be derived with `make-condition`.  Accepts additional
`:parent` and `:name` key value pairs.

### Examples
Creating `error` condition:

``` fennel
(define-condition error)
```

Creating `simple-error` condition with parent set to `error` condition:

``` fennel
(define-condition error)
(define-condition simple-error :parent error)
```

Altering condition's printable name:

``` fennel
(define-condition dbze :name "divide by zero error")
```

## `handler-bind`
Function signature:

```
(handler-bind binding-vec ...)
```

Bind handlers to conditions.

`binding-vec` is a sequential table of conditions and their respecting
handlers followed by the body expression.  Each handler is a function
of at least one argument - the signal being handled.  Other arguments
are optional, and can be used inside the handler.

If body expression signals a condition, a bound handler is invoked.
If no handler were bound for condition, condition is thrown as an
exception.

### Examples
Handlers executed but their return values are not used:

``` fennel
(assert-not
 (handler-bind [:signal-condition (fn [] (print "caught signal condition") 10)
                :error-condition (fn [] (print "caught error condition") 20)]
   (signal :error-condition)))
```

To provide a return value use either [`handler-case`](#handler-case) or [`restart-case`](#restart-case)
and `invoke-restart`.

## `handler-case`
Function signature:

```
(handler-case expr ...)
```

Condition handling.
Accepts expression `expr` and handlers that can be used when handling
conditions raised from within the expression.  Provides the facility
to catch named conditions raised with `signal` or `error` macros.  If
any condition is raised, before propagating condition to error, a
handler is searched.  If handler is bound for this condition, it is
executed, and the result of [`handler-case`](#handler-case) expression will be result
of the handler.

Handlers are lists where first object represents condition, which can
be of any type, and the rest is fn-tail - sequential table of function
arguments, and function body.


### Examples
Handling `error` condition:

``` fennel
(assert-eq 42 (handler-case (error :error-condition)
                (:error-condition [] 42)))
```

## `restart-case`
Function signature:

```
(restart-case expr ...)
```

Resumable condition restart point.
Accepts expression `expr` and restarts that can be used when handling
conditions thrown from within the expression.  Similarly to
[`handler-case`](#handler-case) restarts are lists with condition and fn-tail.

If expression or any of it's subsequent expressions raises a
condition, it will be possible to return into the [`restart-case`](#restart-case), and
execute one of the provided restarts.  Restarts can be executed with
the `invoke-restart` macro only from handlers bound with
[`handler-bind`](#handler-bind).  Restart names are always strings.

### Examples
Specifying two restarts for `:signal-condition`:

``` fennel
(restart-case (signal :signal-condition)
  (:some-restart [] :body)
  (:some-other-restart [] :body))
```


---

Copyright (C) 2021 Andrey Listopadov

License: [MIT](https://gitlab.com/andreyorst/fennel-conditions/-/raw/master/LICENSE)


<!-- Generated with Fenneldoc v0.1.5
     https://gitlab.com/andreyorst/fenneldoc -->
