# Conditions.fnl
Condition system for Fennel language.

This module provides a set of macros, that implement Common
Lisp-inspired condition system for Fennel language.

Because public API is macros only, it is not possible to use this
library from pure Lua. This library depends on Fennel compiler, but it
is embedded in the internal API, and can be packaged together with the
library.

**Table of contents**

- [`error`](#error)
- [`warn`](#warn)
- [`signal`](#signal)
- [`handler-case`](#handler-case)
- [`handler-bind`](#handler-bind)
- [`restart-case`](#restart-case)
- [`invoke-restart`](#invoke-restart)

## `error`
Function signature:

```
(error condition-name ...)
```

Raise `condition-name` as an error.
Errors are thrown as Lua errors when no handler is bound.

### Examples
Error is thrown if not handled:

``` fennel
(assert-not (pcall #(error :error-condition)))
```

Errors can be handled with [`handler-case`](#handler-case):

``` fennel
(assert-eq 42 (handler-case (warn :signal-condition 42)
                (:signal-condition [_ x] x)))
```

Errors, signal, and warnings can be recovered with [`handler-bind`](#handler-bind) and
[`restart-case`](#restart-case) by using [`invoke-restart`](#invoke-restart):

``` fennel
(assert-eq 42 (handler-bind [:error-condition
                             (fn [_ x]
                               (invoke-restart :use-value (+ x 10)))]
                (restart-case (error :error-condition 32)
                  (:use-value [x] x))))
```

## `warn`
Function signature:

```
(warn condition-name ...)
```

Raise `condition-name` as a warning.
Warnings are not thrown as errors when no handler is bound but their
message is printed to stderr.

### Examples
Warning is ignored if not handled:

``` fennel
(assert-eq nil (warn :warn-condition))
```

Warnings can be handled like any other conditions.
See [`error`](#error) for examples of how to handle warnings.

## `signal`
Function signature:

```
(signal condition-name ...)
```

Raise `condition-name` as a signal.
Signals are not thrown as errors when no handler is bound.

### Examples
Signal is ignored if not handled:

``` fennel
(assert-eq nil (signal :signal-condition 10))
```

Signals can be handled like any other conditions.
See [`error`](#error) for examples of how to handle signals.

## `handler-case`
Function signature:

```
(handler-case expr ...)
```

Condition handling.
Accepts expression `expr` and handlers that can be used when handling
conditions raised from within the expression.

### Examples
Handling [`error`](#error) condition:

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
handlers followed by the body expression.  Each handler is a function
of at least one argument - the signal being handled.  Other arguments
are optional, and can be used inside the handler.

If body expression signals a condition, a bound handler is invoked.
If no handler were bound for condition, condition is thrown as an
exception.

### Examples
Handlers executed but their return values are not used:

``` fennel
(handler-bind [:signal-condition (fn [] (print "caught signal condition") 10)
               :error-condition (fn [] (print "caught error condition") 20)]
  (signal :signal-condition))
;; caught signal condition
;; => nil
```

To provide a return value use either [`handler-case`](#handler-case) or [`restart-case`](#restart-case)
and [`invoke-restart`](#invoke-restart).

## `restart-case`
Function signature:

```
(restart-case expr ...)
```

Resumable condition restart point.
Accepts expression `expr` and restarts that can be used when handling
conditions thrown from within the expression.

### Examples
Specifying two restarts for `:signal-condition`:

``` fennel
(restart-case (signal :signal-condition)
  (:some-restart [] :body)
  (:some-other-restart [] :body))
```

## `invoke-restart`
Function signature:

```
(invoke-restart restart-name ...)
```

Invoke restart `restart-name` to handle condition.
Must be used only in handler functions defined with [`handler-bind`](#handler-bind).
Transfers control flow to handler function when executed.

### Examples
Handle the [`error`](#error) with `:use-value' restart:

``` fennel
(assert-eq 42 (handler-bind [:error-condition
                             (fn [_c x]
                               (invoke-restart :use-value (+ x 10))
                               (print "never prints"))]
                (restart-case (do (error :error-condition 32)
                                  (print "also never prints"))
                  (:use-value [x] x))))
```


---

Copyright (C) 2021 Andrey Listopadov

License: [MIT](https://gitlab.com/andreyorst/fennel-conditions/-/raw/master/LICENSE)


<!-- Generated with Fenneldoc v0.1.5
     https://gitlab.com/andreyorst/fenneldoc -->
