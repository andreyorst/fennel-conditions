# Conditions.fnl (v0.0.2)
Condition system for Fennel language.

This module provides a set of macros, that implement Common
Lisp-inspired condition system for Fennel language.

Because public API is macros only, it is not possible to use this
library from pure Lua. This library depends on Fennel compiler, but it
is embedded in the internal API, and can be packaged together with the
library.

**Table of contents**

- [`error`](#error)
- [`cerror`](#cerror)
- [`warn`](#warn)
- [`signal`](#signal)
- [`handler-case`](#handler-case)
- [`handler-bind`](#handler-bind)
- [`restart-case`](#restart-case)
- [`invoke-restart`](#invoke-restart)
- [`continue`](#continue)

## `error`
Function signature:

```
(error condition-object ...)
```

Raise `condition-object` as an error.

This macro is meant to replace inbuilt [`error`](#error) function.  It has a bit
different interface than conventional Lua [`error`](#error) function, as it
accepts condition as it's first argument and arguments of that
condition.  Similarly to [`signal`](#signal) and Lua's [`error`](#error), this macro will
interrupt function execution where it was called, and no code after
[`error`](#error) will be executed.  If no handler bound for raised condition,
[`error`](#error) is promoted to Lua error with detailed message about
condition.

```
>> (error :condition-object 42)
runtime error: condition "condition-object" was thrown with the following arguments: 42
stack traceback...
```

### Examples
Error is thrown a Lua if not handled, thus can be caught with
`pcall` (note that [`error`](#error) is wrapped into anonymous function, because
it is a macro):

``` fennel
(assert-not (pcall #(error :error-condition)))
```

Errors can be handled with [`handler-case`](#handler-case):

``` fennel
(fn handle-error []
  (handler-case (error :error-condition 42)
    (:error-condition [_ x] x)))

(assert-eq 42 (handle-error))
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

## `cerror`
Function signature:

```
(cerror continue-description condition-object ...)
```

Raise `condition-object` as an error with continue restart described by `continue-description`.

Similarly to [`error`](#error), [`cerror`](#cerror) raises condition as an error, but
automatically binds the [`continue`](#continue) restart, which can be used either
with the [`continue`](#continue) function in the handler, or in the interactive
debugger.  The `continue-description` is a string, describing what
will happen if [`continue`](#continue) restart is invoked.

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

## `warn`
Function signature:

```
(warn condition-object ...)
```

Raise `condition-object` as a warning.
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
(signal condition-object ...)
```

Raise `condition-object` as a signal.

Raises given condition as a signal.  Signals can be handled
with [`handler-case`](#handler-case) or [`handler-bind`](#handler-bind), and don't promote to errors if
no handler found.  This macro will interrupt function execution at the
point where it was called, and no code after [`signal`](#signal) will be
executed.


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
conditions raised from within the expression.  Provides the facility
to catch named conditions raised with [`signal`](#signal) or [`error`](#error) macros.  If
any condition is raised, before propagating condition to error, a
handler is searched.  If handler is bound for this condition, it is
executed, and the result of [`handler-case`](#handler-case) expression will be result
of the handler.

Handlers are lists where first object represents condition, which can
be of any type, and the rest is fn-tail - sequential table of function
arguments, and function body.


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
(assert-not
 (handler-bind [:signal-condition (fn [] (print "caught signal condition") 10)
                :error-condition (fn [] (print "caught error condition") 20)]
   (signal :error-condition)))
```

To provide a return value use either [`handler-case`](#handler-case) or [`restart-case`](#restart-case)
and [`invoke-restart`](#invoke-restart).

## `restart-case`
Function signature:

```
(restart-case expr ...)
```

**Undocumented**

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
(fn handle-error []
  (handler-bind [:error-condition
                 (fn [_c x]
                   (invoke-restart :use-value (+ x 10))
                   (print "never prints"))]
    (restart-case (do (error :error-condition 32)
                      (print "also never prints"))
      (:use-value [x] x))))

(assert-eq 42 (handle-error))
```

## `continue`
Function signature:

```
(continue)
```

Invoke [`continue`](#continue) restart bound by [`cerror`](#cerror).


---

Copyright (C) 2021 Andrey Listopadov

License: [MIT](https://gitlab.com/andreyorst/fennel-conditions/-/raw/master/LICENSE)


<!-- Generated with Fenneldoc v0.1.5
     https://gitlab.com/andreyorst/fenneldoc -->
