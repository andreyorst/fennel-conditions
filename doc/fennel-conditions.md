# Fennel-conditions (v0.0.11-dev)
Condition system for the Fennel language.

This module provides a set of functions for control transfer, that
implement Common Lisp-inspired condition system for the Fennel language.

**Table of contents**

- [`error`](#error)
- [`warn`](#warn)
- [`signal`](#signal)
- [`invoke-restart`](#invoke-restart)
- [`continue`](#continue)
- [`invoke-debugger`](#invoke-debugger)
- [`make-condition`](#make-condition)

## `error`
Function signature:

```
(error condition-object)
```

Raise `condition-object` as an error.

This function is a drop-in replacement for the inbuilt `error`
function.  Similarly to Lua's `error` accepting message as its first
argument, this function accepts condition object as it's first
argument.  Like Lua's [`error`](#error), this function will interrupt function
execution where it was called, and no code after [`error`](#error) will be
executed.  If no handler bound for raised condition, [`error`](#error) is
promoted to Lua error with detailed message about the unhandled
condition.

```
>> (error :condition-object)
runtime error: condition condition-object was raised
stack traceback...
```

Conditions support inheritance, and all conditions that are raised
with the [`error`](#error) function automatically derive from both
`:fennel-conditions/error` and `:fennel-conditions/condition`, and can
be catched with any of these handlers.

Condition can be any Lua object, and such conditions are handled by
reference.  If more complex inheritance rules are required,
`define-condition` and [`make-condition`](#make-condition) can be used.

### Examples

Condition is thrown as a Lua error if not handled, and can be caught
with `pcall`:

``` fennel
(assert-not (pcall error :error-condition))
```

Error conditions can be handled with `handler-case`:

``` fennel
(define-condition error-condition)

(fn handle-error []
  (handler-case (error (make-condition error-condition 42))
    (error-condition [_ x] x)))

(assert-eq 42 (handle-error))
```

Errors, signal, and warnings can be recovered with `handler-bind` and
`restart-case` by using [`invoke-restart`](#invoke-restart):

``` fennel
(define-condition error-condition)

(fn recover-from-error []
  (handler-bind [error-condition
                 (fn [_ x]
                   (invoke-restart :use-value (+ x 10)))]
    (restart-case (error (make-condition error-condition 32))
      (:use-value [x] x))))

(assert-eq 42 (recover-from-error))
```

Error conditions, and Lua errors can be handled with special
`:fennel-conditions/error` and `:fennel-conditions/condition`
handlers:

``` fennel
(assert-eq 27 (handler-case (error :some-error-condition)
                (:fennel-conditions/condition [] 27)))

(assert-eq 42 (handler-case (/ 1 nil)
                (:fennel-conditions/error [] 42)))
```

## `warn`
Function signature:

```
(warn condition-object)
```

Raise `condition-object` as a warning.
Warnings are not thrown as errors when no handler is bound but their
message is printed to stderr.

Warnings derive from both `:fennel-conditions/warning` and
`:fennel-conditions/condition`, and can be catched with any of these
handlers.

### Examples

Warning is ignored if not handled:

``` fennel
(assert-eq nil (warn :warn-condition))
```

See [`error`](#error) for examples of how to handle conditions.

## `signal`
Function signature:

```
(signal condition-object)
```

Raise `condition-object` as a signal.

Raises given condition as a signal.  Signals can be handled with the
same ways as [`error`](#error) conditions, but don't promote to errors if no
handler was found.  This function transfers control flow to the
handler at the point where it was called, and no code after [`signal`](#signal)
will be executed.

Signals derive from `:fennel-conditions/condition`, and can be catched
with this handler.

### Examples

Signal is ignored if not handled:

``` fennel
(assert-eq nil (signal :signal-condition))
```

See [`error`](#error) for examples of how to handle conditions.

## `invoke-restart`
Function signature:

```
(invoke-restart restart-name ...)
```

Invoke restart `restart-name` to handle a condition.

Must be used only in handler functions defined with `handler-bind`.
Transfers control flow to handler function when executed.

### Examples

Handle the [`error`](#error) with `:use-value' restart:

``` fennel
(define-condition error-condition)

(fn handle-error []
  (handler-bind [error-condition
                 (fn [_c x]
                   (invoke-restart :use-value (+ x 10))
                   (print "never prints"))]
    (restart-case (do (error (make-condition error-condition 32))
                      (print "also never prints"))
      (:use-value [x] x))))

(assert-eq 42 (handle-error))
```

See [`error`](#error) for examples of how to handle conditions.

## `continue`
Function signature:

```
(continue)
```

Invoke the [`continue`](#continue) restart bound automatically by `cerror` macro.

Must be used only in handler functions defined with `handler-bind`.
Transfers control flow to handler function when executed.

## `invoke-debugger`
Function signature:

```
(invoke-debugger condition-object)
```

Invokes debugger for given `condition-object` to call restarts from the interactive menu.

## `make-condition`
Function signature:

```
(make-condition condition-object arg1 ...)
```

Derives condition from base `condition-object`.  Accepts any amount
of additional arguments that will be passed as arguments to handlers
when handling this condition instance.


---

Copyright (C) 2021 Andrey Listopadov

License: [MIT](https://gitlab.com/andreyorst/fennel-conditions/-/raw/master/LICENSE)


<!-- Generated with Fenneldoc v0.1.5
     https://gitlab.com/andreyorst/fenneldoc -->