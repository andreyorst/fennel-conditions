# Fennel-conditions (v0.1.0)
Condition system for the Fennel language.

This module provides a set of functions for control transfer, that
implement Common Lisp-inspired condition system for the Fennel
language.

**Table of contents**

- [`error`](#error)
- [`warn`](#warn)
- [`signal`](#signal)
- [`invoke-restart`](#invoke-restart)
- [`continue`](#continue)
- [`Condition`](#condition)
- [`Error`](#error-1)
- [`Warning`](#warning)
- [`find-restart`](#find-restart)
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
argument, although it ignores the second argument, because the
throwing semantics are different.  Like Lua's `error`, this function
will interrupt function execution where it was called, and no code
after `error` will be executed.  If no handler bound for raised
condition, the condition will be promoted to a Lua error with detailed
message about the unhandled condition and it's arguments, if any.

```
>> (error :condition-object)
runtime error: condition condition-object was raised
stack traceback...
```

Condition objects support inheritance, and all conditions that are
raised with the [`error`](#error) function automatically derive from [`Error`](#error-1)
condition, and can be catched with handler bound to this condition
object.

Likewise all conditions automatically derive from [`Condition`](#condition)
condition, which is a base type for all condition objects.

Any Lua object can be a condition, and such conditions are handled by
reference, but still can be handled by binding handler for [`Condition`](#condition)
and [`Error`](#error-1) (in case Lua object was raised with the `error` function).
If more complex inheritance rules are required, `define-condition` and
[`make-condition`](#make-condition) can be used.

### Examples

Condition is thrown as a Lua error if not handled, and can be caught
with `pcall`:

``` fennel
(assert-not (pcall error :error-condition))
```

Error conditions, and Lua errors can be handled by binding a handler
to `Error` and `Condition` conditions via `handler-case`:

``` fennel
(assert-eq 27 (handler-case (error :some-error-condition)
                (Condition [] 27)))

(assert-eq 42 (handler-case (/ 1 nil)
                (Error [] 42)))
```

User-defined conditions can be handled by their base type:

``` fennel
(define-condition some-error)

(fn handle-condition []
  (handler-case (error (make-condition some-error 42))
    (some-error [_ x] x)))

(assert-eq 42 (handle-condition))
```

Conditions also can be recovered with `handler-bind` and
`restart-case` by using [`invoke-restart`](#invoke-restart).  Error handling code doesn't
necessary has to be a part of the same lexical scope of where
condition was raised:

``` fennel
(define-condition some-error)

(fn some-function []
  (restart-case (error (make-condition some-error 32))
    (:use-value [x] x)))

(handler-bind [some-error
               (fn [_ x]
                 (invoke-restart :use-value (+ x 10)))]
  (assert-eq 42 (some-function)))
```

In this case, `restart-case` is in the lexical scope of
`some-function`, and `handler-bind` is outside of it's lexical
scope. When `some-function` raises `some-error` condition a handler
bound to this condition is executed. Handler invokes restart named
`:use-value`, which recovers function from error state and function
returns the value provided by the restart.

## `warn`
Function signature:

```
(warn condition-object)
```

Raise `condition-object` as a warning.

Warnings are not thrown as errors when no handler is bound but the
message is printed to standard error out when warning condition is not
handled.  Similarly to [`signal`](#signal), the control is temporarily
transferred to a handler, but code evaluation continues if handler did
not transferred control flow.

Warnings derive from both `Warning` and `Condition`, and can be
catched by binding handler to any of these types.

### Examples

Warning is ignored if not handled:

```
(warn :warn-condition)
```

See [`error`](#error) for examples how to handle conditions.

## `signal`
Function signature:

```
(signal condition-object)
```

Raise `condition-object` as a signal.

Signals can be handled the same way as [`error`](#error) conditions, but don't
promote to errors if no handler was found.  This function transfers
control flow to the handler at the point where it was called but will
continue execution if handler itself doesn't transfer control flow.

Signals derive from `Condition`, and can be catched with handler bound
to this type.

### Examples

Signal is ignored if not handled:

``` fennel
(assert-eq nil (signal :signal-condition))
```

Handler doesn't transfer control flow, and code evaluation continues
after [`signal`](#signal) call:

``` fennel
(local result [])

(handler-bind [:some-signal
               (fn [] (table.insert result 2))]
  (table.insert result 1)
  (signal :some-signal)
  (table.insert result 3))

(assert-eq [1 2 3] result)
```

See [`error`](#error) for more examples on how to handle conditions.

## `invoke-restart`
Function signature:

```
(invoke-restart restart-name ...)
```

Invoke restart `restart-name` to handle a condition.

Additional arguments are passed to restart function as arguments.

Must be used only within the dynamic scope of `restart-case`.
Transfers control flow to restart function when executed.

### Examples

Handle the [`error`](#error) with the `:use-value` restart:

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

See [`error`](#error) for examples how to handle conditions.

## `continue`
Function signature:

```
(continue)
```

Invoke the [`continue`](#continue) restart, which is automatically bound by `cerror` macro.

Must be used only within the dynamic scope of `restart-case`.
Transfers control flow to handler function when executed.

## `Condition`
Condition object that acts as a base for all conditions.

## `Error`
Condition object that acts as a base for all error conditions.
Inherits [`Condition`](#condition).

## `Warning`
Condition object that acts as a base for all warning conditions.
Inherits [`Condition`](#condition).

## `find-restart`
Function signature:

```
(find-restart restart-name)
```

Searches `restart-name` in the dynamic scope, and if found, returns
its name.

## `invoke-debugger`
Function signature:

```
(invoke-debugger condition-object)
```

Invokes debugger for given `condition-object` to call restarts from
the interactive menu.

## `make-condition`
Function signature:

```
(make-condition condition-object arg1 ...)
```

Creates an instance of `condition-object`.  Accepts any amount of
additional arguments that will be passed as arguments to a handler
when handling this condition instance.

Condition created with `define-condition` and derived condition are
different objects, but the condition system sees those as the same
type.  Comparison semantics are such that derived condition is equal
to its base condition object.

### Examples

Defining a condition, making instance of this condition with two
arguments, and registering the handler for the original condition
object:

``` fennel
(define-condition some-condition)

(handler-case
    (error (make-condition some-condition {:foo "bar"} 42))
  (some-condition [c foo-bar forty-two]
    (assert-is (= some-condition c)) ; condition instance is equal to its base type
    (assert-eq {:foo "bar"} foo-bar)
    (assert-eq 42 forty-two)))
```


---

Copyright (C) 2021 Andrey Listopadov

License: [MIT](https://gitlab.com/andreyorst/fennel-conditions/-/raw/master/LICENSE)


<!-- Generated with Fenneldoc v0.1.5
     https://gitlab.com/andreyorst/fenneldoc -->
