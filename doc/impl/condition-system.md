# Condition-system.fnl (v0.0.6)

**Table of contents**

- [`handlers`](#handlers)
- [`restarts`](#restarts)
- [`raise`](#raise)
- [`handle`](#handle)
- [`invoke-restart`](#invoke-restart)
- [`compose-error-message`](#compose-error-message)
- [`pack`](#pack)
- [`unpack`](#unpack)

## `handlers`
Dynamic scope for condition handlers.

## `restarts`
Dynamic scope for restarts.

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
(handle condition-object type*)
```

Handle the `condition-object` of `type*`.

Finds the `condition-object` handler in the dynamic scope.  If found,
calls the handler, and returns a table with `:state` set to
`:handled`, and `:data` bound to a packed table of handler's return
values.

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
