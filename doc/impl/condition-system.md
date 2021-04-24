# Condition-system.fnl
Internal API for condition system.

**Table of contents**

- [`conditions`](#conditions)
- [`restarts`](#restarts)
- [`invoke-restart`](#invoke-restart)
- [`raise`](#raise)

## `conditions`
Dynamic scope for conditions.

## `restarts`
Dynamic scope for restarts.

## `invoke-restart`
Function signature:

```
(invoke-restart restart-name ...)
```

Invoke `restart-name` with args.

## `raise`
Function signature:

```
(raise t condition-name ...)
```

Raise `condition-name` of type `t` with given arguments.
Supported types include: `:signal`, `:warn`, and `:error`.


---

Copyright (C) 2021 Andrey Listopadov

License: [MIT](https://gitlab.com/andreyorst/fennel-conditions/-/raw/master/LICENSE)


<!-- Generated with Fenneldoc v0.1.5
     https://gitlab.com/andreyorst/fenneldoc -->
