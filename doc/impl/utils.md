# Utils.fnl (v0.0.3)

**Table of contents**

- [`build-arg-str`](#build-arg-str)
- [`compose-error-message`](#compose-error-message)
- [`get-data`](#get-data)
- [`get-name`](#get-name)
- [`pack`](#pack)
- [`unpack`](#unpack)

## `build-arg-str`
Function signature:

```
(build-arg-str sep args)
```

Constructs the string of arguments pretty-printed values stored in
`args`, separated by `sep`.

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

## `get-data`
Function signature:

```
(get-data condition-object)
```

Extracts data from `condition-object`.

### Examples
Extracting data set with `make-condition` function:

``` fennel
(define-condition simple-error)
(assert-eq
 {1 :a 2 :b :n 2}
 (get-data (make-condition simple-error :a :b)))
```

Primitive objects return table with `:n` set to 0:

``` fennel
(assert-eq
 {:n 0}
 (get-data :simple-condition))
```

## `get-name`
Function signature:

```
(get-name condition-object)
```

Extracts name string from `condition-object`.

### Examples
Condition objects return base condition name:

``` fennel
(define-condition simple-error)
(assert-eq
 :simple-error
 (get-name (make-condition simple-error -1)))
```

Primitive objects are transformed with `tostring`:

``` fennel
(assert-eq
 "-1"
 (get-name -1))
```

## `pack`
See [utils.md#pack](utils.md#pack)

## `unpack`
See [utils.md#unpack](utils.md#unpack)


---

Copyright (C) 2021 Andrey Listopadov

License: [MIT](https://gitlab.com/andreyorst/fennel-conditions/-/raw/master/LICENSE)


<!-- Generated with Fenneldoc v0.1.5
     https://gitlab.com/andreyorst/fenneldoc -->
