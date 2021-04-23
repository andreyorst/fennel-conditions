# Fennel Conditions

Implementation of condition system inspired by Common Lisp.

This library provides several macros that implement resumable exception model for Fennel language.
It is based on the idea of leveraging tables to implement pseudo-dynamic scope.


## Installation

Clone this repo into your project:

    git clone https://gitlab.com/andreyorst/fennel-conditions.git


## Usage

This library provides it's public API in terms of a set of macros.
Each macro calls internal API functions, which means that `impl/condition-system.fnl` must be compiled in order to use this library in resulting lua application without Fennel.

To use provided macros in your application, import needed macros from `conditions.fnl` module:

``` fennel
(import-macros
 {: restart-case
  : handler-case
  : handler-bind
  : error
  : signal
  : invoke-restart}
 :fennel-conditions.conditions)
```


### `signal`

A macro raises given condition as a signal.
Signals can be handled with `handler-case` or `handler-bind`, and don't promote to errors if no handler found.
This macro will interrupt function execution at the point where it was called, and no code after `signal` will be executed.

``` fennel
>> (signal :condition-name)
nil
```


### `error`

A macro meant to replace inbuilt `error` function.
It has a bit different interface than conventional Lua `error` function, as it accepts condition as it's first argument and arguments of that condition.
Similarly to `signal` and Lua's `error`, this macro will interrupt function execution where it was called, and no code after `error` will be executed.
If no handler bound for raised condition, `error` is promoted to Lua error with detailed message about condition.

``` fennel
>> (error :condition-name 42)
runtime error: condition "condition-name" was thrown with the following arguments: 42
stack traceback...
```


### `handler-case`

A macro that provides facility to catch named conditions raised with `signal` or `error` macros.
It accepts expression as first argument, which is run and if any condition is raised, before propagating condition to error, a handler is searched.
If handler is bound for this condition, it is executed, and the result of `handler-case` expression will be result of the handler.
Handlers are lists where first object represents condition, which can be of any type, and the rest is fn-tail - sequential table of function arguments, and function body:

```fennel
>> (handler-case 42
     (:some-error [condition]
       (print "caught" condition)
       10))
42
>> (handler-case (error :some-error 42)
     (:some-error [condition x]
       (print (.. "caught " condition " with arg: " x))
       10))
caught some-error with arg: 42
10
```


### `restart-case`

A macro that accepts expression as it's first argument, and restarts as the rest arguments.
Similarly to `handler-case` restarts are lists with condition and fn-tail.

``` fennel
(restart-case (error :foo)
  (:some-restart [x] x)
  (:other-restart [x y] (+ x y)))
```

If expression or any of it's subsequent expressions calls `signal` or `error` macro, it will be possible to return into the `restart-case`, and execute one of the provided restarts.
Restarts can be executed with `invoke-restart` macro.
Restarts are not special objects, although they can be, it is not meaningful to use something other than strings because `invoke-restart` will not pass the restart object to a restart function.


### `invoke-restart`

A macro to handle the condition.
This macro transfers control flow to outer function, which means that any code after `invoke-restart` call will not run.
It accepts restart object as it's first argument, and the rest arguments are passed to the specified restart function.
When `error` condition handler is exited with `invoke-restart` the condition will not be raised to upper handlers.


### `handler-bind`

A macro for registering handlers for conditions.
Each condition is bound to a function, which accepts condition as first argument, and decides whether it is able to handle the condition.
If handler function exits normally, `signal` and `error` are rethrown to an upper handler.
If there's no upper handler, the `error` is thrown as Lua `error`.

``` fennel
>> (fn foo []
     (restart-case (error :foo)
       (:some-restart [x] x)
       (:other-restart [x y] (+ x y))))
>> (handler-bind [:foo #(invoke-restart :other-restart 1 2)]
     (foo))
3
>> (handler-bind [:foo #(invoke-restart :some-restart 10)]
     (foo))
10
>> (handler-bind [:foo #(print (.. "decline to handle: " $))]
     (foo))
decline to handle: foo
runtime error: condition "foo" was thrown with the following arguments:
stack traceback...
```


## Examples

Beyond is the port of the example from [Chapter 19 of Practical Common Lisp book](http://www.gigamonkeys.com/book/beyond-exception-handling-conditions-and-restarts.html).

``` clojure
(import-macros
 {: restart-case : handler-bind : error : invoke-restart}
 :fennel-conditions.conditions)

(fn well-formed-log-entry? [text]
  (pick-values 1
    (text:match "^well formed log entry")))

(fn parse-log-entry [text]
  (if (well-formed-log-entry? text)
      {:parsed-log-entry text}
      (error :malformed-log-entry-error {:text text})))

(fn parse-log-file [file]
  (with-open [f (io.open file :r)]
    (icollect [line (f:lines)]
      (restart-case (parse-log-entry line)
        (:use-value [value] value)
        (:reparse-entry [fixed-text] (parse-log-entry fixed-text))))))

(fn analyze-entry [entry]
  entry.parsed-log-entry)

(fn analyze-log [log]
  (icollect [_ entry (ipairs (parse-log-file log))]
    (analyze-entry entry)))

(fn fix-log-entry [text]
  (.. (text:gsub "^not a " "") " (fixed)"))

(fn log-analyzer [log-file]
  (handler-bind [:malformed-log-entry-error
                 (fn [_c data]
                   (invoke-restart :reparse-entry (fix-log-entry data.text)))]
    (each [_ line (ipairs (analyze-log log-file))]
      (print line))))

(log-analyzer "log.txt")
```

For a log file `log.txt` with the following contents:

```
well formed log entry 1
well formed log entry 2
not a well formed log entry 3
well formed log entry 4
not a well formed log entry 5
not a well formed log entry 6
well formed log entry 7
```

Calling `log-analyzer` will prints the following:

```
well formed log entry 1
well formed log entry 2
well formed log entry 3 (fixed)
well formed log entry 4
well formed log entry 5 (fixed)
well formed log entry 6 (fixed)
well formed log entry 7
```
