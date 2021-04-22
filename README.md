# Fennel Conditions

WIP implementation of condition system from Common Lisp.


## Usage

Clone this repo into your project, and require relevant macros:

    git clone https://gitlab.com/andreyorst/fennel-conditions.git

``` clojure
(import-macros
 {: restart-case : handler-bind : error : invoke-restart}
 :fennel-conditions.conditions)
```

`error` is a macro meant to replace inbuilt `error`.
It has a bit different interface than conventional Lua `error` function, as it accepts condition as it's first argument and arguments of that condition.
As ordinary `error`, this macro will also interrupt function where it was called and no code after `error` will be executed.

`restart-case` is a macro that accepts expression as it's first argument, and restarts as the rest arguments.
Restarts are lists where first object is of any type, and the rest is fn-tail: sequential table of function arguments, and function body:

``` clojure
(local special-restart {:some :data})
(fn foo []
  (restart-case (error :foo)
    (:restart1 [x] x)
    (special-restart [x y] (+ x y))))
```

If expression or any of it's subsequent expressions call `error` macro, it will be possible to return into `restart-case` and execute one of the provided restarts.

`handler-bind` macro registers handlers for conditions thrown with `error`.
Each condition is bound to a function, which accepts condition as first argument, and decides whether it is able to handle the condition.
If handler function exits normally, the `error` is rethrown to an upper handler.
If there's no upper handler, the `error` is thrown as Lua `error`.

``` clojure
(handler-bind [:foo #(invoke-restart special-restart 1 2)]
  (foo)) ;; => 3
```

`invoke-restart` macro is used to handle the condition.
This macro also transfers control flow to outer function, which means that any code after `invoke-restart` will not run.
It accepts restart as it's first argument, and the rest arguments are passed to the specified restart.
When condition handler is exited with `invoke-restart` the condition will not be raised to upper handlers.


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

(fn log-analyzer []
  (handler-bind [:malformed-log-entry-error
                 (fn [_c data]
                   (invoke-restart :reparse-entry (fix-log-entry data.text)))]
    (each [_ line (ipairs (analyze-log "log.txt"))]
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
