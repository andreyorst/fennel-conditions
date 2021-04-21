# Fennel Conditions

WIP implementation of condition system from Common Lisp.

## Usage

Clone this repo into your project, and require relevant functions and macros:

    git clone https://gitlab.com/andreyorst/fennel-conditions.git

``` clojure
(local c (require :fennel-conditions))
(import-macros {: restart-case : handler-bind} :fennel-conditions.macros)
```

Conditions are thrown with `c.error` and handled with `handler-bind`.
Restarts can be used with `restart-case`.

## Examples

Beyond is the port of the example from [Chapter 19 of Practical Common Lisp book](http://www.gigamonkeys.com/book/beyond-exception-handling-conditions-and-restarts.html).

``` clojure
(local c (require :fennel-conditions))
(import-macros {: restart-case : handler-bind} :fennel-conditions.macros)

(fn well-formed-log-entry? [text]
  (pick-values 1
    (text:match "^well formed log entry")))

(fn parse-log-entry [text]
  (if (well-formed-log-entry? text)
      {:parsed-log-entry text}
      (c.error :malformed-log-entry-error {:text text})))

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
                   (c.invoke-restart :reparse-entry (fix-log-entry data.text)))]
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
