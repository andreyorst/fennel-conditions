(local {: metadata : view} (require :fennel))

(fn current-thread []
  "Returns the name of current thread when possible"
  (or (and coroutine
           coroutine.running
           (tostring (coroutine.running)))
      :main))

(local dynamic-scope
  (metadata:set
   {}
   :fnl/docstring
   "Dynamic scope for the condition system.

Dynamic scope is a maintained table where handlers and restarts are
stored thread-locally.  Thread name is obtained with
`coroutine.running` call and each thread holds a table with the
following keys `:handlers`, `:restarts`, and `:current-context`.
Handlers and restarts itselves are tables."))

(fn get-name [condition-object]
  "Extracts name string from `condition-object'.

# Examples

Condition objects return base condition name:

``` fennel
(define-condition simple-error)
(assert-eq
 :simple-error
 (get-name (make-condition simple-error -1)))
```

Primitive objects are transformed with `tostring`:

``` fennel
(assert-eq \"-1\" (get-name -1))
```"
  (if (and (= :table (type condition-object))
           (= condition-object.type :condition))
      (tostring condition-object.id.name)
      (view condition-object)))

(fn get-data [condition-object]
  "Extracts data from `condition-object'.

# Examples

Extracting data set with `make-condition' function:

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
```"
  (match (and (= :table (type condition-object))
              (= condition-object.type :condition)
              condition-object.data)
    (where data data) data
    _ {:n 0}))

(fn build-arg-str [sep args]
  "Constructs the string of arguments pretty-printed values stored in
`args', separated by `sep'."
  (let [res []]
    (for [i 1 args.n]
      (table.insert res (view (. args i) {:one-line? true})))
    (table.concat res sep)))

(fn compose-error-message [condition-object]
  "Composes message for `condition-object' based on it's name and data
stored within the object.

# Examples

Conditions without data produce short messages:

``` fennel
(define-condition simple-error)
(assert-eq
 \"condition simple-error was raised\"
 (compose-error-message simple-error))
```

Conditions with data produce extended messages:

``` fennel
(define-condition simple-error)
(assert-eq
 \"condition simple-error was raised with the following arguments: 1, 2, 3\"
 (compose-error-message (make-condition simple-error 1 2 3)))
```"
  (.. "condition " (get-name condition-object)
      " was raised"
      (match (build-arg-str ", " (get-data condition-object))
        "" ""
        s (.. " with the following arguments: " s))))

(local unpack-fn (or table.unpack _G.unpack))
(fn unpack [tbl]
  "Automatically try to query `tbl` for it's size `n` and unpack whole
thing."
  (let [len (or tbl.n (length tbl))]
    (unpack-fn tbl 1 len)))

(fn pack [...]
  "Portable `table.pack` implementation."
  (doto [...]
    (tset :n (select :# ...))))

{: current-thread
 : dynamic-scope
 : get-data
 : get-name
 : compose-error-message
 : build-arg-str
 : unpack
 : pack}
