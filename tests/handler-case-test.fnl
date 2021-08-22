(require-macros :fennel-test.test)
(local {: error : warn : signal : make-condition &as cs} (require :init))
(require-macros :init-macros)

(macro with-no-stderr [expr]
  "Suppress output to stderr."
  `(let [stderr-mt# (. (getmetatable io.stderr) :__index)
         write# stderr-mt#.write]
     (tset stderr-mt# :write (fn [fd# ...]
                               (when (not= fd# io.stderr)
                                 (write# fd# ...))))
     (let [res# (table.pack ,expr)]
       (tset stderr-mt# :write write#)
       (table.unpack res# 1 res#.n))))

(deftest runtime-check
  (testing "invalid handler-case"
    (let [(ok? msg) (pcall #(handler-case :ok
                              (nil [] :bad)))]
      (assert-not ok?)
      (assert-is (msg:match "condition object must not be nil")))))

(deftest handling-base
  (testing "throwing condtion"
    (define-condition err)
    (assert-eq :ok (handler-case (error err) (err [] :ok)))))

(deftest handler-order
  (testing "first handler called first"
    (assert-eq :ok (handler-case (error :err)
                     (:err [] :ok)
                     (:err [] :bad)))))

(deftest handling-unhandled
  (testing "handling error on top level"
    (assert-not (pcall #(handler-case (error :error) (:info [] 10))))))

(deftest handling-primitive-types
  (testing "conditions as strings"
    (assert-eq :ok (handler-case (error :error)
                     (:error [] :ok)))

    (assert-eq 27 (handler-case (+ 10 (handler-case (error :error)
                                        (:error [] 17)))
                    (:error [] :bad)))

    (assert-eq :ok (handler-case (+ 10 (handler-case (error :error)
                                         (:info [] 17)))
                     (:error [] :ok)))))

(deftest nested-handler-cases
  (define-condition err)
  (define-condition info :parent err)

  (testing "outer handler is used"
    (assert-eq
     :ok
     (handler-case
         (handler-case (error (make-condition err))
           (info [] :bad))
       (err [] :ok))))

  (testing "inner handler is used"
    (assert-eq
     42
     (handler-case (+ 10 (handler-case (error (make-condition info))
                           (info [] 32)))
       (err [] :bad))))

  (testing "inheritance"
    (define-condition warning :parent err)
    (assert-eq :ok
               (handler-case
                   (handler-case (error (make-condition warning))
                     (info [] :bad))
                 (err [] :ok)))
    (assert-eq :ok
               (handler-case
                   (handler-case (error (make-condition info))
                     (info [] :ok))
                 (err [] :bad)))
    (assert-eq :ok
               (handler-case (error (make-condition info))
                 (info [] :ok)
                 (err [] :bad)))
    (assert-eq :ok
               (handler-case (error (make-condition warning))
                 (info [] :bad)
                 (err [] :ok))))

  (testing "nested inheritance"
    (define-condition warning :parent err)
    (define-condition simple-warning :parent warning)
    (define-condition very-simple-warning :parent simple-warning)
    (assert-eq :ok
               (handler-case (error very-simple-warning)
                 (info [] :bad)
                 (err [] :ok)))
    (assert-eq :ok
               (handler-case (error (make-condition very-simple-warning))
                 (info [] :bad)
                 (err [] :ok)))))

(deftest condition-arguments
  (testing "passing arguments"
    (define-condition err)
    (assert-eq
     "2 abc nil nil"
     (handler-case (error (make-condition err 2 "abc" nil nil))
       (err [_c a b c d] (string.format "%s %s %s %s" a b c d)))))
  (testing "getting when handling parent condition"
    (define-condition err)
    (define-condition err2 :parent err)
    (assert-eq
     "2 abc nil nil"
     (handler-case (error (make-condition err2 2 "abc" nil nil))
       (err [_c a b c d] (string.format "%s %s %s %s" a b c d))))))

(deftest unspecific-handler
  (testing "handling by concrete condition"
    (define-condition err)
    (local err1 (make-condition err))
    (assert-eq
     :ok
     (handler-case (error err1)
       (err [] :ok)))))

(deftest non-error-conditions
  (testing "Signals and warnings do not transfer control flow if unhandled"
    (define-condition info)
    (define-condition warning)
    (assert-not (signal info))
    (assert-not
     (handler-case (signal info)
       (warning [] :bad)))
    (assert-not (with-no-stderr (warn warning)))
    (assert-not
     (with-no-stderr
      (handler-case (warn warning)
        (info [] :bad)))))

  (testing "Signals and warnings transfer control flow if handled"
    (define-condition info)
    (define-condition warning)
    (assert-eq
     :ok
     (handler-case (signal info)
       (info [] :ok)))
    (assert-eq
     :ok
     (handler-case (warn warning)
       (warning [] :ok)))))

(deftest lua-errors
  (testing "handling lua errors"
    (assert-eq :ok (handler-case (* 1 nil)
                     (cs.Error [] :ok)))
    (assert-eq :ok (handler-case (* 1 nil)
                     (cs.Condition [] :ok)))

    (assert-eq :ok (handler-case (restart-case (* 1 nil)
                                   (:r [] :bad))
                     (cs.Error [] :ok)))))

(deftest rethrowing
  (testing "throwing condition from handler"
    (let [res []]
      (assert-eq :ok (handler-case (handler-case (error :err)
                                     (:err [c] (table.insert res "throw") (error c)))
                       (:err [c] (table.insert res "catch") :ok)))
      (assert-eq res ["throw" "catch"])))

  (testing "Lua error in handler"
    (let [res []]
      (assert-eq :ok (handler-case (handler-case (error :err)
                                     (:err [c] (table.insert res "error") (* 1 nil)))
                       (cs.Error [c] (table.insert res "catch") :ok)))
      (assert-eq res ["error" "catch"]))))
