(require-macros :fennel-test.test)
(local {: error : warn : signal : make-condition} (require :init))
(require-macros :macros)

(macro with-no-stderr [expr]
  `(let [stderr-mt# (getmetatable io.stderr)
         write# stderr-mt#.write]
     (tset stderr-mt# :write #"")
     (let [res# (table.pack ,expr)]
       (tset stderr-mt# :write write#)
       (table.unpack res# 1 res#.n))))


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
  (testing "nested inheritance")
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
               (err [] :ok))))

(deftest condition-arguments
  (testing "passing arguments"
    (define-condition err)
    (assert-eq
     "2 abc nil nil"
     (handler-case (error (make-condition err 2 "abc" nil nil))
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
    (assert-not
     (handler-case (signal info)
       (warning [] :bad)))
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
    (assert-is (handler-case (* 1 nil)
                 (:fennel-conditions/error [] true)))
    (assert-is (handler-case (* 1 nil)
                 (:fennel-conditions/condition [] true)))))

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
                       (:fennel-conditions/error [c] (table.insert res "catch") :ok)))
      (assert-eq res ["error" "catch"]))))
