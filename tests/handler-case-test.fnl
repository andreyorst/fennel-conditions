(require-macros :fennel-test.test)
(local {: error : warn : signal : make-condition} (require :init))
(require-macros :macros)

(deftest handling-base
  (testing "throwing condtion"
    (define-condition err)
    (assert-eq 42 (handler-case (error err) (err [] 42)))))

(deftest handler-order
  (testing "first handler called first"
    (assert-eq 42 (handler-case (error :err)
                    (:err [] 42)
                    (:err [] 27)))))

(deftest handling-unhandled
  (testing "handling error on top level"
    (assert-not (pcall #(handler-case (error :error) (:info [] 10))))))

(deftest handling-primitive-types
  (testing "conditions as strings"
    (assert-eq 10 (handler-case (error :error)
                    (:error [] 10)))

    (assert-eq 27 (handler-case (+ 10 (handler-case (error :error)
                                        (:error [] 17)))
                    (:error [] 42)))

    (assert-eq 42 (handler-case (+ 10 (handler-case (error :error)
                                        (:info [] 17)))
                    (:error [] 42)))))

(deftest nested-handler-cases
  (define-condition err)
  (define-condition info :parent err)

  (testing "outer handler is used"
    (assert-eq
     42
     (handler-case
         (handler-case (error (make-condition err))
           (info [] 27))
       (err [] 42))))

  (testing "inner handler is used"
    (assert-eq
     42
     (handler-case (+ 10 (handler-case (error (make-condition info))
                           (info [] 32)))
       (err [] (print "outer handler")))))

  (testing "inheritance"
    (define-condition warning :parent err)
    (assert-eq
     42
     (handler-case
         (handler-case (error (make-condition warning))
           (info [] 27))
       (err [] 42)))
    (assert-eq
     27
     (handler-case
         (handler-case (error (make-condition info))
           (info [] 27))
       (err [] 42)))
    (assert-eq
     27
     (handler-case (error (make-condition info))
       (info [] 27)
       (err [] 42)))
    (assert-eq
     42
     (handler-case (error (make-condition warning))
       (info [] 27)
       (err [] 42)))))

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
     42
     (handler-case (error err1)
       (err [] 42)))))

(deftest non-error-conditions
  (testing "Signals and warnings do not transfer control flow if unhandled"
    (define-condition info)
    (define-condition warning)
    (assert-not
     (handler-case (signal info)
       (warning [] 42)))
    (assert-not
     (handler-case (warn warning)
       (info [] 42))))

  (testing "Signals and warnings transfer control flow if handled"
    (define-condition info)
    (define-condition warning)
    (assert-eq
     42
     (handler-case (signal info)
       (info [] 42)))
    (assert-eq
     27
     (handler-case (warn warning)
       (warning [] 27)))))

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
