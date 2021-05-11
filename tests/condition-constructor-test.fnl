(require-macros :fennel-test.test)
(local {: error : warn : signal : make-condition} (require :init))
(require-macros :macros)

(deftest constructors
  (testing "default constructor"
    (define-condition err)
    (assert-eq {:type :condition :name "err" :id err} err))
  (testing "alternative name"
           (define-condition warning :name "warn")
           (assert-eq {:type :condition :name "warn" :id warning} warning))
  (testing "inheritance"
    (define-condition err)
    (define-condition warning :parent err)
    (assert-eq {:type :condition :name "warning" :parent err :id warning} warning)))

(deftest creating-instances
  (testing "no params"
    (define-condition err)
    (let [err1 (make-condition err)
          err2 (make-condition err)]
      (assert-eq {:id err :type :condition} err1)
      (assert-eq {:id err :type :condition} err2)
      (assert-eq (tostring err1.id) (tostring err2.id))))

  (testing "overriding parameters"
    (define-condition err)
    (let [err1 (make-condition err 1)
          err2 (make-condition err 1 2 3 4)]
      (assert-eq {:data {1 1 :n 1} :id err :type :condition} err1)
      (assert-eq {:data {1 1 2 2 3 3 4 4 :n 4} :id err :type :condition} err2))))

(deftest unhandled-condition-message
  (testing "primitive message"
    (let [(_ msg) (pcall error :primitive-condition)]
      (assert-is (msg:match "condition \"primitive%-condition\" was raised$"))))

  (testing "object message"
    (define-condition err)
    (let [(_ msg) (pcall error (make-condition err :a nil 42 {:b :c} [:d]))]
      (assert-is
       (msg:match "condition err was raised with the following arguments: \"a\", nil, 42, {:b \"c\"}, %[\"d\"%]$"))))
  )
