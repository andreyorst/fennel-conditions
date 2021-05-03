(require-macros :fennel-test.test)
(require-macros :macros)
(local {: make-condition : invoke-restart : error} (require :init))

(fn well-formed-log-entry? [text]
  (pick-values 1
    (text:match "^well formed log entry")))

(define-condition malformed-log-entry-error)

(fn parse-log-entry [text]
  (if (well-formed-log-entry? text)
      {:parsed-log-entry text}
      (error (make-condition malformed-log-entry-error {:text text}))))

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
  (handler-bind [malformed-log-entry-error
                 (fn [_c data]
                   (invoke-restart :reparse-entry (fix-log-entry data.text)))]
    (analyze-log log-file)))


(deftest practical-common-lisp
  (testing "Example from practical common lisp"
    (assert-eq ["well formed log entry 1"
                "well formed log entry 2"
                "well formed log entry 3 (fixed)"
                "well formed log entry 4"
                "well formed log entry 5 (fixed)"
                "well formed log entry 6 (fixed)"
                "well formed log entry 7"]
               (log-analyzer "tests/data/log.txt"))))
