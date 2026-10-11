;;; -*- Lisp -*-

(defsystem "named-let-test"
  :author "Joe Marshall <eval.apply@gmail.com>"
  :description "FiveAM test suite for named-let."
  :license "MIT"
  :depends-on ("named-let" "fiveam")
  :components ((:file "named-let-test"))
  :perform (test-op (op c)
             (unless (symbol-call "NAMED-LET-TEST" "RUN-TESTS")
               (error "named-let tests failed"))))
