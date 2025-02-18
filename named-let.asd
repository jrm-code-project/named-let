;;; -*- Lisp -*-

(defsystem "named-let"
  :author "Joe Marshall <eval.apply@gmail.com>"
  :bug-tracker "https://github.com/jrm-code-project/named-let/issues"
  :description "Named LET special form."
  :homepage "https://github.com/jrm-code-project/named-let/"
  :license "MIT"
  :long-description "A named variant of the standard LET special form that allows a name to be provided before the binding list.  The name is bound to the lambda that binds the variables making it possible to recursively call the LET expression with new bindings."
  :mailto "eval.apply@gmail.com"
  :maintainer "Joe Marshall <eval.apply@gmail.com>"
  :source-control (:git "https://github.com/jrm-code-project/named-let.git")
  :version "1.0.0"
  :depends-on ()
  :components ((:file "named-let")))
