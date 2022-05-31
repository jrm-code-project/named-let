;;; -*- Lisp -*-

(defpackage "NAMED-LET"
  (:shadow "LET")
  (:use "COMMON-LISP")
  (:export "LET"
           "NAMED-LAMBDA"))

(in-package "NAMED-LET")

(defmacro named-lambda (name (&rest arglist) &body body)
  `(labels ((,name ,arglist ,@body))
     #',name))

(defmacro let (name-or-bindings bindings-or-first &body body)
  (flet ((binding-name (binding)
           (cond ((symbolp binding) binding)
                 ((not (consp binding)) (error "Bad synax: LET"))
                 ((symbolp (car binding)) (car binding))
                 (t (error "Bad syntax: LET"))))

         (binding-value (binding)
           (cond ((symbolp binding) 'nil)
                 ((not (consp binding)) (error "Bad syntax: LET"))
                 ((null (cdr binding)) 'nil)
                 ((not (consp (cdr binding))) (error "Bad syntax: LET"))
                 ((null (cddr binding)) (cadr binding))
                 (t `(progn ,@(cdr binding))))))

    (cond ((symbolp name-or-bindings)
           `(funcall (named-lambda ,name-or-bindings
                                   ,(map 'list #'binding-name
                                          bindings-or-first)
                                   ,@body)
                     ,@(map 'list #'binding-value
                            bindings-or-first)))
          ((or (null name-or-bindings)
               (consp name-or-bindings))
           `(,(cl:let ((series-package (find-package "SERIES")))
                      (if series-package
                          (cl:let ((let-symbol (find-symbol "LET" series-package)))
                                  (if (and let-symbol
                                           (fboundp let-symbol))
                                      let-symbol
                                    'cl:let))
                        'cl:let))
             ,name-or-bindings
             ,bindings-or-first
             ,@body))
          (t (error "Bad syntax: LET")))))
