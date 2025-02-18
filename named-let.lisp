;;; -*- Lisp -*-

(defpackage "NAMED-LET"
  (:shadow "LET")
  (:use "COMMON-LISP")
  (:export
   "FLAMBDA"
   ;; "LET"
   "NAMED-LAMBDA"
   "NAMED-LET"))

(in-package "NAMED-LET")

(defmacro flambda ((&rest arglist) &body body)
  "A lambda that binds its arguments in the function namespace."
  (cl:let ((vars (map 'list (lambda (arg)
                              (gensym (concatenate 'string (symbol-name arg) "-")))
                      arglist)))
    `(lambda ,vars
       (flet ,(map 'list (lambda (arg var)
                           `(,arg (&rest args)
                              (apply ,var args)))
                   arglist
                   vars)
         ,@body))))

(defmacro named-lambda (name (&rest arglist) &body body)
  `(labels ((,name ,arglist ,@body))
     #',name))

;; Override the LET special form with an extended syntax.
(defmacro let (name-or-bindings bindings-or-first &body body)
  "The LET special form with extended syntax.  A symbolic NAME may be provided before the binding list
that is bound to the function performing the LET binding so that the function may be called again."
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
                 (t `(progn ,@(cdr binding)))))

         (get-symbol (name)
           (cl:let ((series-package (find-package "SERIES")))
             (if series-package
                 (cl:let ((symbol (find-symbol name series-package)))
                   (if (and symbol
                            (fboundp symbol))
                       symbol
                       (find-symbol name (find-package "COMMON-LISP"))))
                 (find-symbol name (find-package "COMMON-LISP"))))))

    (cond ((or (null name-or-bindings)
               (consp name-or-bindings))
           `(,(get-symbol "LET")
             ,(map 'list (lambda (binding)
                          (list (binding-name binding) (binding-value binding)))
                   name-or-bindings)
             ,bindings-or-first
             ,@body))
          ((symbolp name-or-bindings)
           `(,(get-symbol "FUNCALL") (named-lambda ,name-or-bindings
                                        ,(map 'list #'binding-name
                                          bindings-or-first)
                                      ,@body)
                     ,@(map 'list #'binding-value
                            bindings-or-first)))
          (t (error "Bad syntax: LET")))))

(defmacro named-let (name bindings &body body)
  `(let ,name ,bindings ,@body))
