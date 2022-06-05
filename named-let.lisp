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
  (let ((vars (map 'list (lambda (arg)
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

(defmacro named-let (name bindings &body body)
  (flet ((binding-name (binding)
           (cond ((symbolp binding) binding)
                 ((not (consp binding)) (error "Bad synax: NAMED-LET"))
                 ((symbolp (car binding)) (car binding))
                 (t (error "Bad syntax: NAMED-LET"))))

         (binding-value (binding)
           (cond ((symbolp binding) 'nil)
                 ((not (consp binding)) (error "Bad syntax: NAMED-LET"))
                 ((null (cdr binding)) 'nil)
                 ((not (consp (cdr binding))) (error "Bad syntax: NAMED-LET"))
                 ((null (cddr binding)) (cadr binding))
                 (t `(progn ,@(cdr binding))))))

    `(funcall (named-lambda ,name ,(map 'list #'binding-name bindings)
                ,@body)
              ,@(map 'list #'binding-value
                     bindings))))
