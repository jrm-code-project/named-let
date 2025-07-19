;;; -*- Lisp -*-

(defpackage "NAMED-LET"
  (:shadow "LET")
  (:use "COMMON-LISP")
  (:export
   "DEFINE"
   "FLAMBDA"
   ;; "LET"
   "LETREC"
   "LETREC*"
   "NAMED-LAMBDA"
   "NAMED-LET"))

(in-package "NAMED-LET")

;; Macros are aware of the SERIES package and will expand to calls
;; that use the SERIES definitions if it is present.  If not, they
;; will expand to calls that use the COMMON-LISP definitions.

(defmacro define (name &body body)
  "A Scheme-like DEFINE macro that binds name globally in the function and value namespace."
  (flet ((get-symbol (name)
           (cl:let ((series-package (find-package "SERIES")))
             (if series-package
                 (cl:let ((symbol (find-symbol name series-package)))
                   (if (and symbol
                            (fboundp symbol))
                       symbol
                       (find-symbol name (find-package "COMMON-LISP"))))
                 (find-symbol name (find-package "COMMON-LISP"))))))

    (cond ((consp name)
           `(DEFINE ,(car name) (LAMBDA ,(cdr name) ,@body)))
          ((stringp (car body))
           `(PROGN
              (DEFMACRO ,name (&REST ARGS)
                ,(car body)
                `(,',(get-symbol "FUNCALL") (SYMBOL-VALUE ',',name) ,@args))
              (SETF (SYMBOL-VALUE ',name) (PROGN ,@(cdr body)))))
          (t
           `(PROGN
              (DEFMACRO ,name (&REST ARGS)
                `(,',(get-symbol "FUNCALL") (SYMBOL-VALUE ',',name) ,@args))
              (SETF (SYMBOL-VALUE ',name) (PROGN ,@body)))))))

#||
(define (foo x) "Add x and 42." (+ x 42))

(progn
 (progn
  (eval-when (:compile-toplevel)
    (sb-c::%compiler-defmacro :macro-function 'foo))
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (sb-c::%defmacro 'foo
                     (sb-int:named-lambda (macro-function foo)
                         (#:expr #:env)
                       (declare (sb-c::lambda-list (&rest args)))
                       (declare (ignore #:env))
                       (let* ((#:g829 (cdr #:expr)) (args #:g829))
                         (declare (sb-c::constant-value args))
                         (block foo
                           `(,'series::funcall (symbol-value ','foo) ,@args))))
                     (sb-c:source-location))))
 (set 'foo (progn (lambda (x) (+ x 42)))))
||#

(defmacro flambda ((&rest arglist) &body body)
  "A lambda that binds its arguments in the function namespace."
  (cl:let ((vars (map 'list (lambda (arg)
                              (gensym (concatenate 'string (symbol-name arg) "-")))
                      arglist)))
    `(LAMBDA ,vars
       (FLET ,(map 'list (lambda (arg var)
                           `(,arg (&REST ARGS)
                              (APPLY ,var ARGS)))
                   arglist
                   vars)
         ,@body))))

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
                 (t `(PROGN ,@(cdr binding)))))

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
           `(,(get-symbol "FUNCALL") (NAMED-LAMBDA ,name-or-bindings
                                        ,(map 'list #'binding-name
                                          bindings-or-first)
                                      ,@body)
                     ,@(map 'list #'binding-value
                            bindings-or-first)))
          (t (error "Bad syntax: LET")))))

(defmacro letrec ((&rest bindings) &body body)
  "Variant of LET that binds the variables recursively, both in the function and value namespaces."
  (flet ((get-symbol (name)
           (cl:let ((series-package (find-package "SERIES")))
             (if series-package
                 (cl:let ((symbol (find-symbol name series-package)))
                   (if (and symbol
                            (fboundp symbol))
                       symbol
                       (find-symbol name (find-package "COMMON-LISP"))))
                 (find-symbol name (find-package "COMMON-LISP"))))))

    `(,(get-symbol "LET") (,@(map 'list #'car bindings))
       (MACROLET (,@(map 'list (lambda (binding)
                                 `(,(car binding) (&REST ARGS)
                                   `(,',(get-symbol "FUNCALL") ,',(car binding) ,@ARGS)))
                      bindings))
         (PSETQ ,@(apply #'append
                         (map 'list (lambda (binding)
                                      (list (car binding)
                                            (cons 'PROGN (cdr binding))))
                              bindings)))
         ,@body))))

#||
(letrec ((even? (lambda (x)
                  (or (zerop x)
                      (odd? (1- x)))))
         (odd? (lambda (x)
                 (and (not (zerop x))
                      (even? (1- x))))))
  (even? 22))

(common-lisp:let (even? odd?)
  (macrolet ((even? (&rest args)
               `(,'series::funcall ,'even? ,@args))
             (odd? (&rest args)
               `(,'series::funcall ,'odd? ,@args)))
    (let* ((#:new832
            (progn
             (lambda (x)
               (common-lisp:let ((#:g834 (zerop x)))
                 (if #:g834
                     #:g834
                     (funcall odd? (1- x)))))))
           (#:new833
            (progn
             (lambda (x)
               (if (not (zerop x))
                   (funcall even? (1- x)))))))
      (setq even? #:new832)
      (setq odd? #:new833)
      nil)
    (funcall even? 22)))
||#

(defmacro letrec* ((&rest bindings) &body body)
  "Variant of LET that binds the variables recursively, both in the function and value namespaces."
  (flet ((get-symbol (name)
           (cl:let ((series-package (find-package "SERIES")))
             (if series-package
                 (cl:let ((symbol (find-symbol name series-package)))
                   (if (and symbol
                            (fboundp symbol))
                       symbol
                       (find-symbol name (find-package "COMMON-LISP"))))
                 (find-symbol name (find-package "COMMON-LISP"))))))

    `(,(get-symbol "LET") (,@(map 'list #'car bindings))
       (MACROLET (,@(map 'list (lambda (binding)
                                 `(,(car binding) (&REST ARGS)
                                   `(,',(get-symbol "FUNCALL") ,',(car binding) ,@ARGS)))
                      bindings))
         (SETQ ,@(apply #'append
                        (map 'list (lambda (binding)
                                     (list (car binding)
                                           (cons 'PROGN (cdr binding))))
                             bindings)))
         ,@body))))

#||
(letrec* ((even? (lambda (x)
                   (or (zerop x)
                       (odd? (1- x)))))
          (odd? (lambda (x)
                  (and (not (zerop x))
                       (even? (1- x))))))
  (even? 22))

(common-lisp:let (even? odd?)
  (macrolet ((even? (&rest args)
               `(,'series::funcall ,'even? ,@args))
             (odd? (&rest args)
               `(,'series::funcall ,'odd? ,@args)))
    (progn
     (setq even?
             (progn
              (lambda (x)
                (common-lisp:let ((#:g836 (zerop x)))
                  (if #:g836
                      #:g836
                      (funcall odd? (1- x)))))))
     (setq odd?
             (progn
              (lambda (x)
                (if (not (zerop x))
                    (funcall even? (1- x)))))))
    (funcall even? 22)))
||#

(defmacro named-lambda (name (&rest arglist) &body body)
  `(LABELS ((,name ,arglist ,@body))
     #',name))

(defmacro named-let (name bindings &body body)
  `(LET ,name ,bindings ,@body))
