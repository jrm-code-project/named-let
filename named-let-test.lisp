;;; -*- Lisp -*-

(defpackage "NAMED-LET-TEST"
  (:use "COMMON-LISP" "NAMED-LET" "FIVEAM")
  (:shadowing-import-from "NAMED-LET" "LET")
  (:export "RUN-TESTS"))

(in-package "NAMED-LET-TEST")

(def-suite named-let-suite :description "Tests for the NAMED-LET library.")
(in-suite named-let-suite)

(defun run-tests ()
  (run! 'named-let-suite))

;;; LET (extended)

(test let-plain-bindings
  (is (= 3 (let ((x 1) (y 2)) (+ x y))))
  (is (eq nil (let () nil)))
  (is (equal '(1 nil nil) (let ((x 1) y (z)) (list x y z)))))

(test let-named
  (is (= 120 (let fact ((n 5) (acc 1))
               (if (zerop n)
                   acc
                   (fact (1- n) (* n acc))))))
  (is (= 10 (let iter ((i 0))
              (if (< i 10) (iter (1+ i)) i)))))

(test let-named-no-bindings
  (is (= 42 (let thunk () 42))))

(test let-named-evaluates-inits-outside-name
  (is (= 2 (cl:let ((n 1))
             (let n ((m (1+ n))) m)))))

;;; NAMED-LET

(test named-let-basic
  (is (equal '(3 2 1)
             (named-let rev ((in '(1 2 3)) (out '()))
               (if (null in)
                   out
                   (rev (cdr in) (cons (car in) out)))))))

(test named-let-deep-recursion
  (is (= 100000 (named-let tally ((i 0))
                  (if (< i 100000) (tally (1+ i)) i)))))

;;; NAMED-LAMBDA

(test named-lambda-recursion
  (let ((f (named-lambda fib (n)
             (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))))
    (is (functionp f))
    (is (= 55 (funcall f 10)))))

;;; LETREC / LETREC*

(test letrec-mutual-recursion
  (is (eq t (letrec ((even? (lambda (x) (or (zerop x) (odd? (1- x)))))
                     (odd? (lambda (x) (and (not (zerop x)) (even? (1- x))))))
              (even? 22))))
  (is (eq nil (letrec ((even? (lambda (x) (or (zerop x) (odd? (1- x)))))
                       (odd? (lambda (x) (and (not (zerop x)) (even? (1- x))))))
                (even? 7)))))

(test letrec*-sequential
  (is (= 6 (letrec* ((a (lambda () 1))
                     (b (lambda () (+ (a) 2)))
                     (c (lambda () (+ (a) (b) 2))))
             (c))))
  (is (eq t (letrec* ((even? (lambda (x) (or (zerop x) (odd? (1- x)))))
                      (odd? (lambda (x) (and (not (zerop x)) (even? (1- x))))))
              (even? 10)))))

(test letrec-values-are-funcallable
  (is (= 3 (letrec ((f (lambda (x) (1+ x))))
             (funcall f 2)))))

;;; FLAMBDA

(test flambda-binds-function-namespace
  (is (= 7 (funcall (flambda (f) (f 3 4)) #'+)))
  (is (equal '(1 2) (funcall (flambda (f) (f 1 2)) #'list))))

;;; DEFINE

(define (add-42 x) "Add x and 42." (+ x 42))
(define twice (lambda (f x) (funcall f (funcall f x))))
(define (fact-define n) (if (zerop n) 1 (* n (fact-define (1- n)))))

(test define-function-syntax
  (is (= 43 (add-42 1)))
  (is (= 42 (add-42 0))))

(test define-value-syntax
  (is (= 4 (twice #'1+ 2))))

(test define-recursive
  (is (= 120 (fact-define 5))))

;;; Syntax errors

(test let-bad-syntax
  (signals error (macroexpand-1 '(named-let::let ((1 2)) 3)))
  (signals error (macroexpand-1 '(named-let::let 5 3))))
