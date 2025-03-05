# named-let
Named lambda and named let macros.

## Installation

 - Clone this repository into `~/quicklisp/local-projects/`

 - (ql:quickload "named-let")

## Use

In your defpackage forms, add `(:shadowing-import-from "NAMED-LET" "LET")`

---

A `named-lambda` is a lambda expression that has a recursive binding for itself that is visible within the lambda body.  This allows the lambda to recursively call itself.  For example, here we map an anonymous factorial program over a list:

```
(map 'list (named-lambda fact (x)
             (if (zerop x)
                 1
                 (* x (fact (1- x)))))
           '(7 11 2))

(5040 39916800 2)
```

A `named-let` is a let expression that has an optional name for the lambda expression that performs the let binding.  This allows you to recursively invoke the let expression with new values for the bindings.

```
(named-let recur ((items (some-items))
                  (passing '())
                  (failing '()))
  (cond ((consp items) (if (predicate? (car items))
                           (recur (cdr items) (cons (car items) passing) failing)
                           (recur (cdr items) passing (cons (car items) failing))))
        ((null items) (values passing failing))
        (t (error "Dotted tail on items."))))
```

This package also rebinds `let`, so you can overload the standard `let` syntax:
```
(let foo ((x 22)
          (y 13))
  (if (zerop x)
      y
      (foo (- x 1) (+ y 1))))
```
If you wish, you can use a shadowing import on this symbol:

```
(defpackage "MY-PACKAGE"
  (:shadowing-import-from "NAMED-LET" "LET")
  (:use "COMMON-LISP" "NAMED-LET"))
```

If you are a `series` *aficionado*, you can use this `let` instead of `series::let`:

```
(defpackage "MY-PACKAGE"
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES" "DEFUN" "FUNCALL" "LET*" "MULTIPLE-VALUE-BIND")
  (:use "COMMON-LISP" "NAMED-LET" "SERIES"))
```
