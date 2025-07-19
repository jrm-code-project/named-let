# named-let
Named lambda and named let macros.

## Overview

This is a small Common Lisp library that provides a collection of
Scheme-inspired binding constructs. It offers macros like `named-let`,
`letrec`, `letrec*`, and a Lisp-1 style `define` to allow for more
expressive and convenient ways to handle local recursion and
definitions.

The main goal is to bring some of the syntactic sugar and binding
paradigms from Scheme into Common Lisp, making it easier to write
certain recursive or self-referential constructs.

- **`named-lambda`**: A lambda expression with a recursive binding for itself, enabling self-referential calls.
- **`named-let`**: An enhanced `let` expression that allows recursive invocation with updated bindings.

These macros simplify recursive programming and enable concise, expressive code.

---

## Installation

1. Clone this repository into your Quicklisp local projects directory:

   ```bash
   git clone https://github.com/your-repo/named-let.git ~/quicklisp/local-projects/
   ```

2. Load the package using Quicklisp:

   ```lisp
   (ql:quickload "named-let")
   ```

---

## Usage

### Importing the Package

To use the macros, add the following to your `defpackage` form:

```lisp
(defpackage "MY-PACKAGE"
  (:shadowing-import-from "NAMED-LET" "LET")
  (:use "COMMON-LISP" "NAMED-LET"))
```

---
### `define`
`define` is a macro that allows you to define local functions in a
Lisp-1 style, similar to Scheme's `define`. It can be used to create
local functions that can refer to themselves recursively.

Caveat:  This macro is not a replacement for `defun` and will only
work at top-level.

```lisp
(define (fact x)
  (if (zerop x)
      1
      (* x (fact (1- x)))))
      
(fact 5) ; Output: 120

```

---
### `letrec` and `letrec*`
`letrec` and `letrec*` are macros that allow you to define local
recursive functions. They are similar to the `let` construct but
enable the functions to refer to themselves recursively.

`letrec` performs a simultaneous binding of all variables, while
`letrec*` allows for sequential binding, where each variable can use
previous bindings during its definition.

```lisp
(letrec ((fact (lambda (x)
                 (if (zerop x)
                     1
                     (* x (fact (1- x))))))
         (fib (lambda (n)
                 (if (< n 2)
                     n
                     (+ (fib (- n 1)) (fib (- n 2))))))
         (sum (lambda (lst)
                 (if (null lst)
                     0
                     (+ (car lst) (sum (cdr lst)))))))
  (list (fact 5)   ; Output: 120
        (fib 10)    ; Output: 55
        (sum '(1 2 3 4 5)))) ; Output: 15

;; Output: (120 55 15)
```

---

### `named-lambda`

A `named-lambda` is a lambda expression with a recursive binding for itself. This allows the lambda to call itself recursively. Here's an example:

```lisp
(map 'list (named-lambda fact (x)
             (if (zerop x)
                 1
                 (* x (fact (1- x)))))
           '(7 11 2))

;; Output: (5040 39916800 2)
```

---

### `named-let`

A `named-let` is a `let` expression with an optional name for the lambda expression that performs the let binding. This enables recursive invocation with updated bindings.

Example: Partitioning a list into passing and failing items based on a predicate:

```lisp
(named-let recur ((items '(1 2 3 4 5))
                  (passing '())
                  (failing '()))
  (cond ((consp items) (if (evenp (car items))
                           (recur (cdr items) (cons (car items) passing) failing)
                           (recur (cdr items) passing (cons (car items) failing))))
        ((null items) (values passing failing))
        (t (error "Dotted tail on items."))))

;; Output: Passing: (4 2), Failing: (5 3 1)
```

---

### Overloading `let`

This package redefines `let` to support recursive invocation. For example:

```lisp
(let foo ((x 22)
          (y 13))
  (if (zerop x)
      y
      (foo (- x 1) (+ y 1))))

;; Output: 35
```

If you prefer, you can shadow the standard `let` syntax in your package:

```lisp
(defpackage "MY-PACKAGE"
  (:shadowing-import-from "NAMED-LET" "LET")
  (:use "COMMON-LISP" "NAMED-LET"))
```

---

### Integration with `series`

If you are an *aficionado* of the `series` library, you can integrate `named-let` with `series` by shadowing additional symbols:

```lisp
(defpackage "MY-PACKAGE"
  (:shadowing-import-from "NAMED-LET" "LET")
  (:shadowing-import-from "SERIES" "DEFUN" "FUNCALL" "LET*" "MULTIPLE-VALUE-BIND")
  (:use "COMMON-LISP" "NAMED-LET" "SERIES"))
```

`named-let` will use the series definition of `let` if the package is loaded.

---

## Additional Features

- **Error Handling**: The macros include error handling for invalid inputs, such as dotted lists in `named-let`.

---

## Contributing

Contributions are welcome! Please submit issues or pull requests on the [GitHub repository](https://github.com/your-repo/named-let).

---

## License

This project is licensed under the MIT License. See the `LICENSE` file for details.
