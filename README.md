# named-let
Named lambda and named let macros.

A `named-lambda` is a lambda expression that has a recursive binding for itself that is visible within the lambda body.  This allows the lambda to recursively call itself.  For example, here we map an anonymous factorial program over a list:

```
(map 'list (named-lambda fact (x)
             (if (zerop x)
                 1
                 (* x (fact (1- x)))))
           '(7 11 2))

(5040 39916800 2)
```

A `named-let` is a let expression that has a name for the lambda expression that performs the let binding.  This allows you to recursively invoke the let expression with new values for the bindings.

```
(let recur ((items (some-items))
            (passing '())
            (failing '()))
  (cond ((consp items) (if (predicate? (car items))
                           (recur (cdr items) (cons (car items) passing) failing)
                           (recur (cdr items) passing (cons (car items) failing))))
        ((null items) (values passing failing))
        (t (error "Dotted tail on items."))))
```
