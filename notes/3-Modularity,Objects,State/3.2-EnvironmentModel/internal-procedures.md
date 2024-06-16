## Example

```clojure
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
```

## Analysis

1. Define `sqrt`

- Create a procedure object whose associated environment is the global environment.

2. Call `(sqrt 2)`
- Create a new environment `E1`, subordinate to the global environment, in which `x` (parameter of `sqrt`) is bound to 2.
- In `E1`, evaluate the body of `sqrt` => `good-enough?`, `improve`, `square-iter` are defined as procedures in `E1`.
- After the local procedures are defined, evaluate `(square-iter 1.0)`
- Create a new environment `E2` in which `guess` (parameter of `square-iter`) is bound to 1
- `square-iter` in turn call `good-enough?` with the value of `guess` from `E2` as the argument.
- Create another environment `E3`, in which `guess` (parameter of `good-enough?`) is bound to 1
- Note that `E2` and `E3` both have `E1` as their enclosing environment => The symbol `x` in `good-enough?` will reference the binding of `x` in `E1`.
