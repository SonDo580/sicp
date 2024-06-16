## Example

```scheme
(define (make-withdraw balance)
    (lambda (amount)
            (if (>= balance amount)
                    (begin (set! balance (- balance amount))
                           balance)
                "Insufficient funds")))
```

=> Describe the evaluation of

```scheme
(define W1 (make-withdraw 100))
(W1 50)
```

## Analysis

1. Define `make-withdraw`

- Create a procedure object that contains a pointer to the global environment (the body of the procedure is also a lambda-expression).

2. Apply `make-withdraw`

```scheme
(define W1 (make-withdraw 100))
```

- Setup an environment `E1` in which `balance` (parameter of `make-withdraw`) is bound to 100.
- In `E1`, evaluate the body of `make-withdraw`. This constructs a new procedure object, whose code is specified by the `lambda` and whose environment is `E1` - the environment in which the `lambda` was evaluated to produce the procedure.
- The new procedure object is bound to `W1` in the global environment, since the `define` itself is being evaluated in the global environment.

3. Apply `W1` to an argument

```scheme
(W1 50) ; 50
```

- Construct a frame in which `amount` (parameter of `W1`) is bound to the argument 50. This frame has `E1` as its enclosing environment, because this is the environment specified by the `W1` procedure object.
- Within the new environment, evaluate the body of the procedure `W1`

```scheme
(if (>= balance amount)
    (begin (set! balance (- balance amount))
            balance)
    "Insufficient funds")
```

`amount` will be found in the first frame of the environment, while `balance` will be found in `E1`

- When `set!` is executed, the binding of `balance` in `E1` in changed.
- At the completion of the call to `W1`, `balance` is 50, and the frame that contains `balance` is still pointed to by the procedure object `W1`. - The frame that binds `amount` is no longer relevant, since the procedure call that constructed it has terminated, and there are no pointers to that frame from other parts of the environment.
- The next time `W1` is called, this will build a new frame that binds `amount` and whose enclosing environment is `E1`.

**Conclusion:**

- `E1` serves as that "place" that holds the local state variable for the procedure object `W1`.

4. Create other "withdraw" objects

```scheme
(define W2 (make-withdraw 100))
```

- `W1` and `W2` have the same code (specified by the lambda-expression in the body of `make-withdraw`).
- But they behave as independent objects, with local state stored in different environments (`E1` and `E2`).