## To evaluate a combination

- Evaluate the sub-expressions.
- Apply the value of the operator sub-expression to the values of the operand expressions.

## Procedure in environment model

- A pair consisting of "some code" and a pointer to an environment.
- Created by evaluating a lambda-expression.

## Define a procedure

```scheme
(define (square x)
    (* x x))
```

- The procedure definition syntax is just syntactic sugar for an underlying implicit lambda-expression.

```
(define square
    (lambda (x) (* x x)))
```

=> Evaluate `(lambda (x) (* x x))` and binds `square` to the resulting value in the global environment.

=> The procedure object: procedure code and a pointer to the global environment.

## Apply a procedure

- Create a new environment containing a frame that binds the parameters to the values of the arguments. The enclosing environment of this frame is the environment specified by the procedure.
- Within the new environment, evaluate the procedure body.

**Example**: Evaluating `(square 5)`

- Create a new environment E1, that begins with a frame in which x (the parameter) is bound to the argument 5. The frame's enclosing environment is the global environment (indicated by the `square` procedure object).
- Within E1, evaluate the body of the procedure `(* x x)`. The result is `(* 5 5)`, or `25`.

## Summary

1. **Create procedure**

- By evaluating a lambda-expression relative to a given environment.
- The resulting procedure object is a pair consisting of the text of the lambda-expression and a pointer to the environment in which the procedure was created.

2. **Apply procedure**

- By constructing a frame, binding the formal parameters of the procedure to the arguments of the call, then evaluate the body of the procedure in the context of the new environment constructed.
- The new frame has its enclosing environment the environment part of the procedure object.

3. `define` behavior
- Create a binding in the current environment frame and assign value to the symbol.
- If there is already a binding for the variable in the current frame, overwrite the binding (allow re-definitions of symbols).

4. `set!` behavior
- Locate the binding of the variable in the environment (first frame that contains the binding).
- Change the binding to the new value.
- If the variable is unbound in the environment, signals an error.
