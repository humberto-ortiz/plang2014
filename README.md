# CCOM 4029 - High Level Programming Languages - Spring 2014

Example programs for CCOM 4029.

## Conditional statement example.

Here's some sample code for an interpreter (and parser) to handle
simple conditional statements.

The parser defines a language construct called `if0`

```
(if0 {test-expression}
      {then-expression}
	  {else-expression})
```

The `if0` evaluates the test-expression, and if the result is
different from 0, it evaluates the then-expression, if the result of
the test-expression is 0, the else-expression is evaluated.

For example, the following code should evaluate to 436.

```
(if0 (* (+ 0 1) (- 2 3))
      (+ 400 (* 6 6))
	  112)
```

I have provided the parser, the core datatype, and part of the
interpreter in the file `exprc-if.arr`, but the interpreter is broken.

Your task is to fix the interpeter.
