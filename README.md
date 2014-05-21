symbolic-differentiation
========================

Symbolic differentiation in Common Lisp

to use the program in any Common Lisp REPL:

  (load "symbolic-derivation.lisp")
  
you can now run the function derivative, which needs your function expression to be in s-expr format with at most two arguments in any operation.
  
Example:

  (derivative x (sin (* 2 x))) ; this prints (* cos (* 2 x) 2) which we expected from d/dx

Besides the usual +,-,*,/ operators and numbers, any of the following functions is evaluted too: (sin cos tg ctg sqrt exp ln log asin acos atg actg), as well is the symbol ^, which is an alias for the power function 


An additional instruction for compiling this into a executable (using SBCL) is provided in the source
  
  
