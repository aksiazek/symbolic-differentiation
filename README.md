symbolic-differentiation
========================

Symbolic differentiation in Common Lisp

to use the program in any Common Lisp REPL:

  (load "symbolic-derivation.lisp")
  
you can now run the function derivative like so:
  
  (derivative x (sin (* 2 x))) ; this prints (* cos (* 2 x) 2) which we expected from d/dx
  
derivative is actually only a macro to avoid having to quote every time, which yields the same result as:
  
  (derive 'x '(sin (*2 x)))
  

An additional instruction for compiling this into a executable (using SBCL) is provided in the source
  
  
