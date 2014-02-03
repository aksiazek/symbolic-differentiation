#|
Aleksander Ksiazek, Obliczenia Symboliczne II, 2014
Developed and Tested using Emacs with SLIME and SBCL 1.1
|#
(defun flatten (list)
  (cond
    ((null list) nil)
    ((atom list) (list list))
    (T (mapcan #'flatten list))
    ))

(defun not-in (var expr)
  (not (member var (flatten expr))))

(defmacro derivative (var expr)
  `(derive (quote ,var) (quote ,expr)))

(defun is-named-function (name)
  (if (member name '(^ sin cos tg ctg sqrt exp ln log asin acos atg actg)) T nil))

(defun named-function-derivative (name arg)
  (cond
    ((eq name 'sin) `(cos ,(rewrite arg)))
    ((eq name 'cos) `(- (sin ,(rewrite arg))))
    ((eq name 'tg) `(+ 1 (^ (tg ,(rewrite arg)) 2)))
    ((eq name 'ctg) `(- (+ 1 (^ (ctg ,(rewrite arg)) 2))))
    ((eq name 'sqrt) `(/ 1 (* 2 (sqrt ,(rewrite arg)))))
    ((eq name 'exp) (rewrite `(exp ,(rewrite arg))))
    ((eq name 'ln) `(/ 1 ,(rewrite arg)))
    ((eq name 'log) `(/ 1 (* ,(rewrite arg) (ln 10))))
    ((eq name 'asin) `(/ 1 (sqrt (- 1 (^ ,(rewrite arg) 2)))))
    ((eq name 'acos) `(- (/ 1 (sqrt (- 1 (^ ,(rewrite arg) 2))))))
    ((eq name 'atg) `(/ 1 (+ 1 (^ ,(rewrite arg) 2))))
    ((eq name 'actg) `(- (/ 1 (+ 1 (^ ,(rewrite arg) 2)))))
    ))

(defun rewrite (expr)
; reduce redundant operations
  (if (atom expr) expr
      (let ((operator (car expr)) (arg1 (cadr expr)) (arg2 (caddr expr)))
	(cond
	  ((eq operator '+)
	   (cond
	     ((null arg2) (rewrite arg1)) ; (+ arg1)
	     ((eq arg2 0) arg1) ; +0
	     ((eq arg1 0) arg2) ; 0+
	     (T expr)
	     ))
	  ((eq operator '-)
	   (cond
	     ((and (eq arg1 0) (null arg2)) 0) ; (-0)
	     ((and (null arg2) (listp arg1) (eq (car arg1) '-)) (rewrite (cadr arg1))) ; -(- arg1)
	     ((eq arg1 0) (list '- arg2)) ; 0 - arg2
	     ((eq arg2 0) arg1) ; arg1 - 0
	     (T expr)
	     ))
	  ((eq operator '*)
	   (cond
	     ((or (eq arg2 0) (eq arg1 0)) 0) ; 0*
	     ((eq arg2 1) arg1) ; 1*
	     ((eq arg1 1) arg2) ; *1
	     (T expr)
	     ))
	  (T expr)
	  ))))     

; already quoted  
(defun derive (var expr)
  (cond
    ((null expr) nil)
    ((and (atom expr) (eq var expr)) 1)
    ((not (member var (flatten expr))) 0)
    (T (let ((operator (car expr)) (arg1 (cadr expr)) (arg2 (caddr expr)))
	 (cond
	   ((member operator '(+ -)) 
	    (if (null arg2) (rewrite (list operator (derive var arg1)))
		(rewrite (list operator (derive var arg1) (derive var arg2)))))
	   ((eq operator '*)
	    (rewrite (list '+
			   (rewrite (list '* (derive var arg1) arg2))
			   (rewrite (list '* arg1 (derive var arg2)))
			   )))
	   ((eq operator '/)
	    (rewrite (list '/ 
			   (rewrite (list '- 
					  (rewrite (list '* (derive var arg1) arg2))
					  (rewrite (list '* arg1 (derive var arg2)))
					  ))
			   (rewrite (list '^ arg2 2)))))
	   
	   ((eq operator '^) 
	    (cond
	; a^f(x)
	      ((not (member var (flatten arg1)))
	       (rewrite (list '* 
			      (rewrite (list '* 
					     (rewrite (list '^ arg1 arg2))
					     `(ln ,arg1)))
			      (derive var arg2))))
	; f(x) ^ a
	      ((not (member var (flatten arg2)))
	       (rewrite (list '* 
			      (rewrite (list '* arg2
					     (rewrite (list '^ arg1 
							    (rewrite (list '- arg2 '1))))))
			      (derive var arg1))))
	; f(x) ^ g(x)
	      (T (derive var (list 'exp (rewrite (list '* arg2 (list 'ln arg1))))))
	      ))
	   (T 
	    (if (is-named-function operator) 
		(rewrite (list '* (named-function-derivative operator arg1) (derive var arg1)))
		(error "unknown function")))
	   )))))

