(defun flatten (list)
  (cond
    ((null list) nil)
    ((atom list) (list list))
    (T (mapcan #'flatten list))
    ))

(defmacro derivative (var expr)
  `(der (quote ,var) (quote ,expr)))

(defun rewrite (expr)
  ; reduce redundant operations
  (cond
    ((atom expr) expr)
    ((eq (car expr) '+)
     (cond
       ((eq (caddr expr) 0) (cadr expr)) ; +0
       ((eq (cadr expr) 0) (caddr expr)) ; 0+
       (T expr)
       ))
    ((eq (car expr) '-)
     (cond
       ((and (eq (cadr expr) 0) (null (caddr expr))) 0) ; (-0)
       ((eq (cadr expr) 0) (list '- (caddr expr))) ; 0-
       ((eq (caddr expr) 0) (cadr expr)) ; -0
       ;((and (listp (cadr expr)) (eq (caadr expr) '-)) expr) ; -(-x)
       ; +(-x)
       (T expr)
       ))
    ((eq (car expr) '*)
     (cond
       ((or (eq (caddr expr) 0) (eq (cadr expr) 0)) 0) ; 0*
       ((eq (caddr expr) 1) (cadr expr)) ; 1*
       ((eq (cadr expr) 1) (caddr expr)) ; *1
       (T expr)
       ))
    (T expr)
    ))     

; already quoted  
(defun der (var expr)
  (cond
    ((null expr) nil)
    ((and (atom expr) (eq var expr)) 1)
    ((not (member var (flatten expr))) 0)
    (T
     (case (car expr)
       ('+ (rewrite (list '+ (der var (cadr expr)) (der var (caddr expr)))))
       ('- (rewrite (list '- (der var (cadr expr)) (der var (caddr expr)))))
       ('* (rewrite (list '+
			  (rewrite (list '* (der var (cadr expr)) (caddr expr)))
			  (rewrite (list '* (cadr expr) (der var (caddr expr))))
			  )))
       ('/ (rewrite (list '/ 
		 (rewrite (list '- 
				(rewrite (list '* (der var (cadr expr)) (caddr expr)))
				(rewrite (list '* (cadr expr) (der var (caddr expr))))
				))
		 (rewrite (list '^ (caddr expr) 2)))))
      
       ('^ (cond
	     ; a^f(x)
	     ((not (member var (flatten (cadr expr))))
	      (rewrite (list '* 
			     (rewrite (list '* 
					    (rewrite (list '^ (cadr expr) (caddr expr)))
					    `(ln ,(cadr expr))))
			     (der var (caddr expr)))))
	     ; f(x) ^ a
	     ((not (member var (flatten (caddr expr))))
	      (rewrite (list '* 
			     (rewrite (list '* (caddr expr)
					    (rewrite (list '^ (cadr expr) 
							   (rewrite (list '- (caddr expr) '1))))))
			     (der var (cadr expr)))))
	     ; f(x) ^ g(x)
	     (T (der var (list 'exp (rewrite (list '* (caddr expr) (list 'ln (cadr expr)))))))
	     ))
       (otherwise 
	(if (is-named-function (car expr)) 
	    (rewrite (list '* (named-function-derivative (car expr) (cadr expr)) (der var (cadr expr))))
	    (error "unknown function")))
       ))))

(defun is-named-function (name)
  (if (member name '(^ sin cos tg ctg sqrt exp ln log asin acos atg actg)) T nil))

(defun named-function-derivative (name &rest arg)
  (case name
    ('sin `(cos ,@arg))
    ('cos `(- (sin ,@arg)))
    ('tg `(+ 1 (^ (tg ,@arg) 2)))
    ('ctg `(- (+ 1 (^ (ctg ,@arg) 2))))
    ('sqrt `(/ 1 (* 2 (sqrt ,@arg))))
    ('exp `(exp ,@arg))
    ('ln `(/ 1 ,@arg))
    ('log `(/ 1 (* ,@arg (ln 10))))
    ('asin `(/ 1 (sqrt (- 1 (^ ,@arg 2)))))
    ('acos `(- (/ 1 (sqrt (- 1 (^ ,@arg 2))))))
    ('atg `(/ 1 (+ 1 (^ ,@arg 2))))
    ('actg `(- (/ 1 (+ 1 (^ ,@arg 2)))))
    ))
