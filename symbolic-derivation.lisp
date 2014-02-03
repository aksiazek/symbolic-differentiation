(defmacro print (expr)
  `(quote ,expr))

(defmacro printa (&rest expr)
  `(quote ,expr))
  
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

(defun flatten (list)
  (cond
    ((null list) nil)
    ((atom list) (list list))
    (T (mapcan #'flatten list))
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
	     ((
	))
       (otherwise (if (is-named-function (car expr)) (named-function-derivative (car expr) (cdr expr)) 
       ))))

(defmacro new-cond (&rest cond-pairs)
  (cond      ;; you need cond to compile new-cond syntax, LOL!
    ((null cond-pairs) nil)
    ((atom cond-pairs) (error "new-cond: bad syntax!"))
    (t `(if ,(first (first cond-pairs))
           (progn ,@(rest (first cond-pairs)))
           (new-cond ,@(rest cond-pairs)))))) 

(defmacro derivative (var expression)
  `(let ((expr ',expression))
     (case (car expr)
       ('* (list '+ (list '* (derivative (quote ,var) (cadr expr)) (caddr expr))
		 (list '* (cadr expr) nil) ))
       (otherwise nil))))

(defun is-named-function (name)
  (if (member name '(^ sin cos tg ctg sqrt exp ln log asin acos atg actg)) T nil))

(defmacro named-function-derivative (name &rest arg)
  `(case (quote ,name) 
     ('sin '(cos ,@arg))
     ('cos '(- (sin ,@arg)))
     ('tg '(+ 1 (^ (tg ,@arg) 2)))
     ))
