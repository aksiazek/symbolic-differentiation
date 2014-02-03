(defmacro print (&rest expr)
  `(quote ,expr))

(defmacro derivative (var &rest expression)
  `(let ((expr ',@expression))
     (case (car expr)
       ('* (list '+ (list '* (derivative (quote ,var) (cadr expr)) (cddr expr))
		 (list '* (cadr expr) (derivative (quote ,var) (cddr expr))) ))
       )))


(defun is-named-function (name)
  (if (member name '(sin cos tg ctg sqrt exp ln log asin acos atg actg)) T nil))

(defmacro named-function-derivative (name &rest arg)
  `(case (quote ,name) 
     ('sin '(cos ,@arg))
     ('cos '(- (sin ,@arg)))
     ('tg '(+ 1 (^ (tg ,@arg) 2)))
     ))
