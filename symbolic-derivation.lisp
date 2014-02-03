(defmacro print (&rest expr)
  `(quote ,expr))

(defmacro derivative (var &rest expr)
  `(quote ,expr))

(defun is-named-function (name)
  (if (member name '(sin cos tg ctg sqrt exp ln log asin acos atg actg)) T nil))

(defmacro named-function-derivative (name &rest args)
  `(case (quote ,name) ('sin '(cos (quote ,args))) ))
