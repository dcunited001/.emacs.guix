
(defgroup metaprog
  "test metaprogramming with types")
(defcustom i-ching-svg-test-sexp nil
  "Defaults to use when generating the image"
  :type '(restricted-sexp :match-alternatives
                          (integerp 'nil)))
(defcustom i-ching-svg-sexp nil
  "Defaults to use when generating the image"
  :type 'sexp)

(funcall (symbol-function 'dc/tab-next))
(function-get 'fdsa)
(fset 'fdsa i-ching-svg-sexp)

(setq fdsa '(+ 1 1))
(setf fdsa `(closure (t) nil "fdsa" nil ,fdsa))
(funcall fdsa)

(cons 1 '(1 2 3))
(nconc '(1) '(1 2 3))
(setq foolist '(1 2 3))
(nconc '(a b c) foolist)
(nconc foolist '(a b c))
