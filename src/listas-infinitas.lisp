;;; listas-infinitas.lisp
;;; Listas infinitas en Lisp.
;;; José A. Alonso Jiménez <https://jaalonso.github.io>
;;; Sevilla, 11-febrero-2025
;;; ====================================================================

;;; (retrasa e) retrasa la evaluación de la función e. Por ejemplo,
;;;    > (retrasa (+ 2 3))
;;;    #<FUNCTION (LAMBDA ()) {5363B46B}>
;;;    > (funcall (retrasa (+ 2 3)))
;;;    5
(defmacro retrasa (e)
  `(lambda () ,e))

;;; (fuerza er) fuerza la evaluación retrasada er. Por ejemplo,
;;;    > (fuerza (retrasa (+ 2 3)))
;;;    5
(defun fuerza (er)
  (funcall er))

;;; (cons-lp x y) crea una lista perezosa cuyo primer elemento es x
;;; y el resto es la expresión y retrasada. Por ejemplo,
;;;    > (setf ej-lp (cons-lp 1 (cons-lp 2 nil)))
;;;    (1 . #<FUNCTION (LAMBDA ()) {5368A37B}>)
;;;    > (funcall (cdr ej-lp))
;;;    (2 . #<FUNCTION (LAMBDA ()) {5368A3DB}>)
;;;    > (funcall (cdr (funcall (cdr ej-lp))))
;;;    NIL
(defmacro cons-lp (x y)
  `(cons ,x (retrasa ,y)))

;;; (primero s) es el primer elemento de la lista perezosa s. Por
;;; ejemplo,
;;;    > (primero (cons-lp 1 (cons-lp 2 nil)))
;;;    1
(defun primero (s)
  (car s))

;;; (resto s) es el resto de la lista perezosa s. Por ejmplo,
;;;    > (resto (cons-lp 1 (cons-lp 2 nil)))
;;;    (2 . #<FUNCTION (LAMBDA ()) {5363B00B}>)
(defun resto (s)
  (fuerza (cdr s)))

;;; (take n s) es la lista de los n primeros elementos de la lsta
;;; infinita s.
(defun take (n s)
  (if (zerop n)
      nil
      (cons (primero s)
            (take (1- n) (resto s)))))

;;; (enteros n) es la lista infinita de los números enteros comenzando
;;; desde n (por defecto 1). Por ejemplo,
;;;    > (take 10 (enteros))
;;;    (1 2 3 4 5 6 7 8 9 10)
(defun enteros (&optional (n 1))
  (cons-lp n (enteros (1+ n))))

;;; (fibs) es la lista de los números de Fibonacci. Por ejemplo,
;;;    > (take 10 (fibs))
;;;    (0 1 1 2 3 5 8 13 21 34)
(defun fibs (&optional (a 0) (b 1))
  (cons-lp a (fibs b (+ a b))))

;;; (factoriales) es la lista de los factoriales. Por ejemplo,
;;;    > (take 10 (factoriales))
;;;    (1 1 2 6 24 120 720 5040 40320 362880)
(defun factoriales (&optional (n 0) (f 1))
  (cons-lp f (factoriales (1+ n) (* (1+ n) f))))

;;; (take-while p s) es la lista de los primeros elementos de la lista
;;; infinita s que verifica el predicado p. Por ejemplo,
;;;    > (take-while (lambda (x) (< x 30)) (fibs))
;;;    (0 1 1 2 3 5 8 13 21)
(defun take-while (p s)
  (if (not (funcall p (primero s)))
      nil
      (cons (primero s)
            (take-while p (resto s)))))

;;; (divisiblep x y) se verifica si x es divisible por y. Por ejemplo,
;;;    > (divisiblep 6 3)
;;;    T
;;;    > (divisiblep 6 4)
;;;    NIL
(defun divisiblep (x y)
  (= (mod x y) 0))

;;; (filter p s) es la sublista infinita de s con los elementos que
;;; cumplen el predicado p. Por ejemplo,
;;;    > (take 5 (filter (lambda (x) (divisiblep x 3)) (enteros)))
;;;    (3 6 9 12 15)
(defun filter (p s)
  (if (funcall p (primero s))
      (cons-lp (primero s)
               (filter p (resto s)))
      (filter p (resto s))))

;;; (criba s) es la lista de números primos obtenidos cribando la lista
;;; infinita s. por ejemplo,
;;;    > (take 5 (criba (enteros 2)))
;;;    (2 3 5 7 11)
(defun criba (s)
  (let ((prime (primero s)))
    (cons-lp
      prime
      (criba
        (filter #'(lambda (x) (not (divisiblep x prime))) (resto s))))))

;;; (primos) es la lista infinita de los números primos. Por ejmplo,
;;;    > (take 10 (primos))
;;;    (2 3 5 7 11 13 17 19 23 29)
(defun primos ()
  (let ((s (enteros 2)))
    (criba s)))
