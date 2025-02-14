;;; primos-equidistantes.lisp
;;; Primos equidistantes.
;;; José A. Alonso Jiménez <https://jaalonso.github.io>
;;; Sevilla, 14-febrero-2025
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; Definir la función primosEquidistantes tal que (primosEquidistantes
;;; k) es la lista de los pares de primos cuya diferencia es k. Por
;;; ejemplo,
;;;    > (s-take 3 (primos-equidistantes1 2))
;;;    ((3 5) (5 7) (11 13))
;;;    > (s-take 3 (primos-equidistantes1 4))
;;;    ((7 11) (13 17) (19 23))
;;;    > (s-take 3 (primos-equidistantes1 6))
;;;    ((23 29) (31 37) (47 53))
;;;    > (s-take 3 (primos-equidistantes1 8))
;;;    ((89 97) (359 367) (389 397))
;;; ---------------------------------------------------------------------

(ql:quickload "fiveam" :silent t)

(defpackage :primos-equidistantes
  (:use :cl
        :fiveam))

(in-package :primos-equidistantes)

;;; Listas infinitas
;;; ================

;;; En las soluciones se usarán las siguientes funciones para trabajar
;;; con listas infinitas (ver "Streams in Common Lisp"
;;; https://tinyurl.com/266685bz).

;;; (delay e) retrasa la evaluación de la función e. Por ejemplo,
;;;    > (delay (+ 2 3))
;;;    #<FUNCTION (LAMBDA ()) {5363B46B}>
(defmacro delay (e)
  `(lambda () ,e))

;;; (force e) evalúa la expresión retrasada retrasada e. Por ejemplo,
;;;    > (force (retrasa (+ 2 3)))
;;;    5
(defun force (e)
  (funcall e))

;;; (s-cons x y) crea una lista perezosa cuyo primer elemento es x
;;; y el resto es la expresión y retrasada. Por ejemplo,
;;;    > (setf ej-lp (s-cons 1 (s-cons 2 nil)))
;;;    (1 . #<FUNCTION (LAMBDA ()) {5368A37B}>)
(defmacro s-cons (x y)
  `(cons ,x (delay ,y)))

;;; (s-first s) es el primer elemento de la lista perezosa s. Por
;;; ejemplo,
;;;    > (s-first (s-cons 1 (s-cons 2 nil)))
;;;    1
(defun s-first (s)
  (car s))

;;; (s-rest s) es el resto de la lista perezosa s. Por ejmplo,
;;;    > (s-rest (s-cons 1 (s-cons 2 nil)))
;;;    (2 . #<FUNCTION (LAMBDA ()) {5363B00B}>)
(defun s-rest (s)
  (force (cdr s)))

;;; (enteros n) es la lista infinita de los números enteros comenzando
;;; desde n (por defecto 1). Por ejemplo,
;;;    > (enteros)
;;;    (1 . #<FUNCTION (LAMBDA () :IN ENTEROS) {100215088B}>)
(defun enteros (&optional (n 1))
  (s-cons n (enteros (1+ n))))

;;; (s-take n s) es la lista de los n primeros elementos de la lista
;;; infinita s.
;;;    > (take 10 (enteros))
;;;    (1 2 3 4 5 6 7 8 9 10)
;;;    > (take 10 (enteros 2))
;;;    (2 3 4 5 6 7 8 9 10 11)
(defun s-take (n s)
  (if (zerop n)
      nil
      (cons (s-first s)
            (s-take (1- n) (s-rest s)))))

;;; (s-filter p s) es la sublista infinita de s con los elementos que
;;; cumplen el predicado p. Por ejemplo,
;;;    > (s-take 5 (s-filter #'evenp (enteros)))
;;;    (2 4 6 8 10)
(defun s-filter (p s)
  (if (funcall p (s-first s))
      (s-cons (s-first s)
               (s-filter p (s-rest s)))
      (s-filter p (s-rest s))))

;;; (s-zip xs ys) es la lista perezosa donde cada elemento es un par
;;; formado por el primer elemento de xs y el primer elemento de ys,
;;; seguido por el segundo elemento de xs y el segundo de ys, y así
;;; sucesivamente. Por ejemplo,
;;;    > (s-take 5 (s-zip (enteros) (s-rest (enteros))))
;;;    ((1 2) (2 3) (3 4) (4 5) (5 6))
(defun s-zip (xs ys)
  (s-cons (list (s-first xs) (s-first ys))
          (s-zip (s-rest xs) (s-rest ys))))

;;; 1ª solución
;;; ===========

;;; (primo1 x) se verifica si x es primo. Por ejemplo,
;;;    > (primo1 7)
;;;    T
;;;    > (primo1 8)
;;;    NIL
(defun primo1 (x)
  (equal (loop for y from 1 to x
               when (= (mod x y) 0)
                 collect y)
         (list 1 x)))

;;; (primos1) es la lista de los números primos. Por ejemplo,
;;;    > (s-take 10 (primos1))
;;;    (2 3 5 7 11 13 17 19 23 29)
(defun primos1 ()
  (s-filter #'primo1 (enteros 2)))

(defun primos-equidistantes1 (k)
  (labels ((aux (ps)
             (if (null (s-rest ps))
                 nil
                 (let ((x (s-first ps))
                       (y (s-first (s-rest ps))))
                   (if (= (- y x) k)
                       (s-cons (list x y) (aux (s-rest ps)))
                       (aux (s-rest ps)))))))
    (aux (primos1))))

;;; 2ª solución
;;; ===========

;;; (primo2 x) se verifica si x es primo. Por ejemplo,
;;;    > (primo2 7)
;;;    T
;;;    > (primo2 8)
;;;    NIL
(defun primo2 (x)
  (and
    (>= x 2)
    (loop for y from 2 to (isqrt x)
          when (= (mod x y) 0)
            do (return nil)
          finally (return t))))

;;; (impares n) es la lista infinita de los números impares comenzando
;;; desde n (por defecto 1). Por ejemplo,
;;;    > (s-take 10 (impares))
;;;    (1 3 5 7 9 11 13 15 17 19)
;;;    > (s-take 10 (impares 3))
;;;    (3 5 7 9 11 13 15 17 19 21)
(defun impares (&optional (n 1))
  (s-cons n (impares (+ n 2))))

;;; (primos2) es la lista de los números primos. Por ejemplo,
;;;    > (s-take 10 (primos2))
;;;    (2 3 5 7 11 13 17 19 23 29)
(defun primos2 ()
  (s-cons 2 (s-filter #'primo2 (impares 3))))

(defun primos-equidistantes2 (k)
  (labels ((aux (ps)
             (if (null (s-rest ps))
                 nil
                 (let ((x (s-first ps))
                       (y (s-first (s-rest ps))))
                   (if (= (- y x) k)
                       (s-cons (list x y) (aux (s-rest ps)))
                       (aux (s-rest ps)))))))
    (aux (primos2))))

;;; 3ª solución
;;; ===========

;;; (primo3 x) se verifica si x es primo. Por ejemplo,
;;;    > (primo3 7)
;;;    T
;;;    > (primo3 8)
;;;    NIL
(defun primo3 (x)
  (cond
    ((< x 2) nil)
    ((= x 2) t)
    ((evenp x) nil)
    (t
     (loop for y from 3 to (isqrt x) by 2
           when (= (mod x y) 0)
             do (return nil)
           finally (return t)))))

;;; (primos3) es la lista de los números primos. Por ejemplo,
;;;    > (s-take 10 (primos2))
;;;    (2 3 5 7 11 13 17 19 23 29)
(defun primos3 ()
  (s-cons 2 (s-filter #'primo3 (impares 3))))

(defun primos-equidistantes3 (k)
  (labels ((aux (ps)
             (if (null (s-rest ps))
                 nil
                 (let ((x (s-first ps))
                       (y (s-first (s-rest ps))))
                   (if (= (- y x) k)
                       (s-cons (list x y) (aux (s-rest ps)))
                       (aux (s-rest ps)))))))
    (aux (primos3))))

;;; 4ª solución
;;; ===========

(defun primos4 ()
  (defun criba (s)
    (s-cons (s-first s)
            (criba (s-filter (lambda (x) (/= (mod x (s-first s)) 0))
                             (s-rest s)))))
  (criba (enteros 2)))

(defun primos-equidistantes4 (k)
  (labels ((aux (ps)
             (let ((x (s-first ps))
                   (y (s-first (s-rest ps))))
               (if (= (- y x) k)
                   (s-cons (list x y) (aux (s-rest ps)))
                   (aux (s-rest ps))))))
    (aux (primos4))))

;;; 5ª solución
;;; ===========

(defun primos-equidistantes5 (k)
  (s-filter (lambda (par) (= (- (second par)
                                (first par))
                             k))
            (s-zip (primos3) (s-rest (primos3)))))

;;; Verificación
;;; ============

(test primos-equidistantes
  (mapc (lambda (primos-equidistantes)
          (is (equal (s-take 3 (funcall primos-equidistantes 2))
                     '((3 5) (5 7) (11 13))))
          (is (equal (s-take 3 (funcall primos-equidistantes 4))
                     '((7 11) (13 17) (19 23))))
          (is (equal (s-take 3 (funcall primos-equidistantes 6))
                     '((23 29) (31 37) (47 53)))))
        '(primos-equidistantes1
          primos-equidistantes2
          primos-equidistantes3
          primos-equidistantes4
          primos-equidistantes5 )))

(defun verifica ()
  (run 'primos-equidistantes))

;;; La verificación es
;;;    > (verifica)
;;;
;;;    Running test PRIMOS-EQUIDISTANTES ...............

;;; Comprobación de equivalencia
;;; ============================

(defun primos-equidistantes-equiv (n k)
  (every (lambda (ys) (equal ys (s-take n (primos-equidistantes1 k))))
         (mapcar (lambda (f) (s-take n (funcall f k)))
                 '(primos-equidistantes2
                   primos-equidistantes3
                   primos-equidistantes4
                   primos-equidistantes5))))

;;; La comprobación es
;;;    > (primos-equidistantes-equiv 100 4)
;;;    T

;;; Comparación de eficiencia
;;; =========================

;;; La comparación es
;;;    > (time (last (s-take 201 (primos-equidistantes1 4))))
;;;    2.499 seconds of real time
;;;    > (time (last (s-take 201 (primos-equidistantes2 4))))
;;;    0.030 seconds of real time
;;;    > (time (last (s-take 201 (primos-equidistantes3 4))))
;;;    0.020 seconds of real time
;;;    > (time (last (s-take 201 (primos-equidistantes4 4))))
;;;    0.164 seconds of real time
;;;    > (time (last (s-take 201 (primos-equidistantes5 4))))
;;;    0.012 seconds of real time
;;;
;;;    > (time (last (s-take 601 (primos-equidistantes2 4))))
;;;    0.061 seconds of real time
;;;    > (time (last (s-take 601 (primos-equidistantes3 4))))
;;;    0.069 seconds of real time
;;;    > (time (last (s-take 601 (primos-equidistantes4 4))))
;;;    3.182 seconds of real time
;;;    > (time (last (s-take 601 (primos-equidistantes5 4))))
;;;    0.047 seconds of real time
;;;
;;;    > (time (last (s-take (expt 10 4) (primos-equidistantes2 4))))
;;;    4.805 seconds of real time
;;;    > (time (last (s-take (expt 10 4) (primos-equidistantes3 4))))
;;;    2.524 seconds of real time
;;;    > (time (last (s-take (expt 10 4) (primos-equidistantes5 4))))
;;;    1.653 seconds of real time
