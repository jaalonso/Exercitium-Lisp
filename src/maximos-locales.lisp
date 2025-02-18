;;; maximos-locales.lisp
;;; Máximos locales.
;;; José A. Alonso Jiménez <https://jaalonso.github.io>
;;; Sevilla, 19-febrero-2025
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; Un máximo local de una lista es un elemento de la lista que es mayor
;;; que su predecesor y que su sucesor en la lista. Por ejemplo, 5 es un
;;; máximo local de [3,2,5,3,7,7,1,6,2] ya que es mayor que 2 (su
;;; predecesor) y que 3 (su sucesor).
;;;
;;; Definir la función
;;;    maximosLocales :: Ord a => [a] -> [a]
;;; tal que (maximosLocales xs) es la lista de los máximos locales de la
;;; lista xs. Por ejemplo,
;;;    > (maximos-locales '(3 2 5 3 7 7 1 6 2))
;;;    (5 6)
;;;    > (maximos-locales '(7 1 2 3))
;;;    NIL
;;; ---------------------------------------------------------------------

(ql:quickload "fiveam" :silent t)

(defpackage :maximos-locales
  (:use :cl
        :fiveam))

(in-package :maximos-locales)

;;; 1ª solución
;;; ===========

(defun maximos-locales1 (xs)
  (cond
    ((>= (length xs) 3)
     (let ((x (first xs))
           (y (second xs))
           (z (third xs)))
       (if (and (> y x) (> y z))
           (cons y (maximos-locales1 (cddr xs)))
           (maximos-locales1 (cdr xs)))))
    (t ())))

;;; 2ª solución
;;; ===========

(defun maximos-locales2 (xs)
  (cond
    ((null (cddr xs)) nil)
    (t (let ((x (first xs))
             (y (second xs))
             (z (third xs)))
         (if (and (> y x) (> y z))
             (cons y (maximos-locales2 (cddr xs)))
             (maximos-locales2 (cdr xs)))))))

;;; 3ª solución
;;; ===========

;;; (zip l1 ... ln) es la lista de las listas de los elementos de las
;;; listas l1,..., ln en la misma posición. Por ejemplo,
;;;    > (zip '(1 2 3 4) '(a b c) '("x" "y" "z"))
;;;    ((1 A "x") (2 B "y") (3 C "z"))
(defun zip (&rest listas)
  (apply #'mapcar #'list listas))

(defun maximos-locales3 (lista)
  (let ((ternas (zip lista (cdr lista) (cddr lista))))
    (loop for (x y z) in ternas
          when (and (> y x) (> y z))
            collect y)))

;;; 4ª solución
;;; ===========

(defun maximos-locales4 (xs)
  (let ((resultado '()))
    (loop for (x y z) on xs
          while (and y z)
          do (cond ((and (> y x) (> y z))
                    (push y resultado)
                    (setf xs (cddr xs)))
                   (t (setf xs (cdr xs)))))
    (nreverse resultado)))

;;; Verificación
;;; ============

(test maximos-locales
  (mapc (lambda (maximos-locales)
          (is (equal (funcall maximos-locales '(3 2 5 3 7 7 1 6 2))
                     '(5 6)))
          (is (equal (funcall maximos-locales '(7 1 2 3))
                     nil)))
        '(maximos-locales1
          maximos-locales2
          maximos-locales3
          maximos-locales4
          )))

(defun verifica ()
  (run 'maximos-locales))

;;; La verificación es
;;;    (verifica)
;;;
;;;    Running test MAXIMOS-LOCALES ........

;;; Comprobación de equivalencia
;;; ============================

;;; La propiedad es
(test maximos-locales-equiv
  (for-all ((xs (gen-list)))
    (let ((ys (maximos-locales1 xs)))
      (is (equal ys (maximos-locales2 xs)))
      (is (equal ys (maximos-locales3 xs)))
      (is (equal ys (maximos-locales4 xs))))))

(defun comprueba ()
  (run 'maximos-locales-equiv))

;;; La comprobación es
;;;    > (comprueba)
;;;
;;;    Running test MAXIMOS-LOCALES-EQUIV ...

;;; Comparación de eficiencia
;;; =========================

;;; (ejemplo n) es la lista obtenida repitiendo n veces los elementos 1,
;;; 2 y 3. Por ejemplo,
;;;    > (ejemplo 2)
;;;    (1 2 3 1 2 3)
(defun ejemplo (n)
  (reduce #'append
          (make-list n :initial-element '(1 2 3))
          :initial-value '()
          :from-end t))

;;; La comparación es
;;;    > (time (last (maximos-locales1 (ejemplo 10000))))
;;;    0.600 seconds of real time
;;;    > (time (last (maximos-locales2 (ejemplo 10000))))
;;;    0.003 seconds of real time
;;;    > (time (last (maximos-locales3 (ejemplo 10000))))
;;;    0.005 seconds of real time
;;;    > (time (last (maximos-locales4 (ejemplo 10000))))
;;;    0.001 seconds of real time
;;;
;;;    > (time (last (maximos-locales3 (ejemplo 2000000))))
;;;    1.161 seconds of real time
;;;    > (time (last (maximos-locales4 (ejemplo 2000000))))
;;;    0.588 seconds of real time
