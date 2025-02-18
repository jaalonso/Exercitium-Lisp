;;; matriz-toeplitz.lisp
;;; Matrices de Toepliz
;;; José A. Alonso Jiménez <https://jaalonso.github.io>
;;; Sevilla, 18-febrero-2025
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; Una [matriz de Toeplitz](https://tinyurl.com/zqzokbm) es una matriz
;;; cuadrada que es constante a lo largo de las diagonales paralelas a la
;;; diagonal principal. Por ejemplo,
;;;    |2 5 1 6|       |2 5 1 6|
;;;    |4 2 5 1|       |4 2 6 1|
;;;    |7 4 2 5|       |7 4 2 5|
;;;    |9 7 4 2|       |9 7 4 2|
;;; la primera es una matriz de Toeplitz y la segunda no lo es.
;;;
;;; Las anteriores matrices se pueden definir por
;;;    (defvar ej1 (make-array '(4 4) :initial-contents '((2 5 1 6)
;;;                                                       (4 2 5 1)
;;;                                                       (7 4 2 5)
;;;                                                       (9 7 4 2))))
;;;    (defvar ej2 (make-array '(4 4) :initial-contents '((2 5 1 6)
;;;                                                       (4 2 6 1)
;;;                                                       (7 4 2 5)
;;;                                                       (9 7 4 2))))
;;;
;;; Definir la función es-toeplitz tal que (es-toeplitz p) se verifica
;;; si la matriz p es de Toeplitz. Por ejemplo,
;;;    > (es-toeplitz1 ej1)
;;;    T
;;;    > (es-toeplitz1 ej2)
;;;    NIL
;;; ---------------------------------------------------------------------

(ql:quickload "fiveam" :silent t)

(defpackage :matriz-toeplitz
  (:use :cl
        :fiveam))

(in-package :matriz-toeplitz)

(defvar ej1 (make-array '(4 4) :initial-contents '((2 5 1 6)
                                                   (4 2 5 1)
                                                   (7 4 2 5)
                                                   (9 7 4 2))))
(defvar ej2 (make-array '(4 4) :initial-contents '((2 5 1 6)
                                                   (4 2 6 1)
                                                   (7 4 2 5)
                                                   (9 7 4 2))))

;;; 1ª solución
;;; ===========

;;; (esCuadrada p) se verifica si la matriz p es cuadrada. Por ejemplo,
;;;    > (es-cuadrada (make-array '(4 4)))
;;;    T
;;;    > (es-cuadrada (make-array '(4 3)))
;;;    NIL
(defun es-cuadrada (p)
  (= (array-dimension p 0) (array-dimension p 1)))

;;; (posicionesDiagonalesPrincipales m n) es la lista de las
;;; posiciones de las diagonales principales de una matriz con m filas y
;;; n columnas. Por ejemplo,
;;;    > (posiciones-diagonales-principales 3 4)
;;;    (((3 1))
;;;     ((2 1) (3 2))
;;;     ((1 1) (2 2) (3 3))
;;;     ((1 2) (2 3) (3 4))
;;;     ((1 3) (2 4))
;;;     ((1 4)))
(defun posiciones-diagonales-principales (m n)
  (append
    (loop for i from m downto 1
          collect (loop for r from i to m
                        for c from 1 to n
                        collect (list r c)))
    (loop for j from 2 to n
          collect (loop for r from 1 to m
                        for c from j to n
                        collect (list r c)))))

;;; (diagonalesPrincipales p) es la lista de las diagonales principales
;;; de p. Por ejemplo,
;;;    > (diagonales-principales (make-array '(3 4)
;;;                                          :initial-contents '((1  2  3 4)
;;;                                                              (5  6  7 8)
;;;                                                              (9 10 11 12))))
;;;    ((9) (5 10) (1 6 11) (2 7 12) (3 8) (4))
(defun diagonales-principales (p)
  (let ((m (array-dimension p 0))
        (n (array-dimension p 1)))
    (mapcar (lambda (ijs)
              (mapcar (lambda (ij)
                        (aref p (1- (first ij)) (1- (second ij))))
                      ijs))
            (posiciones-diagonales-principales m n))))

;;; (todosIguales xs) se verifica si todos los elementos de xs son
;;; iguales. Por ejemplo,
;;;    > (todos-iguales '(5 5 5))
;;;    T
;;;    > (todos-iguales '(5 4 5))
;;;    NIL
(defun todos-iguales (xs)
  (every (lambda (x) (eql x (first xs))) xs))

(defun es-toeplitz1 (p)
  (and (es-cuadrada p)
       (every #'todos-iguales (diagonales-principales p))))

;;; 2ª solución
;;; ===========

(defun es-toeplitz2 (p)
  (let ((m (array-dimension p 0))
        (n (array-dimension p 1)))
    (and (= m n)
         (loop for i from 0 to (- m 2)
               always
               (loop for j from 0 to (- n 2)
                     always
                     (= (aref p i j)
                        (aref p (+ i 1) (+ j 1))))))))

;;; Verificación
;;; ============

(test es-toeplitz
  (mapc (lambda (es-toeplitz)
          (is-true (funcall es-toeplitz ej1))
          (is-false (funcall es-toeplitz ej2)))
        '(es-toeplitz1
          es-toeplitz2)))

(defun verifica ()
  (run 'es-toeplitz))

;;; La verificación es
;;;    (verifica)
;;;
;;;    Running test ES-TOEPLITZ ....

;;; Comparación de eficiencia
;;; =========================

;;; La comparación es
;;;    > (time (es-toeplitz1 (make-array '(3000 3000))))
;;;    1.282 seconds of real time
;;;    > (time (es-toeplitz2 (make-array '(3000 3000))))
;;;    0.362 seconds of real time
