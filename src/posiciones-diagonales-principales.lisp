;;; Posiciones_diagonales_principales.lisp
;;; Posiciones de las diagonales principales.
;;; José A. Alonso Jiménez <https://jaalonso.github.io>
;;; Sevilla, 4-febrero-2025
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; Las posiciones de una matriz con 3 filas y 4 columnas son
;;;    (1 1) (1 2) (1 3) (1 4)
;;;    (2 1) (2 2) (2 3) (2 4)
;;;    (3 1) (3 2) (3 3) (3 4)
;;; Las posiciones de sus 6 diagonales principales son
;;;   ((3 1))
;;;   ((2 1) (3 2))
;;;   ((1 1) (2 2) (3 3))
;;;   ((1 2) (2 3) (3 4))
;;;   ((1 3) (2 4))
;;;   ((1 4))
;;;
;;; Definir la función posiciones-diagonales-principales tal que
;;; (posiciones-diagonales-principales m n) es la lista de las posiciones
;;; de las diagonales principales de una matriz con m filas y n
;;; columnas. Por ejemplo,
;;;    > (posiciones-diagonales-principales 3 4)
;;;    (((3 1))
;;;     ((2 1) (3 2))
;;;     ((1 1) (2 2) (3 3))
;;;     ((1 2) (2 3) (3 4))
;;;     ((1 3) (2 4))
;;;     ((1 4)))
;;;    > (posiciones-diagonales-principales 4 4)
;;;    (((4 1))
;;;     ((3 1) (4 2))
;;;     ((2 1) (3 2) (4 3))
;;;     ((1 1) (2 2) (3 3) (4 4))
;;;     ((1 2) (2 3) (3 4))
;;;     ((1 3) (2 4))
;;;     ((1 4)))
;;;    > (posiciones-diagonales-principales 4 3)
;;;    (((4 1))
;;;     ((3 1) (4 2))
;;;     ((2 1) (3 2) (4 3))
;;;     ((1 1) (2 2) (3 3))
;;;     ((1 2) (2 3))
;;;     ((1 3)))
;;; ---------------------------------------------------------------------

(ql:quickload "fiveam" :silent t)

(defpackage :posiciones-diagonales-principales
  (:use :cl
        :fiveam))

(in-package :posiciones-diagonales-principales)

;;; 1ª solución
;;; ===========

;;; (iniciales m n) es la lista de los elementos iniciales de las
;;; posiciones de las diagonales principales de una matriz con m filas y
;;; n columnas. Por ejemplo,
;;;    > (iniciales 3 4)
;;;    ((3 1) (2 1) (1 1) (1 2) (1 3) (1 4))
(defun iniciales (m n)
  (append (loop for i from m downto 2 collect (list i 1))
          (loop for j from 1 to n collect (list 1 j))))

;;; (extension ij m n) es extension de la diagonal principal a partir de
;;; la posicion ij en una matriz de orden mxn. Por ejemplo,
;;;    > (extension '(2 1) 3 4)
;;;    ((2 1) (3 2))
;;;    > (extension '(1 1) 3 4)
;;;    ((1 1) (2 2) (3 3))
;;;    > (extension '(2 3) 3 4)
;;;    ((2 3) (3 4))
(defun extension (ij m n)
  (let ((i (first ij))
        (j (second ij)))
    (loop for k from 0 to (min (- m i) (- n j))
          collect (list (+ i k) (+ j k)))))

(defun posiciones-diagonales-principales-1 (m n)
  (mapcar (lambda (ij) (extension ij m n)) (iniciales m n)))

;;; 2ª solución
;;; ===========

(defun posiciones-diagonales-principales-2 (m n)
  (flet ((iniciales ()
           (append (loop for i from m downto 2 collect (list i 1))
                   (loop for j from 1 to n collect (list 1 j))))
         (extension (ij)
           (let ((i (first ij))
                 (j (second ij)))
             (loop for k from 0 to (min (- m i) (- n j))
                   collect (list (+ i k) (+ j k))))))
    (mapcar (lambda (ij) (extension ij)) (iniciales))))

;;; 3ª definición
;;; =============

(defun posiciones-diagonales-principales-3 (m n)
  (append
    (loop for i from m downto 1
          collect (loop for r from i to m
                        for c from 1 to n
                        collect (list r c)))
    (loop for j from 2 to n
          collect (loop for r from 1 to m
                        for c from j to n
                        collect (list r c)))))

;;; 4ª solución
;;; ===========

(defun posiciones-diagonales-principales-4 (m n)
  (let ((iniciales (append (loop for i from m downto 2 collect (list i 1))
                           (loop for j from 1 to n collect (list 1 j))))
        (result '()))
    (dolist (ij iniciales result)
      (let* ((i (first ij))
             (j (second ij))
             (lim (min (- m i) (- n j)))
             (diagonal (loop for k from 0 to lim collect (list (+ i k) (+ j k)))))
        (push diagonal result)))
    (nreverse result)))

;;; Verificación
;;; ============

(test posiciones-diagonales-principales
  (mapc (lambda (func)
          (is (equal (funcall func 3 4) '(((3 1))
                                          ((2 1) (3 2))
                                          ((1 1) (2 2) (3 3))
                                          ((1 2) (2 3) (3 4))
                                          ((1 3) (2 4))
                                          ((1 4)))))
          (is (equal (funcall func '4 4) '(((4 1))
                                           ((3 1) (4 2))
                                           ((2 1) (3 2) (4 3))
                                           ((1 1) (2 2) (3 3) (4 4))
                                           ((1 2) (2 3) (3 4))
                                           ((1 3) (2 4))
                                           ((1 4)))))
          (is (equal (funcall func '4 3) '(((4 1))
                                           ((3 1) (4 2))
                                           ((2 1) (3 2) (4 3))
                                           ((1 1) (2 2) (3 3))
                                           ((1 2) (2 3))
                                           ((1 3))))))
        '(posiciones-diagonales-principales-1
          posiciones-diagonales-principales-2
          posiciones-diagonales-principales-3
          posiciones-diagonales-principales-4)))

(defun verifica ()
  (run 'posiciones-diagonales-principales))

;;; La verificación es
;;;    > (posiciones-diagonales-principales::verifica)
;;;
;;;    Running test POSICIONES-DIAGONALES-PRINCIPALES .........

;;; Equivalencia de las definiciones
;;; ================================

;;; La propiedad es
(test posiciones-diagonales-principales-equiv
  (for-all ((m (gen-integer :min 1 :max 20))
            (n (gen-integer :min 1 :max 20)))
    (let ((res (posiciones-diagonales-principales-1 m n)))
      (is (equal (posiciones-diagonales-principales-2 m n) res))
      (is (equal (posiciones-diagonales-principales-3 m n) res))
      (is (equal (posiciones-diagonales-principales-4 m n) res)))))

(defun comprueba ()
  (run 'posiciones-diagonales-principales-equiv))

;;; La comprobación es
;;;    > (posiciones-diagonales-principales::comprueba)
;;;
;;;    Running test POSICIONES-DIAGONALES-PRINCIPALES-EQUIV ...
;;;    (#<IT.BESE.FIVEAM::FOR-ALL-TEST-PASSED {10030E76B3}>)

;;; Comparación de eficiencia
;;; =========================

;;; La comparación es
;;;   > (time (length (posiciones-diagonales-principales-1 3000 2000)))
;;;   0.453 seconds of real time
;;;   > (time (length (posiciones-diagonales-principales-2 3000 2000)))
;;;   0.807 seconds of real time
;;;   > (time (length (posiciones-diagonales-principales-3 3000 2000)))
;;;   0.440 seconds of real time
;;;   > (time (length (posiciones-diagonales-principales-4 3000 2000)))
;;;   0.672 seconds of real time
