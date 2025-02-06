;;; Diagonales_principales.lisp
;;; Diagonales principales de una matriz.
;;; José A. Alonso Jiménez <https://jaalonso.github.io>
;;; Sevilla, 5-febrero-2025
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; La lista de las diagonales principales de la matriz
;;;    1  2  3  4
;;;    5  6  7  8
;;;    9 10 11 12
;;; es
;;;    [[9],[5,10],[1,6,11],[2,7,12],[3,8],[4]]
;;;
;;; Definir la función  diagonalesPrincipales tal que
;;; (diagonalesPrincipales p) es la lista de las diagonales principales
;;; de p. Por ejemplo,
;;;    > (diagonales-principales (make-array '(3 4)
;;;                                          :initial-contents '((1  2  3 4)
;;;                                                              (5  6  7 8)
;;;                                                              (9 10 11 12))))
;;;    ((9) (5 10) (1 6 11) (2 7 12) (3 8) (4))
;;; ---------------------------------------------------------------------

(ql:quickload "fiveam" :silent t)

(defpackage :diagonales-principales
  (:use :cl
        :fiveam))

(in-package :diagonales-principales)

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

;;; (posiciones-diagonales-principales1 m n) es la lista de las posiciones
;;; de las diagonales principales de una matriz con m filas y n
;;; columnas. Por ejemplo,
;;;    > (posiciones-diagonales-principales1 3 4)
;;;    (((3 1))
;;;     ((2 1) (3 2))
;;;     ((1 1) (2 2) (3 3))
;;;     ((1 2) (2 3) (3 4))
;;;     ((1 3) (2 4))
;;;     ((1 4)))
(defun posiciones-diagonales-principales1 (m n)
  (mapcar (lambda (ij) (extension ij m n)) (iniciales m n)))

(defun diagonales-principales1 (p)
  (let* ((dimension (array-dimensions p))
         (m (first dimension))
         (n (second dimension)))
    (mapcar (lambda (ijs)
              (mapcar (lambda (ij)
                        (aref p (1- (first ij)) (1- (second ij))))
                      ijs))
            (posiciones-diagonales-principales1 m n))))

;;; 2ª solución
;;; ===========

(defun posiciones-diagonales-principales2 (m n)
  (append
    (loop for i from m downto 1
          collect (loop for r from i to m
                        for c from 1 to n
                        collect (list r c)))
    (loop for j from 2 to n
          collect (loop for r from 1 to m
                        for c from j to n
                        collect (list r c)))))

(defun diagonales-principales2 (p)
  (let* ((dimension (array-dimensions p))
         (m (first dimension))
         (n (second dimension)))
    (mapcar (lambda (ijs)
              (mapcar (lambda (ij)
                        (aref p (1- (first ij)) (1- (second ij))))
                      ijs))
            (posiciones-diagonales-principales2 m n))))

;;; Verificación
;;; ============

(test diagonales-principales
  (mapc (lambda (func)
          (is (equal (funcall func (make-array '(3 4)
                                               :initial-contents
                                               '((1 2 3 4)
                                                 (5 6 7 8)
                                                 (9 10 11 12))))
                     '((9) (5 10) (1 6 11) (2 7 12) (3 8) (4)))))
        '(diagonales-principales1
          diagonales-principales2)))

(defun verifica ()
  (run 'diagonales-principales))

;;; La verificación es
;;;    (diagonales-principales::verifica)


;;; Equivalencia de las definiciones
;;; ================================

;;; (matriz-aleatoria) genera una matriz aleatoria. Por ejemplo.
;;;    > (matriz-aleatoria)
;;;    #2A((1 0 5) (0 5 8) (0 2 4) (5 6 9) (3 3 1) (0 5 8) (5 2 0))
;;;    DIAGONALES-PRINCIPALES> (matriz-aleatoria)
;;;    #2A((8 8) (5 4) (9 5) (3 8))
(defun matriz-aleatoria ()
  (let ((m (1+ (random 10)))
        (n (1+ (random 10))))
    (make-array (list  m n)
                :initial-contents
                (loop for i from 1 to m
                                   collect (loop for j from 1 to n
                                                 collect (random 10))))))

;;; La propiedad es
(test diagonales-principales-equiv
  (for-all ((p 'matriz-aleatoria))
    (is (equal (diagonales-principales1 p)
               (diagonales-principales2 p)))))

(defun comprueba ()
  (run 'diagonales-principales-equiv))

;;; La comprobación es
;;;    > (diagonales-principales::comprueba)
;;;
;;;    Running test DIAGONALES-PRINCIPALES-EQUIV ...

;;; Comparación de eficiencia
;;; =========================

;;; La comparación es
;;;    > (setf ejemplo (make-array '(3000 2000) :initial-element 0))
;;;    > (time (length (diagonales-principales1 ejemplo)))
;;;    0.829 seconds of real time
;;;    > (time (length (diagonales-principales2 ejemplo)))
;;;    1.017 seconds of real time
