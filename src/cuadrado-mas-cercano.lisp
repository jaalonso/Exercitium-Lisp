;;; cuadrado-mas-cercano.lisp
;;; Cuadrado más cercano.
;;; José A. Alonso Jiménez <https://jaalonso.github.io>
;;; Sevilla, 21-abril-2025
;;; ======================================================================

;;; ---------------------------------------------------------------------
;;; Definir la función `cuadrado-cercano` tal que (cuadrado-cercano n)
;;; es el número cuadrado más cercano a n, donde n es un entero
;;; positivo. Por ejemplo,
;;;    (cuadrado-cercano 2) == 1
;;;    (cuadrado-cercano 6) == 4
;;;    (cuadrado-cercano 8) == 9
;;;    (cuadrado-cercano (expt 10 46)) == 10000000000000000000000000000000000000000000000
;;; ---------------------------------------------------------------------

(ql:quickload "fiveam" :silent t)

(defpackage :cuadrado-mas-cercano
  (:use :cl
        :fiveam))

(in-package :cuadrado-mas-cercano)

;;; 1ª solución
;;; ===========

;;; (raiz-entera1 x) es el mayor entero cuyo cuadrado no es mayor que
;;; x. Por ejemplo,
;;;    (raiz-entera1 8)   ==  2
;;;    (raiz-entera1 9)   ==  3
;;;    (raiz-entera1 10)  ==  3
(defun raiz-entera1 (x)
  (loop for n from 1
        while (<= (* n n) x)
        finally (return (- n 1))))

(defun cuadrado-cercano1 (n)
  (let* ((a (raiz-entera1 n))
         (b (* a a))
         (c (* (+ a 1) (+ a 1))))
    (if (< (- n b) (- c n))
        b
        c)))

;;; 2ª solución
;;; ===========

;;; (raiz-entera2 x) es el mayor entero cuyo cuadrado no es mayor que
;;; x. Por ejemplo,
;;;    (raiz-entera2 8)   ==  2
;;;    (raiz-entera2 9)   ==  3
;;;    (raiz-entera2 10)  ==  3
(defun raiz-entera2 (x)
  (labels ((aux (a b)
             (let* ((c (floor (+ a b) 2))
                   (d (* c c)))
               (cond ((= d x) c)
                     ((= c a) c)
                     ((<= x d) (aux a c))
                     (t (aux c b))))))
    (aux 1 x)))


(defun cuadrado-cercano2 (n)
  (let* ((a (raiz-entera2 n))
         (b (* a a))
         (c (* (+ a 1) (+ a 1))))
    (if (< (- n b) (- c n))
        b
        c)))

;;; 3ª solución
;;; ===========

(defun cuadrado-cercano3 (n)
  (expt (round (sqrt n)) 2))

;;; La 3ª solución es incorrecta. Por ejemplo,
;;;    > (cuadrado-cercano3 (expt 10 46))
;;;    9999999556392621642119762459277931153299865600
;;;    > (cuadrado-cercano2 (expt 10 46))
;;;    10000000000000000000000000000000000000000000000

;;; Verificación
;;; ============

(test cuadrado-cercano
  (mapc (lambda (cuadrado-cercano)
          (is (= (funcall cuadrado-cercano 2) 1))
          (is (= (funcall cuadrado-cercano 6) 4))
          (is (= (funcall cuadrado-cercano 8) 9)))
        '(cuadrado-cercano1
          cuadrado-cercano2
          cuadrado-cercano3
          )))

(defun verifica ()
  (run 'cuadrado-cercano))

;;; La verificación es
;;;    (verifica)
;;;
;;;    Running test CUADRADO-CERCANO ........

;;; Equivalencia de las definiciones
;;; ================================

;;; La propiedad es
(test cuadrado-cercano-equiv
  (for-all ((n (gen-integer :min 1 :max 10)))
    (is (= (cuadrado-cercano1 n)
           (cuadrado-cercano2 n)))))

(defun comprueba ()
  (run 'cuadrado-cercano-equiv))

;;; La comprobación es
;;;    > (comprueba)
;;;
;;;    Running test LISTA-CUADRADA-EQUIV ...

;;; Comparación de eficiencia
;;; =========================

;;; La comparación es
;;;    > (time (cuadrado-cercano1 (expt 10 14)))
;;;    0.082 seconds of real time
;;;    > (time (cuadrado-cercano2 (expt 10 14)))
;;;    0.000 seconds of real time
;;;    > (time (cuadrado-cercano3 (expt 10 14)))
;;;    0.000 seconds of real time
