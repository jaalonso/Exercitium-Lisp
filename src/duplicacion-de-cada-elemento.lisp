;;; duplicacion-de-cada-elemento.lisp
;;; Duplicación de cada elemento.
;;; José A. Alonso Jiménez
;;; Sevilla, 25-abril-2025
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; Definir la función duplica-elementos tal que (duplica-elementos xs)
;;; es la lista obtenida duplicando cada elemento de xs. Por ejemplo,
;;;    > (duplica-elementos '(3 2 5))
;;;    (3 3 2 2 5 5)
;;; ---------------------------------------------------------------------

(ql:quickload "fiveam" :silent t)

(defpackage :duplicacion-de-cada-elemento
  (:use :cl
        :fiveam))

(in-package :duplicacion-de-cada-elemento)

;;; 1ª solución
;;; ============

(defun duplica-elementos-1 (xs)
  (if (null xs)
      nil
      (cons (first xs) (cons (first xs) (duplica-elementos-1 (rest xs))))))

;;; 2ª solución
;;; ===========

(defun duplica-elementos-2 (xs)
  (reduce (lambda (x ys) (cons x (cons x ys)))
          xs
          :from-end t
          :initial-value '()))

;;; 3ª solución
;;; ===========

(defun duplica-elementos-3 (xs)
  (apply #'append
         (mapcar (lambda (x) (list x x)) xs)))

;;; 4ª solución
;;; ===========

(defun duplica-elementos-4 (xs)
  (reduce #'append
          (mapcar (lambda (x) (list x x)) xs)
          :initial-value '()
          :from-end t))

;;; 5ª solución
;;; ===========

(defun duplica-elementos-5 (xs)
  (mapcan (lambda (x) (list x x)) xs))

;;; 6ª solución
;;; ===========

(defun duplica-elementos-6 (xs)
  (loop for x in xs
        appending (list x x)))

;;; 7ª solución
;;; ===========

(defun duplica-elementos-7 (xs)
  (let ((ys '()))
    (dolist (x xs (reverse ys))
      (setf ys (cons x (cons x ys))))))

;;; Verificación
;;; ============

(test duplica-elementos
  (mapc (lambda (duplica-elementos)
          (is (equal (funcall duplica-elementos '(3 2 5)) '(3 3 2 2 5 5))))
        '(duplica-elementos-1
          duplica-elementos-2
          duplica-elementos-3
          duplica-elementos-4
          duplica-elementos-5
          duplica-elementos-6
          duplica-elementos-7)))

(defun verifica ()
  (run 'duplica-elementos))

;;; La verificación es
;;;    (verifica)
;;;
;;;    Running test DUPLICA-ELEMENTOS ........

;;; Equivalencia de las definiciones
;;; ================================

;;; La propiedad es
(test duplica-elementos-equiv
  (for-all ((xs (gen-list)))
    (let ((ys (duplica-elementos-1 xs)))
      (is (equal ys (duplica-elementos-2 xs)))
      (is (equal ys (duplica-elementos-3 xs)))
      (is (equal ys (duplica-elementos-4 xs)))
      (is (equal ys (duplica-elementos-5 xs)))
      (is (equal ys (duplica-elementos-6 xs)))
      (is (equal ys (duplica-elementos-7 xs))))))

(defun comprueba ()
  (run 'duplica-elementos-equiv))

;;; La comprobación es
;;;    > (comprueba)
;;;
;;;    Running test DUPLICA-ELEMENTOS-EQUIV ...

;;; Comparación de eficiencia
;;; =========================

;;; La comparación es
;;;    > (defvar ejemplo1 (loop for i from 1 to 40000 collect i))
;;;    > (time (length (duplica-elementos-1 ejemplo1)))
;;;    0.003 seconds of real time
;;;    > (time (length (duplica-elementos-2 ejemplo1)))
;;;    0.003 seconds of real time
;;;    > (time (length (duplica-elementos-3 ejemplo1)))
;;;    0.003 seconds of real time
;;;    > (time (length (duplica-elementos-4 ejemplo1)))
;;;    0.006 seconds of real time
;;;    > (time (length (duplica-elementos-5 ejemplo1)))
;;;    0.004 seconds of real time
;;;    > (time (length (duplica-elementos-6 ejemplo1)))
;;;    0.001 seconds of real time
;;;    > (time (length (duplica-elementos-7 ejemplo1)))
;;;    0.007 seconds of real time
;;;
;;;    > (time (length (duplica-elementos-2 (make-list 1000000 :initial-element 0))))
;;;    0.154 seconds of real time
;;;    (time (length (duplica-elementos-4 (make-list 1000000 :initial-element 0))))
;;;    0.168 seconds of real time
;;;    > (time (length (duplica-elementos-5 (make-list 1000000 :initial-element 0))))
;;;    0.146 seconds of real time
;;;    > (time (length (duplica-elementos-6 (make-list 1000000 :initial-element 0))))
;;;    0.075 seconds of real time
;;;    > (time (length (duplica-elementos-7 (make-list 1000000 :initial-element 0))))
;;;    0.099 seconds of real time
