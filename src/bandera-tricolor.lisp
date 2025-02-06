;;; Bandera_tricolor.lisp
;;; La bandera tricolor.
;;; José A. Alonso Jiménez <https://jaalonso.github.io>
;;; Sevilla, 3-febrero-2025
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; El problema de la bandera tricolor consiste en lo siguiente: Dada un
;;; lista de objetos xs que pueden ser rojos, amarillos o morados, se pide
;;; devolver una lista ys que contiene los elementos de xs, primero los
;;; rojos, luego los amarillos y por último los morados.
;;;
;;; Definir la función bandera-tricolor tal que (banderaTricolor xs) es
;;; la bandera tricolor formada con los elementos de xs. Por ejemplo,
;;;    banderaTricolor [M,R,A,A,R,R,A,M,M]  ==  [R,R,R,A,A,A,M,M,M]
;;;    banderaTricolor [M,R,A,R,R,A]        ==  [R,R,R,A,A,M]
;;; ---------------------------------------------------------------------

(ql:quickload "fiveam" :silent t)

(defpackage :bandera-tricolor
  (:use :cl
        :fiveam))

(in-package :bandera-tricolor)

(defconstant R 'R)
(defconstant A 'A)
(defconstant M 'M)

;;; 1ª solución
;;; ===========

(defun bandera-tricolor-1 (xs)
  (append (loop for x in xs when (eql x R) collect x)
          (loop for x in xs when (eql x A) collect x)
          (loop for x in xs when (eql x M) collect x)))

;;; 2ª solución
;;; ===========

;;; (colores c xs) es la lista de elementos de xs cuyo color es c. Por
;;; ejemplo,
;;;    > (colores R '(M R A A R R A M M))
;;;    (R R R)
(defun colores (c xs)
  (remove-if-not (lambda (x) (eql x c)) xs))

(defun bandera-tricolor-2 (xs)
  (append (colores R xs)
          (colores A xs)
          (colores M xs)))

;;; 3ª solución
;;; ===========

(defun bandera-tricolor-3 (xs)
  (apply #'append
         (mapcar (lambda (c) (colores c xs))
                 '(R A M))))

;;; 4ª solución
;;; ===========

(defun bandera-tricolor-4 (xs)
  (labels ((aux (ys rs as ms)
             (cond ((null ys) (append (reverse rs) (reverse as) (reverse ms)))
                   ((eql (first ys) R) (aux (rest ys) (cons (first ys) rs) as ms))
                   ((eql (first ys) A) (aux (rest ys) rs (cons (first ys) as) ms))
                   ((eql (first ys) M) (aux (rest ys) rs as (cons (first ys) ms))))))
    (aux xs '() '() '())))

;;; 5ª solución
;;; ===========

(defun bandera-tricolor-5 (xs)
  (let ((rs '())
        (as '())
        (ms '()))
    (dolist (x xs)
      (case x
        (R (push x rs))
        (A (push x as))
        (M (push x ms))))
    (append rs as ms)))

;;; 6ª solución
;;; ===========

(defun orden (c)
  (case c
    (R 0)
    (A 1)
    (M 2)))

(defun bandera-tricolor-6 (xs)
  (sort (copy-list xs) #'< :key #'orden))

;;; 7ª solución
;;; ===========

(defun menor (x y)
  (< (orden x) (orden y)))

(defun bandera-tricolor-7 (xs)
  (sort (copy-list xs) #'menor))

;;; Verificación
;;; ============

(test bandera-tricolor
  (mapc (lambda (func)
          (is (equal (funcall func '(M R A A R R A M M)) '(R R R A A A M M M)))
          (is (equal (funcall func '(M R A R R A)) '(R R R A A M))))
        '(bandera-tricolor-1 bandera-tricolor-2 bandera-tricolor-3
          bandera-tricolor-4 bandera-tricolor-5 bandera-tricolor-6
          bandera-tricolor-7)))

(defun verifica ()
  (run 'bandera-tricolor))

;;; La verificación es
;;;    > (bandera-tricolor::verifica)
;;;
;;;    Running test BANDERA-TRICOLOR .............

;;; Comprobación de equivalencia
;;; ============================

;;; (lista-de-colores-aleatoria) es una lista de colores aleatorias con
;;; 100 elementos como máximo. Por ejemplo,
;;;    > (lista-de-colores-aleatoria)
;;;    (M M A R A A M A R M R R A M A A M M R A R R A A M M A A R R M R A R A M M M A
;;;     R M M M R M M A M R R M R R A A A M A M A R R R M R M A M R A R M R M R R A A
;;;     M M R R A R M A R R R R M R)
;;;    > (lista-de-colores-aleatoria)
;;;    (A A A M A M R R M M M R A M M M R M M A A R A M M A A M M R A M A R R R R A R
;;;     A M R M A M R M M A R R A M M A A M A M A R A)
(defun lista-de-colores-aleatoria ()
  (let ((length (random 100)))
    (loop repeat length collect (nth (random 3) '(R A M)))))

(test bandera-tricolor-equiv
  (let* ((xs (lista-de-colores-aleatoria))
         (r1 (bandera-tricolor-1 xs)))
    (is (equal r1 (bandera-tricolor-2 xs)))
    (is (equal r1 (bandera-tricolor-3 xs)))
    (is (equal r1 (bandera-tricolor-4 xs)))
    (is (equal r1 (bandera-tricolor-5 xs)))
    (is (equal r1 (bandera-tricolor-6 xs)))
    (is (equal r1 (bandera-tricolor-7 xs)))))

(defun comprueba ()
  (run 'bandera-tricolor-equiv))

;;; La comprobación es
;;;    > (bandera-tricolor::comprueba)
;;;    Running test BANDERA-TRICOLOR-EQUIV ......

;;; Comparación de eficiencia
;;; =========================

;;; (bandera n) una lista que repite cada color (M, R, A) n veces. Por
;;; ejemplo,
;;;    > (bandera 3)
;;;    (M M M R R R A A A)
(defun bandera (n)
  (loop for c in '(M R A)
        append (loop repeat n collect c)))

;;; La comparación es
;;;    > (time (length (bandera-tricolor-1 (bandera (expt 10 6)))))
;;;    0.252 seconds of real time
;;;    > (time (length (bandera-tricolor-2 (bandera (expt 10 6)))))
;;;    0.383 seconds of real time
;;;    > (time (length (bandera-tricolor-3 (bandera (expt 10 6)))))
;;;    0.371 seconds of real time
;;;    > (time (length (bandera-tricolor-4 (bandera (expt 10 6)))))
;;;    0.308 seconds of real time
;;;    > (time (length (bandera-tricolor-5 (bandera (expt 10 6)))))
;;;    0.301 seconds of real time
;;;    > (time (length (bandera-tricolor-6 (bandera (expt 10 6)))))
;;;    0.378 seconds of real time
;;;    > (time (length (bandera-tricolor-7 (bandera (expt 10 6)))))
;;;    Evaluation took:
;;;    0.276 seconds of real time
