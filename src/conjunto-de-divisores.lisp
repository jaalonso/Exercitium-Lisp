;;; conjunto-de-divisores.lisp
;;; Conjunto de divisores.
;;; José A. Alonso Jiménez <https://jaalonso.github.io>
;;; Sevilla, 3-mayo-2025
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; Definir la función
;;;    divisores :: Integer -> [Integer]
;;; tal que (divisores x) es el conjunto de divisores de x. Por ejemplo,
;;;   divisores 30  ==  [1,2,3,5,6,10,15,30]
;;;   length (divisores (product [1..10]))  ==  270
;;;   length (divisores (product [1..25]))  ==  340032
;;; ---------------------------------------------------------------------

(ql:quickload '("fiveam"
                "alexandria"
                "cl-factoring")
              :silent t)

(defpackage :conjunto-de-divisores
  (:use :cl
        :fiveam
        :cl-factoring
        :alexandria
        ))

(in-package :conjunto-de-divisores)

;;; 1ª solución
;;; ===========

(defun divisores-1 (n)
  (labels ((aux (x)
             (cond ((> x n) nil)
                   ((zerop (mod n x))
                    (cons x (aux (1+ x))))
                   (t (aux (1+ x))))))
    (aux 1)))

;;; 2ª solución
;;; ===========

(defun divisores-2 (n)
  (labels ((aux (xs x)
             (cond ((zerop x) xs)
                   ((zerop (mod n x))
                    (aux (cons x xs) (1- x)))
                   (t (aux xs (1- x))))))
    (aux nil n)))

;;; 3ª solución
;;; ===========

(defun divisores-3 (n)
  (mapcan (lambda (x)
            (when (zerop (mod n x)) (list x)))
          (iota n :start 1)))

;;; 4ª solución
;;; ===========

(defun divisores-4 (n)
  (remove-if-not (lambda (x) (zerop (mod n x)))
                 (iota n :start 1)))

;;; 5ª solución
;;; ===========

(defun divisores-5 (n)
  (loop for x from 1 to n
        when (zerop (mod n x))
          collect x))

;;; 6ª solución
;;; ===========

(defun divisores-6 (n)
  (sort
   (remove-duplicates
    (loop for i from 1 to (isqrt n)
          when (zerop (mod n i))
          append (list i (floor n i))))
   #'<))

;;; 7ª solución
;;; ===========

;;; (divisores-primos n) es la lista de los divisores primos de n. Por
;;; ejemplo,
;;;    > (divisores-primos 300)
;;;    (2 2 3 5 5)
(defun divisores-primos (n &optional (d 2))
  (cond
    ((<= n 1) nil)
    ((= (mod n d) 0) (cons d (divisores-primos (/ n d) d)))
    (t (divisores-primos n (+ d 1)))))

;;; (subsucesiones xs) es la lista de las subsucesiones de xs. Por
;;; ejemplo,
;;;    > (subsucesiones '(2 3 5))
;;;    (NIL (5) (3) (3 5) (2) (2 5) (2 3) (2 3 5))
(defun subsucesiones (xs)
  (if (null xs)
      '(())
      (let ((yss (subsucesiones (cdr xs))))
        (append yss (mapcar (lambda (ys) (cons (first xs) ys)) yss)))))

;;; (sin-duplicados xs) es la lista obtenida eliminando los elementos
;;; duplicados de xs. Por ejemplo,
;;;    > (sin-duplicados '(3 2 3 2 5))
;;;    (3 2 5)
(defun sin-duplicados (xs)
  (remove-duplicates xs :test #'=))

(defun divisores-7 (n)
  (sin-duplicados
   (sort
    (mapcar #'(lambda (factors) (apply #'* 1 factors))
            (subsucesiones (divisores-primos n)))
    #'<)))

;;; 8ª definición
;;; =============

(defun divisores-8 (n)
  (sin-duplicados
   (sort
    (mapcar #'(lambda (factors) (apply #'* 1 factors))
            (subsucesiones (factor n)))
    #'<)))

;;; 9ª solución
;;; ===========

;;; (group xs) es la lista obtenida agrupando los elementos consecutivos
;;; iguales. Por ejemplo,
;;;    > (group '(2 2 2 3 5 5 3 2))
;;;    ((2 2 2) (3) (5 5) (3) (2))
(defun group (xs)
  (labels ((aux (xs ys zss)
             (cond
               ((null xs) (reverse (cons ys zss)))
               ((and ys (equal (first xs) (first ys)))
                (aux (rest xs) (cons (first xs) ys) zss))
               (t
                (aux (rest xs) (list (first xs)) (cons ys zss))))))
    (if xs
        (aux (rest xs) (list (first xs)) nil)
        nil)))

;;; (inits xs) es la lista de las sublistas iniciales de xs. Por ejemplo,
;;;    > (inits '(3 2 5))
;;;    ((3 2 5) (3 2) (3) NIL)
(defun inits (xs)
  (reverse
   (loop for i from 0 to (length xs)
         collect (subseq xs 0 i))))

;;; (producto-cartesiano xss) es el producto cartesiano de los conjuntos
;;; xss. Por ejemplo,
;;;    > (producto-cartesiano '((1 3) (2 5) (6 4)))
;;;    ((1 2 6) (1 2 4) (1 5 6) (1 5 4) (3 2 6) (3 2 4) (3 5 6) (3 5 4))
(defun producto-cartesiano (xss)
  (if (null xss)
      '(())
      (mapcan (lambda (xs)
                (mapcar (lambda (ys) (cons xs ys))
                        (producto-cartesiano (rest xss))))
              (first xss))))

(defun divisores-9 (n)
  (sort
   (mapcar (lambda (xss)
             (apply #'* (mapcan #'copy-list xss)))
           (producto-cartesiano
            (mapcar #'inits
                    (group (factor n)))))
   #'<))

;;; Verificación
;;; ============

(test divisores
  (mapc (lambda (divisores)
          (is (equal (funcall divisores 60)
                     '(1 2 3 4 5 6 10 12 15 20 30 60))))
        '(divisores-1
          divisores-2
          divisores-3
          divisores-4
          divisores-5
          divisores-6
          divisores-7
          divisores-8
          divisores-9)))

(defun verifica ()
  (run 'divisores))

;;; La verificación es
;;;    (verifica)
;;;
;;;    Running test DIVISORES ........


;;; Comprobación de equivalencia
;;; ============================

;;; La propiedad es
(test divisores-equiv
  (for-all ((n (gen-integer :min 2 :max 1000)))
    (let ((x (divisores-1 n)))
      (is (equal x (divisores-2 n)))
      (is (equal x (divisores-3 n)))
      (is (equal x (divisores-4 n)))
      (is (equal x (divisores-5 n)))
      (is (equal x (divisores-6 n)))
      (is (equal x (divisores-7 n)))
      (is (equal x (divisores-8 n)))
      (is (equal x (divisores-9 n))))))

(defun comprueba ()
  (run 'divisores-equiv))

;;; La comprobación es
;;;    > (comprueba)
;;;
;;;    Running test DIVISORES-EQUIV ...

;;; Comparación de la eficiencia
;;; ============================

;;; La comparación es
;;;    > (time (length (divisores-1 (factorial 11))))
;;;    0.819 seconds of real time
;;;    > (time (length (divisores-2 (factorial 11))))
;;;    0.817 seconds of real time
;;;    > (time (length (divisores-4 (factorial 11))))
;;;    2.453 seconds of real time
;;;    > (time (length (divisores-5 (factorial 11))))
;;;    0.687 seconds of real time
;;;    > (time (length (divisores-6 (factorial 11))))
;;;    0.000
;;;    > (time (length (divisores-7 (factorial 11))))
;;;    0.215 seconds of real time
;;;    > (time (length (divisores-8 (factorial 11))))
;;;    0.219 seconds of real time
;;;    > (time (length (divisores-9 (factorial 11))))
;;;    0.004 seconds of real time
;;;
;;;    > (time (length (divisores-6 (factorial 12))))
;;;    0.002
;;;    > (time (length (divisores-7 (factorial 12))))
;;;    7.779 seconds of real time
;;;    > (time (length (divisores-8 (factorial 12))))
;;;    7.360 seconds of real time
;;;    > (time (length (divisores-9 (factorial 12))))
;;;    0.005 seconds of real time
