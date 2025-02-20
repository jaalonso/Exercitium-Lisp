;;; lista-cuadrada.lisp
;;; Lista cuadrada.
;;; José A. Alonso Jiménez <https://jaalonso.github.io>
;;; Sevilla, 20-febrero-2025
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; Definir la función lista-cuadrada tal que (lista-cuadrada n x xs) es
;;; una lista de n listas de longitud n formadas con los elementos de xs
;;; completada con x, si no xs no tiene suficientes elementos. Por
;;; ejemplo,
;;;    > (lista-cuadrada 3 7 '(0 3 5 2 4))
;;;    ((0 3 5) (2 4 7) (7 7 7))
;;;    > (lista-cuadrada 2 7 '(0 3 5 2 4))
;;;    ((0 3) (5 2))
;;; ---------------------------------------------------------------------

(ql:quickload "fiveam" :silent t)
(ql:quickload "serapeum" :silent t)

(defpackage :lista-cuadrada
  (:use :cl :fiveam)
  (:import-from :serapeum batches))

(in-package :lista-cuadrada)

;;; 1ª solución
;;; ===========

;;; (take n xs) es la lista de los n primeros elementos de xs. Por
;;; ejemplo,
;;;    > (take 2 '(4 2 5 7 6))
;;;    (4 2)
(defun take (n xs)
  (cond ((= n 0) ())
        ((null xs) ())
        (t (cons (first xs) (take (- n 1) (rest xs))))))

;;; (drop n xs) es la lista obtenida eliminando los n primeros elementos
;;; de xs. Por ejemplo,
;;;    > (drop 2 '(4 2 5 7 6))
;;;    (5 7 6)
(defun drop (n xs)
  (cond ((= n 0) xs)
        ((null xs) ())
        (t (drop (- n 1) (rest xs)))))

;;; (grupos n xs) es la lista obtenida agrupando los elementos de xs en
;;; grupos de n elementos, salvo el último que puede tener menos. Por
;;; ejemplo,
;;;    > (grupos 2 '(4 2 5 7 6))
;;;    ((4 2) (5 7) (6))
(defun grupos (n xs)
  (if (null xs)
      ()
      (cons (take n xs) (grupos n (drop n xs)))))

(defun lista-cuadrada1 (n x xs)
  (take n
        (grupos n
                (append xs
                        (make-list (max 0 (- (* n n) (length xs)))
                                   :initial-element x)))))

;;; 2ª solución
;;; ===========

;;; (split-at n xs) son las listas de los n elementos de xs y lo que
;;; queda de xs al eliminar dichos elementos. Por ejemplo,
;;;    > (split-at 0 '(3 4 5 6 7))
;;;    NIL
;;;    (3 4 5 6 7)
;;;    > (split-at 2 '(3 4 5 6 7))
;;;    (3 4)
;;;    (5 6 7)
;;;    > (split-at 9 '(3 4 5 6 7))
;;;    (3 4 5 6 7)
;;;    NIL
(defun split-at (n xs)
  (defun aux (n ys zs)
    (if (or (= n 0) (null zs))
        (values (nreverse ys) zs)
        (aux (1- n)
             (cons (first zs) ys)
             (rest zs))))
  (aux n () xs))

;;; (grupos n xs) es la lista obtenida agrupando los elementos de xs en
;;; grupos de n elementos, salvo el último que puede tener menos. Por
;;; ejemplo,
;;;    > (grupos2 2 '(4 2 5 7 6))
;;;    ((4 2) (5 7) (6))
(defun grupos2 (n xs)
  (if (null xs)
      ()
      (multiple-value-bind (ys zs) (split-at n xs)
        (cons ys (grupos2 n zs)))))

(defun lista-cuadrada2 (n x xs)
  (take n
        (grupos2 n
                 (append xs
                         (make-list (max 0 (- (* n n) (length xs)))
                                    :initial-element x)))))

;;; 3ª solución
;;; ===========

;;; (take-iter n xs) es la lista de los n primeros elementos de xs. Por
;;; ejemplo,
;;;    > (take-iter 2 '(4 2 5 7 6))
;;;    (4 2)
;;;    > (take-iter 9 '(4 2 5 7 6))
;;;    (4 2 5 7 6)
(defun take-iter (n xs)
 (loop for i below n
       for x in xs
       collect x))

;;; (drop-iter n xs) es la lista obtenida eliminando los n primeros elementos
;;; de xs. Por ejemplo,
;;;    > (drop-iter 2 '(4 2 5 7 6))
;;;    (5 7 6)
;;;    > (drop-iter 9 '(4 2 5 7 6))
;;;    NIL
(defun drop-iter (n xs)
  (loop for i below (1+ n)
        for x on xs
        finally (return x)))

;;; (grupos-iter n xs) es la lista obtenida agrupando los elementos de xs en
;;; grupos de n elementos, salvo el último que puede tener menos. Por
;;; ejemplo,
;;;    > (grupos-iter 2 '(4 2 5 7 6))
;;;    ((4 2) (5 7) (6))
(defun grupos-iter (n xs)
  (let ((result '()))
    (loop while xs
          do (push (take-iter n xs) result)
          (setf xs (drop-iter n xs)))
    (nreverse result)))

(defun lista-cuadrada3 (n x xs)
  (take-iter n
             (grupos-iter n
                          (append xs
                                  (make-list (max 0 (- (* n n) (length xs)))
                                             :initial-element x)))))

;;; 4ª solución
;;; ===========

(defun lista-cuadrada4 (n x xs)
  (take n
        (serapeum:batches (append xs
                                  (make-list (max 0 (- (* n n) (length xs)))
                                             :initial-element x))
                          n)))

;;; Verificación
;;; ============

(test lista-cuadrada
  (mapc (lambda (lista-cuadrada)
          (is (equal (funcall lista-cuadrada 3 7 '(0 3 5 2 4))
                     '((0 3 5) (2 4 7) (7 7 7))))
          (is (equal (funcall lista-cuadrada 2 7 '(0 3 5 2 4))
                     '((0 3) (5 2)))))
        '(lista-cuadrada1
          lista-cuadrada2
          lista-cuadrada3
          lista-cuadrada4
          )))

(defun verifica ()
  (run 'lista-cuadrada))

;;; La verificación es
;;;    > (verifica)
;;;
;;;    Running test LISTA-CUADRADA ........

;;; Comprobación de la equivalencia
;;; ===============================

;;; La propiedad es
(test lista-cuadrada-equiv
  (for-all ((n (gen-integer :min 1 :max 10))
            (x (gen-integer :min 1 :max 10))
            (xs (gen-list)))
    (let ((ys (lista-cuadrada1 n x xs)))
      (is (equal ys (lista-cuadrada2 n x xs)))
      (is (equal ys (lista-cuadrada3 n x xs)))
      (is (equal ys (lista-cuadrada4 n x xs)))
      )))

(defun comprueba ()
  (run 'lista-cuadrada-equiv))

;;; La comprobación es
;;;    > (comprueba)
;;;
;;;    Running test LISTA-CUADRADA-EQUIV ...

;;; Comparación de eficiencia
;;; =========================

;;; La comparación es
;;;    > (time (lista-cuadrada1 2000 5 (make-list 100 :initial-element 0))) (values)
;;;    0.322 seconds of real time
;;;    > (time (lista-cuadrada2 2000 5 (make-list 100 :initial-element 0))) (values)
;;;    0.192 seconds of real time
;;;    (time (lista-cuadrada3 2000 5 (make-list 100 :initial-element 0))) (values)
;;;    0.207 seconds of real time
;;;    > (time (lista-cuadrada3 2000 5 (make-list 100 :initial-element 0))) (values)
;;;    0.155 seconds of real time
