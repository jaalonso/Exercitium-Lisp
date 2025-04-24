;;; sistema-factoradico-de-numeracion.hs
;;; Sistema factorádico de numeración.
;;; José A. Alonso Jiménez <https://jaalonso.github.io>
;;; Sevilla, 24-abril-2025
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; El [sistema factorádico](https://bit.ly/3KQZRue) es un sistema
;;; numérico basado en factoriales en el que el n-ésimo dígito, empezando
;;; desde la derecha, debe ser multiplicado por n! Por ejemplo, el número
;;; "341010" en el sistema factorádico es 463 en el sistema decimal ya
;;; que
;;;    3×5! + 4×4! + 1×3! + 0×2! + 1×1! + 0×0! = 463
;;;
;;; En este sistema numérico, el dígito de más a la derecha es siempre 0,
;;; el segundo 0 o 1, el tercero 0,1 o 2 y así sucesivamente.
;;;
;;; Con los dígitos del 0 al 9 el mayor número que podemos codificar es el
;;; 10!-1 = 3628799. En cambio, si lo ampliamos con las letras A a Z podemos
;;; codificar hasta 36!-1 = 37199332678990121746799944815083519999999910.
;;;
;;; Definir las funciones factoradico-a-decimal y decimalAfactoradico
;;; tales que
;;; + (factoradico-a-decimal cs) es el número decimal correspondiente al
;;;   número factorádico cs. Por ejemplo,
;;;      > (factoradico-a-decimal "341010")
;;;      463
;;;      > (factoradico-a-decimal "2441000")
;;;      2022
;;;      > (factoradico-a-decimal "A0000000000")
;;;      36288000
;;;      > (mapcar #'factoradico-a-decimal '("10" "100" "110" "200" "210" "1000" "1010" "1100" "1110" "1200"))
;;;      (1 2 3 4 5 6 7 8 9 10)
;;;      > (factoradico-a-decimal "3KXWVUTSRQPONMLKJIHGFEDCBA9876543210")
;;;      37199332678990121746799944815083519999999
;;; + (decimalAfactoradico n) es el número factorádico correpondiente al
;;;   número decimal n. Por ejemplo,
;;;      > (decimal-a-factoradico 463)
;;;      "341010"
;;;      > (decimal-a-factoradico 2022)
;;;      "2441000"
;;;      > (decimal-a-factoradico 36288000)
;;;      "A0000000000"
;;;      > (mapcar #'decimal-a-factoradico (loop for i from 1 to 10 collect i))
;;;      ("10" "100" "110" "200" "210" "1000" "1010" "1100" "1110" "1200")
;;;      > (decimal-a-factoradico 37199332678990121746799944815083519999999)
;;;      "3KXWVUTSRQPONMLKJIHGFEDCBA9876543210"
;;;
;;; Comprobar que, para cualquier entero positivo n,
;;;    (factoradico-a-decimal (decimalAfactoradico n)) == n
;;; ---------------------------------------------------------------------

(ql:quickload "fiveam" :silent t)

(defpackage :sistema-factoradico-de-numeracion
  (:use :cl
        :fiveam))

(in-package :sistema-factoradico-de-numeracion)

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

;;; (s-take n s) es la lista de los n primeros elementos de la lista
;;; infinita s.
;;;    > (s-take 10 (factoriales))
;;;    (1 1 2 6 24)
(defun s-take (n s)
  (if (zerop n)
      nil
      (cons (s-first s)
            (s-take (1- n) (s-rest s)))))

;;; (factoriales) es la lista de los factoriales. Por ejemplo,
;;;    > (s-take 10 (factoriales))
;;;    (1 1 2 6 24 120 720 5040 40320 362880)
(defun factoriales (&optional (n 0) (f 1))
  (s-cons f (factoriales (1+ n) (* (1+ n) f))))

;;; 1ª definición de factoradico-a-decimal
;;; ======================================

;;; caracteres es la lista de caracteres
(defparameter *caracteres*
  (concatenate 'string
               "0123456789"
               "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

;;; (caracter-a-entero c) es la posición del carácter c en la lista de
;;; caracteres. Por ejemplo,
;;;    (caracter-a-entero #\0)  ==  0
;;;    (caracter-a-entero #\1)  ==  1
;;;    (caracter-a-entero #\9)  ==  9
;;;    (caracter-a-entero #\A)  ==  10
;;;    (caracter-a-entero #\B)  ==  11
;;;    (caracter-a-entero #\Z)  ==  35
(defun caracter-a-entero (c)
  (position c *caracteres*))

(defun factoradico-a-decimal-1 (cs)
  (let* ((xs (map 'list #'caracter-a-entero cs))
         (n (length cs))
         (ys (reverse (s-take n (factoriales)))))
    (reduce #'+ (mapcar #'* xs ys))))

;;; 2ª definición de factoradico-a-decimal
;;; ======================================

;;; *caracteres-enteros* es la tabla que asigna a cada carácter su
;;; posición correspondiente, desde 0 hasta 35. Por ejemplo,
;;;      > (gethash #\A *caracteres-enteros*)
;;;      10
(defparameter *caracteres-enteros*
  (let ((tabla (make-hash-table)))
    (loop for i from 0 to 35
          for c across *caracteres*
          do (setf (gethash c tabla) i))
    tabla))

;;; (caracter-a-entero-2 c) es la posición del carácter c en la lista de
;;; caracteres. Por ejemplo,
;;;    (caracter-a-entero-2 #\0)  ==  0
;;;    (caracter-a-entero-2 #\1)  ==  1
;;;    (caracter-a-entero-2 #\9)  ==  9
;;;    (caracter-a-entero-2 #\A)  ==  10
;;;    (caracter-a-entero-2 #\B)  ==  11
;;;    (caracter-a-entero-2 #\Z)  ==  35
(defun caracter-a-entero-2 (c)
  (gethash c *caracteres-enteros*))

(defun factoradico-a-decimal-2 (cs)
  (let* ((xs (map 'list #'caracter-a-entero-2 cs))
         (n (length cs))
         (ys (reverse (s-take n (factoriales)))))
    (reduce #'+ (mapcar #'* xs ys))))

;;; 3ª definición de factoradico-a-decimal
;;; ======================================

;;; (caracter-a-entero-3 c) es la posición del carácter c en la lista de
;;; caracteres. Por ejemplo,
;;;    (caracter-a-entero-3 #\0)  ==  0
;;;    (caracter-a-entero-3 #\1)  ==  1
;;;    (caracter-a-entero-3 #\9)  ==  9
;;;    (caracter-a-entero-3 #\A)  ==  10
;;;    (caracter-a-entero-3 #\B)  ==  11
;;;    (caracter-a-entero-3 #\Z)  ==  35
(defun caracter-a-entero-3 (c)
    (loop for x across *caracteres*
          for i from 0
          when (char= x c) return i))

(defun factoradico-a-decimal-3 (cs)
  (reduce #'+
          (mapcar #'*
                  (s-take (length cs) (factoriales))
                  (reverse (map 'list #'caracter-a-entero-3 cs)))))

;;; 1ª definición de decimalAfactoradico
;;; ====================================

;;; (entero-a-caracter k) es el k-ésimo elemento de la lista
;;; de los caracteres. Por ejemplo,
;;;    (entero-a-caracter 0)   ==  #\0
;;;    (entero-a-caracter 1)   ==  #\1
;;;    (entero-a-caracter 9)   ==  #\9
;;;    (entero-a-caracter 10)  ==  #\A
;;;    (entero-a-caracter 11)  ==  #\B
;;;    (entero-a-caracter 35)  ==  #\Z
(defun entero-a-caracter (k)
  (char *caracteres* k))

;;; (s-take-while p s) es la lista de los primeros elementos de la lista
;;; infinita s que verifica el predicado p. Por ejemplo,
;;;    > (s-take-while (lambda (x) (< x 30)) (factoriales))
;;;    (0 1 1 2 3 5 8 13 21)
(defun s-take-while (p s)
  (if (not (funcall p (s-first s)))
      nil
      (cons (s-first s)
            (s-take-while p (s-rest s)))))

(defun decimal-a-factoradico-1 (n)
  (labels ((aux (m xs)
             (if (= m 0)
                 (mapcar (constantly #\0) xs)
                 (let ((x (first xs)))
                   (cons (entero-a-caracter (truncate m x))
                         (aux (mod m x) (rest xs)))))))
    (coerce (aux n (reverse (s-take-while (lambda (x) (<= x n))
                                          (factoriales))))
            'string)))

;;; 2ª definición de decimalAfactoradico
;;; ====================================

;;; *enteros-caracteres* es la tabla cuyas claves son los número de 0
;;; a 35 y las claves son los caracteres. Por ejemplo,
;;;    > (gethash 10 *enteros-caracteres*)
;;;    #\A
(defparameter *enteros-caracteres*
  (let ((hash (make-hash-table)))
    (loop for i from 0 to 35
          for c across *caracteres*
          do (setf (gethash i hash) c))
    hash))

;;; (entero-a-caracter-2 k) es el k-ésimo elemento de la lista
;;; ['0', '1',..., '9', 'A', 'B',..., 'Z']. . Por ejemplo,
;;;    (entero-a-caracter-2 0)   ==  #\0
;;;    (entero-a-caracter-2 1)   ==  #\1
;;;    (entero-a-caracter-2 9)   ==  #\9
;;;    (entero-a-caracter-2 10)  ==  #\A
;;;    (entero-a-caracter-2 11)  ==  #\B
;;;    (entero-a-caracter-2 35)  ==  #\Z
(defun entero-a-caracter-2 (k)
  (gethash k *enteros-caracteres*))

(defun decimal-a-factoradico-2 (n)
  (labels ((aux (m xs)
             (if (= m 0)
                 (mapcar (constantly #\0) xs)
                 (let ((x (first xs)))
                   (cons (entero-a-caracter-2 (truncate m x))
                         (aux (mod m x) (rest xs)))))))
    (coerce (aux n (reverse (s-take-while (lambda (x) (<= x n))
                                          (factoriales))))
            'string)))

;;; 3ª definición de decimalAfactoradico
;;; ====================================

;;; (entero-a-caracter-3 k) es el k-ésimo elemento de la lista de los
;;; caracteres. Por ejemplo,
;;;    (entero-a-caracter-3 0)   ==  #\0
;;;    (entero-a-caracter-3 1)   ==  #\1
;;;    (entero-a-caracter-3 9)   ==  #\9
;;;    (entero-a-caracter-3 10)  ==  #\A
;;;    (entero-a-caracter-3 11)  ==  #\B
;;;    (entero-a-caracter-3 35)  ==  #\Z
(defun entero-a-caracter-3 (k)
  (char *caracteres* k))

(defun decimal-a-factoradico-3 (n)
  (labels ((aux (m xs)
             (if (= m 0)
                 (mapcar (constantly #\0) xs)
                 (let ((x (first xs)))
                   (cons (entero-a-caracter-3 (truncate m x))
                         (aux (mod m x) (rest xs)))))))
    (coerce (aux n (reverse (s-take-while (lambda (x) (<= x n))
                                          (factoriales))))
            'string)))

;;; Verificación
;;; ============

(test factoradico-a-decimal
  (mapc (lambda (factoradico-a-decimal)
          (is (= (funcall factoradico-a-decimal "341010") 463))
          (is (= (funcall factoradico-a-decimal "2441000") 2022))
          (is (= (funcall factoradico-a-decimal "A0000000000") 36288000)))
        '(factoradico-a-decimal-1
          factoradico-a-decimal-2
          factoradico-a-decimal-3)))

(test decimal-a-factoradico
  (mapc (lambda (decimal-a-factoradico)
          (is (equal (funcall decimal-a-factoradico 463) "341010"))
          (is (equal (funcall decimal-a-factoradico 2022) "2441000"))
          (is (equal (funcall decimal-a-factoradico 36288000) "A0000000000")))
        '(decimal-a-factoradico-1
          decimal-a-factoradico-2
          decimal-a-factoradico-3
          )))

(defun verifica ()
  (run 'factoradico-a-decimal)
  (run 'decimal-a-factoradico))

;;; La verificación es
;;;    (verifica)
;;;
;;;    Running test FACTORADICO-A-DECIMAL ........


;;; Propiedad de inverso
;;; ====================

;;; La propiedad es
(test factoradico
  (for-all ((n (gen-integer :min 0)))
    (is (= (factoradico-a-decimal-1 (decimal-a-factoradico-1 n)) n))
    (is (= (factoradico-a-decimal-2 (decimal-a-factoradico-2 n)) n))
    (is (= (factoradico-a-decimal-3 (decimal-a-factoradico-3 n)) n))))

(defun comprueba ()
  (run 'factoradico))

;;; La comprobación es
;;;    > (comprueba)
;;;
;;;    Running test SUMA-CADENAS-EQUIV ...
