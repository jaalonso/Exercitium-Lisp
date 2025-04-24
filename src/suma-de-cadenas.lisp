;;; suma-de-cadenas.lisp
;;; Suma de cadenas.
;;; José A. Alonso Jiménez <https://jaalonso.github.io>
;;; Sevilla, 23-abril-2025
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; Definir la función suma-cadenas tal que (suma-cadenas xs ys) es la
;;; cadena formada por el número entero que es la suma de los números
;;; enteros cuyas cadenas que lo representan son xs e ys; además, se
;;; supone que la cadena vacía representa al cero. Por ejemplo,
;;;    (suma-cadenas "2"   "6")  == "8"
;;;    (suma-cadenas "14"  "2")  == "16"
;;;    (suma-cadenas "14"  "-5") == "9"
;;;    (suma-cadenas "-14" "-5") == "-19"
;;;    (suma-cadenas "5"   "-5") == "0"
;;;    (suma-cadenas ""    "5")  == "5"
;;;    (suma-cadenas "6"   "")   == "6"
;;;    (suma-cadenas ""    "")   == "0"
;;; ---------------------------------------------------------------------

(ql:quickload "fiveam" :silent t)

(defpackage :suma-de-cadenas
  (:use :cl
        :fiveam))

(in-package :suma-de-cadenas)

;;; 1ª solución
;;; ===========

;;; (numero xs) es el número entero representado por la cadena xs
;;; suponiendo que la cadena vacía representa al cero.. Por ejemplo,
;;;    (numero "12")   ==  12
;;;    (numero "-12")  ==  -12
;;;    (numero "0")    ==  0
;;;    (numero "")     ==  0
(defun numero (s)
  (if (string= s "")
      0
      (parse-integer s)))

(defun suma-cadenas1 (xs ys)
  (write-to-string (+ (numero xs) (numero ys))))

;;; 2ª solución
;;; ===========

(defun suma-cadenas2 (xs ys)
  (cond
    ((and (string= xs "") (string= ys "")) "0")
    ((string= xs "") ys)
    ((string= ys "") xs)
    (t (write-to-string (+ (parse-integer xs) (parse-integer ys))))))

;;; 3ª solución
;;; ===========

(defun suma-cadenas3 (xs ys)
  (write-to-string
   (reduce #'+
           (mapcar #'parse-integer
                   (remove-if (lambda (zs) (string= zs "")) (list xs ys)))
           :initial-value 0)))

;;; Verificación
;;; ============

(test suma-cadenas
  (mapc (lambda (suma-cadenas)
          (is (equal (funcall suma-cadenas "2"   "6")  "8"))
          (is (equal (funcall suma-cadenas "14"  "2")  "16"))
          (is (equal (funcall suma-cadenas "14"  "-5") "9"))
          (is (equal (funcall suma-cadenas "-14" "-5") "-19"))
          (is (equal (funcall suma-cadenas "5"   "-5") "0"))
          (is (equal (funcall suma-cadenas ""    "5")  "5"))
          (is (equal (funcall suma-cadenas "6"   "")   "6"))
          (is (equal (funcall suma-cadenas ""    "")   "0")) )
        '(suma-cadenas1
          suma-cadenas2
          suma-cadenas3)))

(defun verifica ()
  (run 'suma-cadenas))

;;; La verificación es
;;;    (verifica)
;;;
;;;    Running test SUMA-CADENAS ........

;;; Equivalencia de las definiciones
;;; ================================

;;; La propiedad es
(test suma-cadenas-equiv
  (for-all ((x (gen-integer))
            (y (gen-integer)))
    (let ((xs (write-to-string x))
          (ys (write-to-string y)))
      (is (string= (suma-cadenas1 xs ys)
                   (suma-cadenas2 xs ys)))
      (is (string= (suma-cadenas1 xs ys)
                   (suma-cadenas2 xs ys))))))

(defun comprueba ()
  (run 'suma-cadenas-equiv))

;;; La comprobación es
;;;    > (comprueba)
;;;
;;;    Running test SUMA-CADENAS-EQUIV ...

;;; Comparación de eficiencia
;;; =========================

;;; La comparación es
;;;    > (defvar c1 (write-to-string (expt 10 10000)))
;;;    > (defvar c2 (write-to-string (- (expt 10 10000))))
;;;    > (time (suma-cadenas1 c1 c2))
;;;    0.050 seconds of real time
;;;    > (time (suma-cadenas2 c1 c2))
;;;    0.051 seconds of real time
;;;    > (time (suma-cadenas3 c1 c2))
;;;    0.036 seconds of real time
