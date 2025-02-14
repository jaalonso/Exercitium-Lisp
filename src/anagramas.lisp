;;; anagramas.lisp
;;; Anagramas.
;;; José A. Alonso Jiménez <https://jaalonso.github.io>
;;; Sevilla, 10-febrero-2025
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; Una palabra es una anagrama de otra si se puede obtener permutando
;;; sus letras. Por ejemplo, "mora" y "roma" son anagramas de "amor".
;;;
;;; Definir la función anagramas tal que (anagramas x ys) es la lista de
;;; los elementos de ys que son anagramas de x. Por ejemplo,
;;;    > (anagramas-1 "amor" '("Roma" "mola" "loma" "moRa" "rama"))
;;;    ("Roma" "moRa")
;;;    > (anagramas-1 "rama" '("aMar" "amaRa" "roMa" "marr" "aRma"))
;;;    ("aMar" "aRma")
;;; ---------------------------------------------------------------------

(ql:quickload "fiveam" :silent t)

(defpackage :anagramas
  (:use :cl
        :fiveam))

(in-package :anagramas)

;;; 1ª solución
;;; ===========

;;; (normalizada s) es la cadena s en minúscula y ordenada. Por ejemplo,
;;;    > (normalizada "RoManA")
;;;    "aamnor"
(defun normalizada (s)
  (coerce (sort (map 'list #'char-downcase s) #'char<) 'string))

;;; (son-anagramas xs ys) se verifica si xs e ys son anagramas. Por
;;; ejemplo,
;;;    > (son-anagramas "amor" "Roma")
;;;    T
;;;    > (son-anagramas "amor" "mola")
;;;    NIL
(defun son-anagramas (xs ys)
  (equal (normalizada xs) (normalizada ys)))

(defun anagramas-1 (x ys)
  (cond ((null ys) nil)
        ((son-anagramas x (car ys))
         (cons (car ys) (anagramas-1 x (cdr ys))))
        (t (anagramas-1 x (cdr ys)))))

;;; 2ª solución
;;; ===========

(defun anagramas-2 (x ys)
  (mapcan (lambda (y) (if (son-anagramas x y) (list y) nil)) ys))

;;; 3ª solución
;;; ===========

(defun anagramas-3 (x ys)
  (remove-if-not (lambda (y) (son-anagramas x y)) ys))

;;; 4ª solución
;;; ===========

(defun anagramas-4 (x ys)
  (loop for y in ys
        when (son-anagramas x y)
          collect y))

;;; 5ª solución
;;; ===========

(defun anagramas-5 (x ys)
  (reduce (lambda (acc y)
            (if (son-anagramas x y)
                (append acc (list y))
                acc))
          ys
          :initial-value '()))

;;; 6ª solución
;;; ===========

(defun anagramas-6 (x ys)
  (mapcan (lambda (y)
            (if (son-anagramas x y)
                (list y)
                '()))
          ys))

;;; 7ª solución
;;; ===========

(defun anagramas-7 (x ys)
  (let ((x-normalizada (normalizada x))
        (ys-normalizada (mapcar #'normalizada ys)))
    (loop for y in ys
          for y-normalizada in ys-normalizada
          when (equal x-normalizada y-normalizada)
          collect y)))

;;; 8ª solución
;;; ===========

;;; (frecuencia s) es el diccionario con los caracteres de l cadena s
;;; junto con su número de ocurrencias en s. Por ejemplo,
;;;    > (ql:quickload :alexandria :silent t)
;;;    (:ALEXANDRIA)
;;;    > (alexandria:hash-table-alist (frecuencia "Sevillanas"))
;;;    ((#\n . 1) (#\a . 2) (#\l . 2) (#\i . 1) (#\v . 1) (#\e . 1) (#\s . 2))
(defun frecuencia (s)
  (let ((histograma (make-hash-table :test #'equal)))
    (map nil
         (lambda (c)
           (incf (gethash (char-downcase c) histograma 0)))
         s)
    histograma))

;;; (son-anagramas-frecuencia xs ys) se verifica si xs e ys son anagramas. Por
;;; ejemplo,
;;;    > (son-anagramas-frecuencia "amor" "Roma")
;;;    T
;;;    > (son-anagramas-frecuencia "amor" "mola")
;;;    NIL
(defun son-anagramas-frecuencia (xs ys)
  (let ((hx (frecuencia xs))
        (hy (frecuencia ys)))
    (equalp hx hy)))

(defun anagramas-8 (x ys)
  (let ((hx (frecuencia x)))
    (loop for y in ys
          when (equalp hx (frecuencia y))
            collect y)))


;;; Verificación
;;; ============

(test anagramas
  (mapc (lambda (anagramas)
          (is (equal (funcall anagramas "amor" '("Roma" "mola" "loma" "moRa" "rama"))
                     '("Roma" "moRa")))
          (is (equal (funcall anagramas "rama" '("aMar" "amaRa" "roMa" "marr" "aRma"))
                     '("aMar" "aRma"))))
        '(anagramas-1
          anagramas-2
          anagramas-3
          anagramas-4
          anagramas-5
          anagramas-6
          anagramas-7
          anagramas-8)))

(defun verifica ()
  (run 'anagramas))

;;; La verificación es
;;;    > (verifica)
;;;
;;;    Running test ANAGRAMAS ................

;;; Comparación de eficiencia
;;; =========================


;;; (cadena n) genera una cadena aleatoria con n letras minúsculas. Por
;;; ejemplo,
;;;    ANAGRAMAS> (cadena 10)
;;;    "wozmfbldfe"
;;;    ANAGRAMAS> (cadena 10)
;;;    "qhezbrtxks"
(defun cadena (n)
  (let ((alfabeto "abcdefghijklmnopqrstuvwxyz")
        (longitud (length "abcdefghijklmnopqrstuvwxyz")))
    (coerce
     (loop repeat n
           collect (char alfabeto (random longitud)))
     'string)))

;;; (cadenas m n) es la lista otenida repitiendo m veces (cadena n). Por
;;; ejemplo,
;;;    ANAGRAMAS> (cadenas 3 10)
;;;    ("yznrohjnli" "mpafmnggcn" "oosulshzcq")
(defun cadenas (m n)
  (loop repeat m collect (cadena n)))

;;; La comparación es
;;;    > (setf ej-cadenas (cadenas 100 (expt 10 5))) (values)
;;;    > (time (length (anagramas-1 (first ej-cadenas) ej-cadenas)))
;;;    4.108 seconds of real time
;;;    > (time (length (anagramas-2 (first ej-cadenas) ej-cadenas)))
;;;    4.263 seconds of real time
;;;    > (time (length (anagramas-3 (first ej-cadenas) ej-cadenas)))
;;;    4.179 seconds of real time
;;;    > (time (length (anagramas-4 (first ej-cadenas) ej-cadenas)))
;;;    4.109 seconds of real time
;;;    > (time (length (anagramas-5 (first ej-cadenas) ej-cadenas)))
;;;    4.419 seconds of real time
;;;    > (time (length (anagramas-6 (first ej-cadenas) ej-cadenas)))
;;;    4.129 seconds of real time
;;;    > (time (length (anagramas-7 (first ej-cadenas) ej-cadenas)))
;;;    2.136 seconds of real time
;;;    > (time (length (anagramas-8 (first ej-cadenas) ej-cadenas)))
;;;    0.629 seconds of real time
