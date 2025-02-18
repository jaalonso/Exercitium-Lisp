(defpackage :tests
  (:use :cl
        :fiveam))

(in-package :tests)

(setf fiveam:*on-failure* :debug)

(defmacro verifica-uno (archivo)
  `(let* ((pkg-name (string-upcase ,archivo))
          (pkg (find-package pkg-name)))
     (when pkg
       (funcall (symbol-function (intern "VERIFICA" pkg))))))

(defun verifica-todos (archivos)
  (dolist (archivo archivos)
    (eval `(verifica-uno ,archivo))))

(verifica-todos
  '("suma"
    "longitud"
    "fibonacci"
    "ordenados-por-maximo"
    "bandera-tricolor"
    "posiciones-diagonales-principales"
    "diagonales-principales"
    "anagramas"
    "primos-equidistantes"
    "matriz-toeplitz"
    "maximos-locales"
    ))
