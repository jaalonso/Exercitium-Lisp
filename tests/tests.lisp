(defpackage :tests
  (:use :cl
        :fiveam))

(in-package :tests)

(setf fiveam:*on-failure* :debug)

(defmacro verifica-uno (archivo)
  `(let* ((src-dir (uiop:merge-pathnames* #P"../src/" (uiop:getcwd)))
          (archivo-path (uiop:merge-pathnames* (make-pathname :name ,archivo :type "lisp") src-dir)))
     (load archivo-path)
     (let ((pkg (intern (string-upcase ,archivo))))
       (funcall (symbol-function (intern "VERIFICA" pkg))))))

(defun verifica-todos (archivos)
  (dolist (archivo archivos)
    (eval `(verifica-uno ,archivo))))

(verifica-todos
  '("suma"
    "longitud"
    "fibonacci"
    "ordenados-por-maximo"))
