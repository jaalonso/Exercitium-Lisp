(defpackage :longitud
  (:use :cl
        :fiveam))

(in-package :longitud)

(defun longitud (xs)
  (if (null xs)
      0
      (+ 1 (longitud (rest xs)))))

(test longitud
  (is (= 3 (longitud '(4 2 5))))
  (is (= 4 (longitud '(4 2 5 6))))
  )

(test longitud-prop
  (for-all ((xs (gen-list))
            (ys (gen-list)))
    (is (= (longitud (append xs ys))
           (+ (longitud xs)
              (longitud ys))))))

(defun verifica ()
  (run 'longitud))

(defun comprueba ()
  (run 'longitud-prop))

;;; Ejemplo de uso
;;;    CL-USER> (longitud::verifica)
;;;
;;;    Running test LONGITUD-SIMPLE ..
;;;     Did 2 checks.
;;;        Pass: 2 (100%)
;;;        Skip: 0 ( 0%)
;;;        Fail: 0 ( 0%)
;;;
;;;
;;;    Running test LONGITUD-ALEATORIO ...
;;;     Did 1 check.
;;;        Pass: 1 (100%)
;;;        Skip: 0 ( 0%)
;;;        Fail: 0 ( 0%)
;;;
;;;    T
;;;    NIL
;;;    NIL
