(defpackage :suma
  (:use :cl
        :fiveam))

(in-package :suma)

(defun suma (a b)
  (+ a b))

(test suma
  (is (= (suma 2 3) 5))
  (is (= (suma 2 4) 6))
  )

(test suma-prop
  (for-all ((a (gen-integer :min 1 :max 10))
            (b (gen-integer :min 1 :max 10)))
    (is (= (+ a b) (+ b a)))))

(defun comprueba ()
  (run 'suma-prop))

(defun verifica ()
  (run 'suma))

;;; Ejemplo de uso
;;;    CL-USER> (suma::comprueba)
;;;
;;;    Running test SUMA-ALEATORIO ...
;;;    (#<IT.BESE.FIVEAM::FOR-ALL-TEST-PASSED {1004647353}>)
;;;    CL-USER> (suma::verifica)
;;;
;;;    Running test SUMA-SIMPLE .
;;;    (#<IT.BESE.FIVEAM::TEST-PASSED {1004658953}>)
