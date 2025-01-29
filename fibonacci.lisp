(defpackage :fibonacci
  (:use :cl
        :fiveam))

(in-package :fibonacci)

(defun fibonacci (n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (t (+ (fibonacci (- n 1))
              (fibonacci (- n 2))))))

(test fibonacci
  (is (= 8 (fibonacci 5)))
  (is (= 89 (fibonacci 10)))
  )

(defun verifica ()
  (run 'fibonacci))

;;; Eficiencia
;;;    CL-USER> (time (fibonacci::fibonacci 30))
;;;    Evaluation took:
;;;      0.040 seconds of real time
;;;      0.040100 seconds of total run time (0.040100 user, 0.000000 system)
;;;      100.00% CPU
;;;      79,908,493 processor cycles
;;;      0 bytes consed
;;;
;;;    1346269 (21 bits, #x148ADD)

;;; Ejemplo de uso
;;;    CL-USER> (fibonacci::verifica)
;;;
;;;    Running test FIBONACCI ..
;;;    (#<IT.BESE.FIVEAM::TEST-PASSED {1002320BD3}>
;;;     #<IT.BESE.FIVEAM::TEST-PASSED {1002320A83}>)
