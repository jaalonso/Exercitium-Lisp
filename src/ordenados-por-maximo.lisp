;;; ordenados-por-maximo.lisp
;;; Ordenación por el máximo.
;;; José A. Alonso Jiménez <https://jaalonso.github.io>
;;; Sevilla, 29-enero-2025
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; Definir la función ordenados-por-maximo tal que
;;; (ordenados-por-maximo xss) es la lista de los elementos de xss
;;; ordenada por sus máximos (se supone que los elementos de xss son
;;; listas no vacía) y cuando tiene el mismo máximo se conserva el orden
;;; original. Por ejemplo,
;;;    > (ordenados-por-maximo '((0 8) (9) (8 1) (6 3) (8 2) (6 1) (6 2)))
;;;    ((6 3) (6 1) (6 2) (0 8) (8 1) (8 2) (9))
;;; ---------------------------------------------------------------------

(ql:quickload '("alexandria"
                "fiveam")
              :silent t)

(defpackage :ordenados-por-maximo
  (:use :cl :alexandria :fiveam))

(in-package :ordenados-por-maximo)

;;; 1ª solución
;;; ===========

(defun ordenados-por-maximo1 (xss)
  (mapcar #'rest
          (sort (loop for xs in xss
                      for k from 0
                      collect (cons (cons (apply #'max xs) k) xs))
                #'<
                :key #'(lambda (pair) (first (first pair))))))

;;; 2ª solución
;;; ===========

;;; (maximo xs) el máximo elemento de la lista no vacía xs. Por ejemplo,
;;;    > (maximo '(3 7 2))
;;;    7
(defun maximo (xs)
  (reduce #'max xs))

(defun ordenados-por-maximo2 (xss)
  (stable-sort (copy-seq xss)
               #'(lambda (xs ys) (< (maximo xs) (maximo ys)))))

;;; 3ª solución
;;; ===========

(defun ordenados-por-maximo3 (xss)
  (stable-sort (copy-seq xss)
               #'< :key (lambda (xs) (reduce #'max xs))))

;;; 4ª solución
;;; ===========

(defun ordenados-por-maximo4 (xss)
  (let* ((con-maximos (mapcar (lambda (xs) (cons xs (maximo xs))) xss))
         (ordenados (sort con-maximos #'< :key #'cdr)))
    (mapcar #'car ordenados)))

;;; Verificación
;;; ============

(test ordenados-por-maximo
  (is (equal (ordenados-por-maximo1 '((0 8) (9) (8 1) (6 3) (8 2) (6 1) (6 2)))
             '((6 3) (6 1) (6 2) (0 8) (8 1) (8 2) (9))))
  (is (equal (ordenados-por-maximo2 '((0 8) (9) (8 1) (6 3) (8 2) (6 1) (6 2)))
             '((6 3) (6 1) (6 2) (0 8) (8 1) (8 2) (9))))
  (is (equal (ordenados-por-maximo3 '((0 8) (9) (8 1) (6 3) (8 2) (6 1) (6 2)))
             '((6 3) (6 1) (6 2) (0 8) (8 1) (8 2) (9))))
  (is (equal (ordenados-por-maximo4 '((0 8) (9) (8 1) (6 3) (8 2) (6 1) (6 2)))
             '((6 3) (6 1) (6 2) (0 8) (8 1) (8 2) (9))))
  )

(defun verifica ()
  (run 'ordenados-por-maximo))

;;; La verificación es
;;;    CL-USER> (ordenados-por-maximo::verifica)
;;;
;;;    Running test ORDENADOS-POR-MAXIMO ....

;;; Equivalencia de las definiciones
;;; ================================

;;; La propiedad es
(test ordenados-por-maximo-equiv
  (for-all ((xss (gen-list
                 :elements (gen-list
                            :elements (gen-integer)))))
    (let* ((yss (remove-if #'null xss))
           (r (ordenados-por-maximo1 yss)))
      (is (equal r (ordenados-por-maximo2 yss)))
      (is (equal r (ordenados-por-maximo2 yss)))
      (is (equal r (ordenados-por-maximo3 yss)))
      )))

(defun comprueba ()
  (run 'ordenados-por-maximo-equiv))

;;; La comprobación es
;;;    > (ordenados-por-maximo::comprueba)
;;;
;;;    Running test ORDENADOS-POR-MAXIMO-EQUIV ...
;;;    (#<IT.BESE.FIVEAM::FOR-ALL-TEST-PASSED {1003EDAF13}>)

;;; Comparación de eficiencia
;;; =========================

;;; (ejemplo n) es una lista con n elementos tal que su k-ésimo
;;; elementos son los números del 1 al k. Por ejemplo,
;;;    > (ejemplo 5)
;;;    ((1) (1 2) (1 2 3) (1 2 3 4) (1 2 3 4 5))
(defun ejemplo (n)
  (loop for k from 1 to n
        collect (loop for i from 1 to k
                      collect i)))

;;; La comparación es
;;;    MAXIMO> (time (length (ordenados-por-maximo1 (ejemplo 6000))))
;;;    Evaluation took:
;;;      0.807 seconds of real time
;;;      0.808579 seconds of total run time (0.601355 user, 0.207224 system)
;;;      [ Run times consist of 0.592 seconds GC time, and 0.217 seconds non-GC time. ]
;;;      100.25% CPU
;;;      1,608,804,177 processor cycles
;;;      288,620,640 bytes consed
;;;
;;;    6000 (13 bits, #x1770)
;;;
;;;    MAXIMO> (time (length (ordenados-por-maximo2 (ejemplo 6000))))
;;;    Evaluation took:
;;;      0.739 seconds of real time
;;;      0.741498 seconds of total run time (0.649615 user, 0.091883 system)
;;;      [ Run times consist of 0.288 seconds GC time, and 0.454 seconds non-GC time. ]
;;;      100.27% CPU
;;;      1,474,304,175 processor cycles
;;;      288,357,248 bytes consed
;;;
;;;    6000 (13 bits, #x1770)
;;;
;;;    MAXIMO> (time (length (ordenados-por-maximo3 (ejemplo 6000))))
;;;    Evaluation took:
;;;      0.821 seconds of real time
;;;      0.822926 seconds of total run time (0.679065 user, 0.143861 system)
;;;      [ Run times consist of 0.412 seconds GC time, and 0.411 seconds non-GC time. ]
;;;      100.24% CPU
;;;      1,636,125,125 processor cycles
;;;      288,374,832 bytes consed
;;;
;;;    6000 (13 bits, #x1770)
;;;
;;;    MAXIMO> (time (length (ordenados-por-maximo4 (ejemplo 6000))))
;;;    Evaluation took:
;;;      0.738 seconds of real time
;;;      0.740806 seconds of total run time (0.570585 user, 0.170221 system)
;;;      [ Run times consist of 0.444 seconds GC time, and 0.297 seconds non-GC time. ]
;;;      100.41% CPU
;;;      1,472,495,301 processor cycles
;;;      288,522,336 bytes consed
;;;
;;;    6000 (13 bits, #x1770)
