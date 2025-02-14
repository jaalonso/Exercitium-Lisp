(defsystem :exercitium
  :serial t
  :depends-on (:fiveam)
  :components ((:module "src"
                :components ((:file "suma")
                             (:file "longitud")
                             (:file "ordenados-por-maximo")
                             (:file "bandera-tricolor")
                             (:file "posiciones-diagonales-principales")
                             (:file "diagonales-principales")
                             (:file "anagramas")
                             (:file "primos-equidistantes")
                             ))))

(defsystem :exercitium/tests
  :depends-on (:exercitium)
  :serial t
  :components ((:module "tests"
                :components ((:file "tests")))))

;;; Ejemplo de uso:
;;;    CL-USER> (asdf:load-system "exercitium")
;;;    T
;;;    CL-USER> (asdf:load-system "exercitium/tests")
;;;
;;;    Running test SUMA ..
;;;    Running test LONGITUD ..
;;;    T
