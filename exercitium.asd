(defsystem :exercitium
  :serial t
  :depends-on (:fiveam
               :serapeum)
  :components ((:module "src"
                :components ((:file "suma")
                             (:file "longitud")
                             (:file "ordenados-por-maximo")
                             (:file "bandera-tricolor")
                             (:file "posiciones-diagonales-principales")
                             (:file "diagonales-principales")
                             (:file "anagramas")
                             (:file "primos-equidistantes")
                             (:file "matriz-toeplitz")
                             (:file "maximos-locales")
                             (:file "lista-cuadrada")
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
