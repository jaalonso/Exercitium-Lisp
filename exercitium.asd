(defsystem :exercitium
  :serial t
  :depends-on (:fiveam
               :cl-factoring
               :serapeum
               :alexandria)
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
                             (:file "cuadrado-mas-cercano")
                             (:file "suma-de-cadenas")
                             (:file "sistema-factoradico-de-numeracion")
                             (:file "duplicacion-de-cada-elemento")
                             (:file "conjunto-de-divisores")
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
