(in-package :cl-user)

(defpackage :n-moku-asd
  (:use :cl :asdf)
  (:documentation "n-moku narabe"))

(in-package :n-moku-asd)

(defsystem n-moku
  :description "n-moku"
  :version "0.1"
  :author "Makinori Ikegami"
  :licence "GPL"
  
  :depends-on (:ltk)
  
  :serial t
  :components
  ((:file "package")
   (:module lib
    :pathname "lib"
              :components ((:file "util")
                           (:file "lazy")))
   (:module src
    :pathname "src"
              :components ((:file "n-moku")
                           (:file "mcts")
                           (:file "gui")))))





