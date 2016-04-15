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
  
  :depends-on ()
  
  :components ((:file "package")
               (:file "util")
               (:file "lazy")
               (:file "n-moku" :depends-on ("util"))))
  


