(cl:in-package #:asdf-user)

(defsystem :compta
  :depends-on (:mcclim)
  :components
  ((:file "packages")
   (:file "io")
   (:file "model")
   (:file "compta-io")
   (:file "gui")))
