(cl:in-package #:asdf-user)

(defsystem :compta
  :depends-on (:mcclim)
  :serial t
  :components
  ((:file "packages")
   (:file "io")
   (:file "model")
   (:file "compta-io")
   (:file "gui")))
