(in-package #:cl-user)

(asdf:defsystem :compta
  :depends-on (:mcclim)
  :components
  ((:file "packages" :depends-on ())
   (:file "model" :depends-on ("packages"))
   (:file "gui" :depends-on ("packages" "model"))
   ))
