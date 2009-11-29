(in-package #:cl-user)

(asdf:defsystem :compta
  :depends-on (:mcclim)
  :components
  ((:file "packages" :depends-on ())
   (:file "model" :depends-on ("packages"))
   (:file "io" :depends-on ("packages" "model"))
   (:file "gui" :depends-on ("packages" "model"))
   ))
