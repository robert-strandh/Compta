(in-package #:cl-user)

(asdf:defsystem :compta
  :depends-on (:mcclim)
  :components
  ((:file "packages" :depends-on ())
   (:file "io" :depends-on ("packages"))
   (:file "model" :depends-on ("packages"))
   (:file "compta-io" :depends-on ("packages" "model" "io"))
   (:file "gui" :depends-on ("packages" "model"))))
