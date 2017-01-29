(cl:in-package #:asdf-user)

(defsystem :compta
  :description "Simple accounting system"
  :author "Robert Strandh <robert.strandh@gmail.com>"
  :license "FreeBSD, see file LICENSE.text"
  :depends-on (:mcclim)
  :serial t
  :components
  ((:file "packages")
   (:file "io")
   (:file "model")
   (:file "compta-io")
   (:file "gui")))
