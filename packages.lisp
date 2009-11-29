(defpackage #:compta-model
    (:use #:common-lisp)
  (:export #:organization
           #:name
           #:account #:transaction
           #:accounts #:transactions
           #:debits #:credits
           #:date #:iso-date-string
           #:creator
           #:*operator*))

(defpackage #:compta-gui
    (:use #:clim-lisp #:clim #:compta-model)
  (:export #:compta))

