(defpackage #:compta-model
    (:use #:common-lisp)
  (:export #:organization
           #:name
           #:accounts #:transactions
           #:account
           #:debits #:credits
           #:date #:iso-date-string
           #:year #:month #:day #:hour #:minute
           #:transaction
           #:creator
           #:entry
           #:amount
           #:*operator*))

(defpackage #:compta-io
    (:use #:common-lisp #:compta-model)
  (:export #:*print-for-file-io*
           #:read-organization
           #:write-organization))

(defpackage #:compta-gui
    (:use #:clim-lisp #:clim #:compta-model #:compta-io)
  (:export #:compta))

