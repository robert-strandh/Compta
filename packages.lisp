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

(defpackage #:io
    (:use #:common-lisp)
  (:export #:*print-for-file-io*
	   #:define-save-info
           #:read-model
           #:write-model))

(defpackage #:compta-io
    (:use #:common-lisp #:compta-model #:io)
  (:export #:*compta-allowed-version-names*
	   #:*compta-current-version-name*))

(defpackage #:compta-gui
    (:use #:clim-lisp #:clim #:compta-model #:io #:compta-io)
  (:export #:compta))

