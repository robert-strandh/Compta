(in-package :compta-io)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Save information for various model classes

(define-save-info organization
  (:name name) (:accounts accounts) (:transactions transactions))

(define-save-info date
  (:year year) (:month month) (:day day)
  (:hour hour) (:minute minute))

(define-save-info account
  (:name name))

(define-save-info entry
  (:account account) (:amount amount))

(define-save-info transaction
  (:name name) (:date date) (:creator creator)
  (:debits debits) (:credits credits))

(defparameter *compta-allowed-version-names* '("ComptaV1"))
(defparameter *compta-current-version-name*  '("ComptaV1"))
