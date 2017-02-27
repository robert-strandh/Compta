(in-package :compta-io)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Save information for various model classes

(io:define-save-info compta-model:organization
  (:name compta-model:name)
  (:accounts compta-model:accounts)
  (:transactions compta-model:transactions))

(io:define-save-info compta-model:date
  (:year compta-model:year)
  (:month compta-model:month)
  (:day compta-model:day)
  (:hour compta-model:hour)
  (:minute compta-model:minute))

(io:define-save-info compta-model:account
  (:name compta-model:name))

(io:define-save-info compta-model:entry
  (:account compta-model:account)
  (:amount compta-model:amount))

(io:define-save-info compta-model:transaction
  (:name compta-model:name)
  (:date compta-model:date)
  (:creator compta-model:creator)
  (:debits compta-model:debits)
  (:credits compta-model:credits))

(defparameter *compta-allowed-version-names* '("ComptaV1"))
(defparameter *compta-current-version-name*  '("ComptaV1"))
