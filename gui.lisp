(in-package #:compta-gui)

(clim:define-application-frame compta ()
  ((%current-organization
    :initform (make-instance 'compta-model:organization :name "Home")
    :accessor current-organization))
  (:panes (main :application
                :width 800
                :height 500
                :display-function 'display-main)
          (accounts :application
                    :width 200
                    :height 650
                    :display-function 'display-accounts)
          (transactions :application
                        :width 300
                        :height 650
                        :display-function 'display-transactions)
          (inter :interactor
                 :width 500
                 :height 100))
  (:layouts (default
                (clim:horizontally
                    ()
                  (clim:vertically () main inter)
                  transactions
                  accounts))))

(defclass account-view (clim:view)
  ((%account :initarg :account :reader account)))

(defclass transaction-view (clim:view)
  ((%transaction :initarg :transaction :reader transaction)))

(defun display-oneline-transaction-summary (pane transaction modifiablep)
  (format pane "~a "
          (compta-model:iso-date-string (compta-model:date transaction)))
  (let ((object (if modifiablep
                    (make-instance 'name-changer :object transaction)
                    transaction))
        (type (if modifiablep 'name-changer 'compta-model:transaction)))
    (clim:with-output-as-presentation (pane object type)
      (format pane "~a~%"
              (compta-model:name transaction)))))

(defun display-oneline-account-summary (pane account)
  (clim:with-output-as-presentation (pane account 'compta-model:account)
    (format pane "~a~%" (compta-model:name account))))

(defgeneric display-main-with-view (frame pane view))

(defmethod display-main-with-view (frame pane view)
  (declare (ignore frame pane view))
  nil)

(defun format-amount (pane amount format)
  (multiple-value-bind
        (euros cents)
      (floor amount 100)
    (format pane format euros cents)))

(defun display-entry (pane transaction entry amount-format)
  (let ((medium (clim:sheet-medium pane)))
    (unless (null entry)
      (clim:with-output-as-presentation
          (pane transaction 'compta-model:transaction)
        (format pane "~a"
                (compta-model:iso-date-string (compta-model:date transaction)))
        (clim:with-text-family
            (medium :fixed)
          (format-amount pane (compta-model:amount entry) amount-format))
        (format pane "~a~%" (compta-model:name transaction))))))

(defmethod display-main-with-view (frame pane (view account-view))
  (declare (ignore frame))
  (let ((account (account view)))
    (display-oneline-account-summary pane account)
    (format pane "~%")
    (loop with frame = clim:*application-frame*
          with organization = (current-organization frame)
          for transaction in (reverse (compta-model:transactions organization))
          do (display-entry
                     pane
                     transaction
                     (find account (compta-model:debits transaction)
                           :key #'compta-model:account)
                     "~10d.~2,'0d~50t")
          do (display-entry
                     pane
                     transaction
                     (find account (compta-model:credits transaction)
                           :key #'compta-model:account)
                     "~30d.~2,'0d~50t"))))

(clim:define-presentation-type amount () :inherit-from 'integer)

(defclass entry-adder ()
  ((%adder :initarg :adder :reader adder)))

(defclass name-changer ()
  ((%object :initarg :object :reader object)))

(defun display-entry-adder (pane area-name push-entry entries)
  (let ((medium (clim:sheet-medium pane)))
    (flet ((show-entry (entry)
             (clim:with-text-family
                 (medium :fixed)
               (clim:with-output-as-presentation
                   (pane (compta-model:amount entry) 'amount)
                 (format-amount pane
                                (compta-model:amount entry)
                                "~10d.~2,'0d        ")))
             (clim:with-output-as-presentation
                 (pane (compta-model:account entry) 'compta-model:account)
               (format pane
                       "~a~%"
                       (compta-model:name (compta-model:account entry))))))
      (format pane "~a: " area-name)
      (let ((adder (make-instance 'entry-adder
                     :adder push-entry)))
        (clim:with-output-as-presentation (pane adder 'entry-adder)
          (format pane "[add]~%")))
      (loop for entry in (reverse entries)
            do (show-entry entry)))))

(defmethod display-main-with-view (frame pane (view transaction-view))
  (declare (ignore frame))
  (let ((transaction (transaction view)))
    (display-oneline-transaction-summary pane transaction t)
    (format pane "Created by: ~a~%~%~%" (compta-model:creator transaction))
    (display-entry-adder
     pane "Debits"
     (lambda (entry) (push entry (compta-model:debits transaction)))
     (compta-model:debits transaction))
    (display-entry-adder
     pane "Credits"
     (lambda (entry) (push entry (compta-model:credits transaction)))
     (compta-model:credits transaction))))

(defun display-main (frame pane)
  (display-main-with-view frame pane (clim:stream-default-view pane)))

(defun display-accounts (frame pane)
  (format pane "Accounts~%~%")
  (loop with organization = (current-organization frame)
        for account in (reverse (compta-model:accounts organization))
        do (display-oneline-account-summary pane account)))

(defun display-transactions (frame pane)
  (format pane "Transactions~%~%")
  (loop with organization = (current-organization frame)
        for transaction in (reverse (compta-model:transactions organization))
        do (display-oneline-transaction-summary pane transaction nil)))

(defun compta ()
  (clim:run-frame-top-level (clim:make-application-frame 'compta)))

(define-compta-command (com-quit :name t) ()
  (clim:frame-exit clim:*application-frame*))

(define-compta-command (com-new-account :name t) ((name 'string))
  (let ((organization (current-organization clim:*application-frame*)))
    (push (make-instance 'compta-model:account :name name)
          (compta-model:accounts organization))))

(define-compta-command (com-write-organization :name t) ((filename 'string))
  (io:write-model filename compta-io:*compta-current-version-name*
                  (current-organization clim:*application-frame*)))

(define-compta-command (com-read-organization :name t) ((filename 'pathname))
  (setf (current-organization clim:*application-frame*)
        (io:read-model filename compta-io:*compta-allowed-version-names*)))

(define-compta-command (com-read-organization-default :name t) ()
  (setf (current-organization clim:*application-frame*)
        (io:read-model "home" compta-io:*compta-allowed-version-names*)))

(define-compta-command (com-new-transaction :name t) ()
  (let* ((transaction (make-instance 'compta-model:transaction :name "unnamed"))
         (frame clim:*application-frame*)
         (organization (current-organization frame))
         (main-pane (clim:find-pane-named frame 'main)))
    (push transaction (compta-model:transactions organization))
    (setf (clim:stream-default-view main-pane)
          (make-instance 'transaction-view :transaction transaction))))

(define-compta-command (com-change-current-transaction-name :name t)
    ((name 'string))
  (let* ((pane (clim:find-pane-named clim:*application-frame* 'main))
         (view (clim:stream-default-view pane)))
    (setf (compta-model:name (transaction view)) name)))

(define-compta-command (com-edit-account :name t)
    ((account 'compta-model:account :gesture :select))
  (let ((pane (clim:find-pane-named clim:*application-frame* 'main)))
    (setf (clim:stream-default-view pane)
          (make-instance 'account-view :account account))))

(define-compta-command (com-edit-transaction :name t)
    ((transaction 'compta-model:transaction :gesture :select))
  (let ((pane (clim:find-pane-named clim:*application-frame* 'main)))
    (setf (clim:stream-default-view pane)
          (make-instance 'transaction-view :transaction transaction))))

(define-compta-command (com-delete-account :name t)
    ((account 'compta-model:account :gesture :delete))
  (let ((organization (current-organization clim:*application-frame*)))
    (setf (compta-model:accounts organization)
          (remove account (compta-model:accounts organization)))))

(define-compta-command (com-delete-transaction :name t)
    ((transaction 'compta-model:transaction))
  (let ((organization (current-organization clim:*application-frame*)))
    (setf (compta-model:transactions organization)
          (remove transaction (compta-model:transactions organization)))))

(clim:define-presentation-method clim:present
    (object (type compta-model:account) stream (view clim:textual-view) &key)
  (format stream "~a" (compta-model:name object)))

(clim:define-presentation-method clim:present
    (object (type compta-model:transaction) stream (view clim:textual-view) &key)
  (format stream "~a" (compta-model:name object)))

(define-compta-command (com-add-entry :name t)
    ((adder 'entry-adder :gesture :select))
  (funcall (adder adder)
           (make-instance 'compta-model:entry
                          :account (clim:accept 'compta-model:account)
                          :amount (clim:accept 'amount))))

(define-compta-command (com-change-name :name t)
    ((changer 'name-changer :gesture :select))
  (setf (compta-model:name (object changer))
        (clim:accept 'string)))
