(in-package #:compta-gui)

(define-application-frame compta ()
  ((%current-organization :initform (make-instance 'organization :name "Home")
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
	      (horizontally
	       ()
	       (vertically () main inter)
	       transactions
	       accounts))))

(defclass account-view (view)
  ((%account :initarg :account :reader account)))

(defclass transaction-view (view)
  ((%transaction :initarg :transaction :reader transaction)))

(defgeneric display-main-with-view (frame pane view))

(defmethod display-main-with-view (frame pane view)
  (declare (ignore frame pane view))
  nil)

(defmethod display-main-with-view (frame pane (view account-view))
  (declare (ignore frame))
  (format pane "Account: ~a~%~%" (name (account view)))
  (loop with account = (account view)
        with organization = (current-organization *application-frame*)
        for transaction in (reverse (transactions organization))
	do (display-entry pane view transaction
			  (find account (debits transaction) :key #'account)
			  "~10d.~2,'0d~50t")
	do (display-entry pane view transaction
			  (find account (credits transaction) :key #'account)
			  "~30d.~2,'0d~50t")))

(defun format-amount (pane amount format)
  (multiple-value-bind
      (euros cents)
      (floor amount 100)
    (format pane format euros cents)))

(defun display-entry (pane view transaction entry amount-format)
  (let ((medium (account view)))
    (unless (null entry)
      (with-output-as-presentation
       (pane transaction 'transaction)
       (format pane "~a" (iso-date-string (date transaction)))
       (with-text-family
	(medium :fixed)
	(format-amount pane (amount entry) amount-format))
       (format pane "~a~%" (name transaction))))))

(define-presentation-type amount () :inherit-from 'integer)

(defclass entry-adder ()
  ((%adder :initarg :adder :reader adder)))

(defclass name-changer ()
  ((%object :initarg :object :reader object)))

(defmethod display-main-with-view (frame pane (view transaction-view))
  (declare (ignore frame))
  (let ((transaction (transaction view))
        (medium (sheet-medium pane)))
    (flet ((show-entry (entry)
		       (with-text-family
			(medium :fixed)
			(with-output-as-presentation (pane (amount entry) 'amount)
						     (format-amount pane (amount entry) "~10d.~2,'0d        ")))
		       (with-output-as-presentation (pane (account entry) 'account)
						    (format pane "~a~%" (name (account entry))))))
      (format pane "Transaction name: ")
      (with-output-as-presentation (pane
                                    (make-instance 'name-changer
                                                   :object transaction)
                                    'name-changer)
				   (format pane "~a~%" (name transaction)))

      (format pane "Date: ~a~%Created by: ~a~%~%~%"
              (iso-date-string (date transaction))
              (creator transaction))
      (format pane "Debits: ")
      (let ((adder (make-instance 'entry-adder
                                  :adder (lambda (entry)
                                           (push entry
                                                 (debits transaction))))))
        (with-output-as-presentation (pane adder 'entry-adder)
				     (format pane "[add]~%")))
      (loop for entry in (reverse (debits transaction))
            do (show-entry entry))
      (format pane "Credits: ")
      (let ((adder (make-instance 'entry-adder
                                  :adder (lambda (entry)
                                           (push entry (credits transaction))))))
        (with-output-as-presentation (pane adder 'entry-adder)
				     (format pane "[add]~%")))
      (loop for entry in (reverse (credits transaction))
            do (show-entry entry)))))

(defun display-main (frame pane)
  (display-main-with-view frame pane (stream-default-view pane)))

(defun display-accounts (frame pane)
  (format pane "Accounts~%~%")
  (loop for account in (reverse (accounts (current-organization frame)))
        do (with-output-as-presentation (pane account 'account)
					(format pane "~a~%" (name account)))))

(defun display-transactions (frame pane)
  (format pane "Transactions~%~%")
  (loop for transaction in (reverse (transactions (current-organization frame)))
        do (with-output-as-presentation (pane transaction 'transaction)
					(format pane "~a ~a~%"
						(iso-date-string (date transaction))
						(name transaction)))))

(defun compta ()
  (run-frame-top-level (make-application-frame 'compta)))

(define-compta-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-compta-command (com-new-account :name t) ((name 'string))
  (push (make-instance 'account :name name)
        (accounts (current-organization *application-frame*))))

(define-compta-command (com-write-organization :name t) ((filename 'string))
  (write-model filename (current-organization *application-frame*)))

(define-compta-command (com-read-organization :name t) ((filename 'pathname))
  (setf (current-organization *application-frame*)
        (read-model filename)))

(define-compta-command (com-read-organization-default :name t) ()
  (setf (current-organization *application-frame*)
        (read-model "home")))

(define-compta-command (com-new-transaction :name t) ()
  (let ((transaction (make-instance 'transaction :name "unnamed")))
    (push transaction (transactions (current-organization *application-frame*)))
    (setf (stream-default-view (find-pane-named *application-frame* 'main))
          (make-instance 'transaction-view :transaction transaction))))

(define-compta-command (com-change-current-transaction-name :name t)
  ((name 'string))
  (let ((view (stream-default-view (find-pane-named *application-frame* 'main))))
    (setf (name (transaction view)) name)))

(define-compta-command (com-edit-account :name t)
  ((account 'account :gesture :select))
  (setf (stream-default-view (find-pane-named *application-frame* 'main))
        (make-instance 'account-view :account account)))

(define-compta-command (com-edit-transaction :name t)
  ((transaction 'transaction :gesture :select))
  (setf (stream-default-view (find-pane-named *application-frame* 'main))
        (make-instance 'transaction-view :transaction transaction)))

(define-compta-command (com-delete-account :name t)
  ((account 'account :gesture :delete))
  (let ((organization (current-organization *application-frame*)))
    (setf (accounts organization)
          (remove account (accounts organization)))))

(define-compta-command (com-delete-transaction :name t) ((transaction 'transaction))
  (let ((organization (current-organization *application-frame*)))
    (setf (transactions organization)
          (remove transaction (transactions organization)))))

(define-presentation-method present (object (type account)
					    stream (view textual-view) &key)
  (format stream "~a" (name object)))

(define-presentation-method present (object (type transaction)
					    stream (view textual-view) &key)
  (format stream "~a" (name object)))

(define-compta-command (com-add-entry :name t)
  ((adder 'entry-adder :gesture :select))
  (funcall (adder adder) 
           (make-instance 'entry
                          :account (accept 'account)
                          :amount (accept 'amount))))

(define-compta-command (com-change-name :name t)
  ((changer 'name-changer :gesture :select))
  (setf (name (object changer))
        (accept 'string)))

