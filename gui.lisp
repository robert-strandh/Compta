(in-package #:compta-gui)

(define-application-frame compta ()
  ((%current-organization :initform (make-instance 'organization)
                          :accessor current-organization))
  (:panes (main :application
		:width 500
		:height 500
                :display-function 'display-main)
	  (accounts :application
                    :width 200
                    :height 650
                    :display-function 'display-accounts)
          (transactions :application
                        :width 200
                        :height 650
                        :display-function 'display-transactions)
	  (inter :interactor
		 :width 500
		 :height 100))
  (:layouts (default
                (horizontally ()
                  (vertically () main inter)
                  accounts
                  transactions))))

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
  (format pane "Account: ~a" (name (account view))))

(defmethod display-main-with-view (frame pane (view transaction-view))
  (declare (ignore frame))
  (let ((transaction (transaction view)))
    (format pane "Transaction: ~a Date: ~a Created by: ~a~%"
            (name transaction)
            (iso-date-string (date transaction))
            (name (creator transaction)))
    (format pane "Debited accounts:~%")
    (loop for account in (debits transaction)
          do (format pane "     ~a~%" account))
    (format pane "Credited accounts:~%")
    (loop for account in (credits transaction)
          do (format pane "     ~a~%" account))))

(defun display-main (frame pane)
  (display-main-with-view frame pane (stream-default-view pane)))

(defun display-accounts (frame pane)
  (format pane "Accounts~%~%")
  (loop for account in (accounts (current-organization frame))
        do (format pane "~a~%" (name account))))

(defun display-transactions (frame pane)
  (format pane "Transactions~%~%")
  (loop for transaction in (transactions (current-organization frame))
        do (format pane "~a ~a~%"
                   (name transaction)
                   (iso-date-string (date transaction)))))

(defun compta ()
  (run-frame-top-level (make-application-frame 'compta)))

(define-compta-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-compta-command (com-new-account :name t) ((name 'string))
  (push (make-instance 'account :name name)
        (accounts (current-organization *application-frame*))))

