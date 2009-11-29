(in-package :compta-io)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader programming

(defparameter *compta-readtable-v1* (copy-readtable))

(defun read-compta-object-v1 (stream char)
  (declare (ignore char))
  (apply #'make-instance (read-delimited-list #\] stream t)))

(set-macro-character #\[ #'read-compta-object-v1 nil *compta-readtable-v1*)
(set-syntax-from-char #\] #\) *compta-readtable-v1*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Printer programming

(defgeneric save-info (object)
  (:method-combination append :most-specific-last))

(defclass compta-object () ())

;;; should really use *print-readably*
(defparameter *print-for-file-io* nil)

(defun print-compta-object (obj stream)
  (pprint-logical-block (stream nil :prefix "[" :suffix "]")
    (format stream "~s ~2i" (class-name (class-of obj)))
    (loop for info in (save-info obj)
          do (format stream
                     "~_~s ~W "
                     (car info)
                     (funcall (cadr info) obj)))))

(defmacro define-save-info (type &body save-info)
  `(progn

     (defmethod print-object ((obj ,type) stream)
       (if *print-for-file-io*
           (print-compta-object obj stream)
           (call-next-method)))

     (defmethod save-info append ((obj ,type))
       ',save-info)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; I/O

(define-condition compta-condition (error) ())

(define-condition file-does-not-exist (compta-condition) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "File does not exist"))))

(define-condition unknown-file-version (compta-condition) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Unknown file version"))))

(defparameter *readtables*
  `(("ComptaV1" . ,*compta-readtable-v1*)))

(defun read-organization (filename)
  (assert (probe-file filename) () 'file-does-not-exist)
  (with-open-file (stream filename :direction :input)
    (let* ((version (read-line stream))
           (readtable (cdr (assoc version *readtables* :test #'string=))))
      (assert readtable () 'unknown-file-version)
      (let ((*read-eval* nil)
            (*readtable* readtable))
        (read stream)))))

(defun write-organization (filename organization)
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (let ((*print-circle* t)
          (*print-for-file-io* t)
          (*package* (find-package :keyword)))
      (format stream "ComptaV1~%")
      (pprint organization stream)
      (terpri stream)
      (finish-output stream))))

