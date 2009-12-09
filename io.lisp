(in-package :io)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader programming

(defparameter *io-readtable-v1* (copy-readtable))

(defun read-model-object-v1 (stream char)
  (declare (ignore char))
  (apply #'make-instance (read-delimited-list #\] stream t)))

(set-macro-character #\[ #'read-model-object-v1 nil *io-readtable-v1*)
(set-syntax-from-char #\] #\) *io-readtable-v1*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Printer programming

(defgeneric save-info (object)
  (:method-combination append :most-specific-last))

(defclass model-object () ())

;;; should really use *print-readably*
(defparameter *print-for-file-io* nil)

(defun print-model-object (obj stream)
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
           (print-model-object obj stream)
           (call-next-method)))

     (defmethod save-info append ((obj ,type))
       ',save-info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; I/O

(define-condition model-condition (error) ())

(define-condition file-does-not-exist (model-condition) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "File does not exist"))))

(define-condition unknown-file-version (model-condition) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Unknown file version"))))

(defparameter *readtables*
  `(("ComptaV1" . ,*io-readtable-v1*)))

(defun read-model (filename)
  (assert (probe-file filename) () 'file-does-not-exist)
  (with-open-file (stream filename :direction :input)
    (let* ((version (read-line stream))
           (readtable (cdr (assoc version *readtables* :test #'string=))))
      (assert readtable () 'unknown-file-version)
      (let ((*read-eval* nil)
            (*readtable* readtable))
        (read stream)))))

(defun write-model (filename object)
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (let ((*print-circle* t)
          (*print-for-file-io* t)
          (*package* (find-package :keyword)))
      (format stream "IOV1~%")
      (pprint object stream)
      (terpri stream)
      (finish-output stream))))

