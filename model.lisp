(cl:in-package #:compta-model)

(defclass name-mixin ()
  ((%name :initarg :name :accessor name)))

(defclass account (name-mixin)
  ())

(defmacro make-time-function (name n)
  `(defun ,name () (nth-value ,n (get-decoded-time))))

(make-time-function current-year 5)
(make-time-function current-month 4)
(make-time-function current-day 3)
(make-time-function current-hour 2)
(make-time-function current-minute 1)

(defclass date ()
  ((%year :initarg :year :initform (current-year) :reader year)
   (%month :initarg :month :initform (current-month) :reader month)
   (%day :initarg :day :initform (current-day) :reader day)
   (%hour :initarg :hour :initform (current-hour) :reader hour)
   (%minute :initarg :minute :initform (current-minute) :reader minute)))

(defun iso-date-string (date)
  (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d"
          (year date) (month date) (day date)
          (hour date) (minute date)))

(defparameter *operator* "Spiaggia")

(defclass person (name-mixin) ())

(defclass entry ()
  ((%account :initarg :account :reader account)
   (%amount :initarg :amount :reader amount)))

(defclass transaction (name-mixin)
  ((%date :initarg :date :initform (make-instance 'date) :reader date)
   (%creator :initarg :creator :initform *operator* :reader creator)
   (%debits :initform '() :initarg :debits :accessor debits)
   (%credits :initform '() :initarg :credits :accessor credits)))

(defclass organization (name-mixin)
  ((%accounts :initarg :accounts :initform '() :accessor accounts)
   (%transactions :initarg :transactions :initform '() :accessor transactions)))
