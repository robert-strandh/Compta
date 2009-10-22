(in-package #:compta-gui)

(define-application-frame compta ()
  ()
  (:panes (main :application
		:width 500
		:height 500
		:display-time nil)
	  (periods :application
		   :width 500
		   :height 50
		   :display-time nil)
	  (accounts :application
		    :width 100
		    :height 650
		    :display-time nil)
	  (inter :interactor
		 :width 500
		 :height 100))
  (:layouts (default
		(horizontally ()
			      (vertically () periods main inter)
			      accounts))))

(defun compta ()
  (run-frame-top-level (make-application-frame 'compta)))

(define-compta-command (quitter :name t) ()
  (frame-exit *application-frame*))
