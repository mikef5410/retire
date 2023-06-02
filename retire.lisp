
;;;; retire.lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:with-user-abort :adopt ) :silent t))

(defpackage :retire
  (:use :cl)
  (:export :toplevel *ui*))

(in-package #:retire)

;;;; Errors ------------------------------------------------------
(define-condition user-error (error) ())

(defparameter *retireDate* (encode-universal-time 0 0 17 1 4 2026 7))

(defun calcRetire ()
  (let* ( (currentTime (get-universal-time))
          (interval  (- *retireDate* currentTime)  )
          (days 0)
          (hours 0)
          (workdays 0)
          (years 0)
          (sec 0)
          (to-day  (nth 6 (multiple-value-list (decode-universal-time currentTime))) ))
    (when (> interval 0) 
      (progn
        (multiple-value-setq (days hours) (floor interval (* 24 (* 60 60))))
        (setf years (/ days 365.25))
        (multiple-value-setq (hours sec) (floor hours (* 60 60)))
        (setf workdays (floor (- (/ (* days 5) 7) (* years 25))))
        (when (> to-day 4) (setf workdays (+ workdays (- to-day 4)))) ;Sat or Sun? 1 or 2 days back
        (format t "Retire in ~d days ~d hours! (~d work days)~%" days hours workdays)))))

;;;; Run ---------------------------------------------------------
(defun run ()
  (calcRetire))

;;;; User Interface ----------------------------------------------
#+sbcl
(defmacro exit-on-ctrl-c (&body body)
  `(handler-case (with-user-abort:with-user-abort (progn ,@body))
     (with-user-abort:user-abort () (sb-ext:exit :code 130))))

;; (defparameter *ui*
;;   (adopt:make-interface
;;     :name "retire"
;;     ...))

(defun toplevel ()
  #+sbcl  (sb-ext:disable-debugger)
  ;; (exit-on-ctrl-c
  ;;   (multiple-value-bind (arguments options) (adopt:parse-options-or-exit *ui*)
  ;;     … ; Handle options.
  ;;     (handler-case (run )
  ;;       (user-error (e) (adopt:print-error-and-exit e)))))
  (run)
  )


