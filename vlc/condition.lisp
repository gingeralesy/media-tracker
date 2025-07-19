(in-package #:media-tracker)

(define-condition vlc-tracker-error (error) ())

(define-condition invalid-vlc-page (vlc-tracker-error)
  ((page :initarg :page :reader page))
  (:report (lambda (condition stream)
             (with-slots (page) condition
               (format stream "Invalid page page: ~a" page)))))

(define-condition invalid-vlc-command (vlc-tracker-error)
  ((page :initarg :page :reader page)
   (command :initarg :command :reader command))
  (:report (lambda (condition stream)
             (with-slots (page command) condition
               (format stream "Invalid command for ~a page: ~a" page command)))))

(define-condition invalid-vlc-parameters (vlc-tracker-error)
  ((page :initarg :page :reader page)
   (command :initarg :command :reader command)
   (parameters :initarg :parameters :reader parameters))
  (:report (lambda (condition stream)
             (with-slots (page command parameters) condition
               (format stream "Invalid ~a page parameters~:[~; for ~1:*~a command~]: ~a"
                       page command parameters)))))

(define-condition invalid-vlc-parameter (vlc-tracker-error)
  ((page :initarg :page :reader page)
   (command :initarg :command :reader command)
   (parameter :initarg :parameter :reader parameter))
  (:report (lambda (condition stream)
             (with-slots (page command parameter) condition
               (format stream "Invalid ~a page parameter~:[~; for ~1:*~a command~]: ~a"
                       page command parameter)))))

(define-condition invalid-vlc-parameter-value (vlc-tracker-error)
  ((page :initarg :page :reader page)
   (command :initarg :command :reader command)
   (name :initarg :name :reader name)
   (value :initarg :value :reader value))
  (:report (lambda (condition stream)
             (with-slots (page command name value) condition
               (format stream
                       "Invalid ~a page parameter value for ~a parameter~:[~; for ~1:*~a command~]: ~a"
                       page name command value)))))
