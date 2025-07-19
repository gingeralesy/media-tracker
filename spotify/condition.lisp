(in-package #:media-tracker)

(define-condition spotify-tracker-error (error) ())

(define-condition spotify-invalid-process-info (spotify-tracker-error)
  ((line :initarg :line :reader line))
  (:report (lambda (condition stream)
             (with-slots (line) condition
               (format stream "Invalid process info line: ~s" line)))))

(define-condition spotify-invalid-headers (spotify-tracker-error)
  ((headers :initarg :headers :reader headers))
  (:report (lambda (condition stream)
             (with-slots (headers) condition
               (format stream "Invalid headers: (~{~s~^ ~})" headers)))))
