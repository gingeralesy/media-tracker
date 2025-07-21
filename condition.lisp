(in-package #:media-tracker)

(define-condition win-tracker-error (error) ())

(define-condition win-invalid-process-info (win-tracker-error)
  ((line :initarg :line :reader line))
  (:report (lambda (condition stream)
             (with-slots (line) condition
               (format stream "Invalid process info line: ~s" line)))))

(define-condition win-invalid-headers (win-tracker-error)
  ((headers :initarg :headers :reader headers))
  (:report (lambda (condition stream)
             (with-slots (headers) condition
               (format stream "Invalid headers: (~{~s~^ ~})" headers)))))

(define-condition win-invalid-image-name (win-tracker-error)
  ((name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (with-slots (name) condition
               (format stream "Invalid image name: ~a" name)))))
