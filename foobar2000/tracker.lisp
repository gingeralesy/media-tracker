(in-package #:media-tracker)

(defparameter *fb2000-title-re* (cl-ppcre:create-scanner "^(.*\\S)\\s+\\[foobar2000\\].*$"
                                                         :case-insensitive-mode T))

(defun foobar2000-current-track ()
  ;; (declare (optimize (speed 3)))
  #+windows
  (loop for title of-type simple-string in (win-process-titles "foobar2000.exe")
        unless (or (zerop (length title)) (istring= title "N/A"))
        do (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings *fb2000-title-re* title)
             (when match (return (svref groups 0)))))
  #+nx ;; TODO:
  (error "Unimplemented.")
  #+darwin ;; TODO:
  (error "Unimplemented."))
