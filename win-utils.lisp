(in-package #:media-tracker)

(defparameter *linebreak-re* (cl-ppcre:create-scanner "\\r?\\n"))

(defun parse-csv (line)
  (declare (type simple-string line))
  (declare (optimize (speed 3)))
  (loop with out-p = T
        and escaped-p = NIL
        and parts = ()
        and start = 0
        and index of-type (unsigned-byte 32) = 0
        for i from 0 below (length line)
        for ch = (schar line i)
        do (cond
             ((and out-p escaped-p)
              (error 'win-invalid-process-info :line line))
             (out-p
              (case ch
                ((#\Space #\Tab #\Linefeed #\Return #\Page))
                (#\, (incf index))
                (#\"
                 (unless (= index (length parts))
                   (error 'win-invalid-process-info :line line))
                 (setf out-p NIL)
                 (setf start (1+ i)))
                (T (error 'win-invalid-process-info :line line))))
             (escaped-p
              (setf escaped-p NIL))
             ((char= ch #\\)
              (setf escaped-p T))
             ((char= ch #\")
              (push (subseq line start i) parts)
              (setf out-p T)))
        finally (return (if (= (1- (length parts)) index)
                            (nreverse parts)
                            (error 'win-invalid-process-info :line line)))))

(defun istring= (a b)
  (declare (type simple-string a b))
  ;; (declare (optimize (speed 3)))
  (string= (string-upcase a) (string-upcase b)))

(defun win-command-output (command)
  ;; (declare (optimize (speed 3)))
  (let ((output (make-array 0 :element-type 'character :fill-pointer 0 :adjustable T)))
    (with-output-to-string (stream output)
      (uiop:run-program command :output stream :force-shell T :external-format :windows-1252))
    output))

(defun win-process-pids (process-name)
  ;; (declare (optimize (speed 3)))
  (loop with command = (format NIL "tasklist /fo \"csv\" /fi \"IMAGENAME eq ~a\" /fi \"STATUS eq running\" /m \"Windows.UI.dll\""
                               process-name)
        with processes = (command-output command)
        with process-list = (cl-ppcre:split *linebreak-re* processes)
        with headers of-type list = (parse-csv (first process-list))
        with pid-index = (or (position "PID" headers :test #'istring=)
                             (error 'win-invalid-headers :headers headers))
        for process in (rest process-list)
        collect (nth pid-index (parse-csv process))))

(defun win-process-titles (process-name)
  ;; (declare (optimize (speed 3)))
  (loop with command = (format NIL "tasklist /fo \"csv\" /fi \"IMAGENAME eq ~a\" /fi \"STATUS eq running\" /v"
                               process-name)
        with pids of-type list = (process-pids process-name)
        and processes = (command-output command)
        with process-list = (cl-ppcre:split *linebreak-re* processes)
        with headers of-type list = (parse-csv (first process-list))
        with title-index = (or (position "Window Title" headers :test #'istring=)
                               (error 'win-invalid-headers :headers headers))
        and pid-index = (or (position "PID" headers :test #'istring=)
                            (error 'win-invalid-headers :headers headers))
        for process in (rest process-list)
        for title = (nth title-index (parse-csv process))
        and pid = (nth pid-index (parse-csv process))
        when (find pid pids :test #'istring=)
        collect title))
