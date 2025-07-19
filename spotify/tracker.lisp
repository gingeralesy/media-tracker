(in-package #:media-tracker)

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
              (error 'spotify-invalid-process-info :line line))
             (out-p
              (case ch
                ((#\Space #\Tab #\Linefeed #\Return #\Page))
                (#\, (incf index))
                (#\"
                 (unless (= index (length parts))
                   (error 'spotify-invalid-process-info :line line))
                 (setf out-p NIL)
                 (setf start (1+ i)))
                (T (error 'spotify-invalid-process-info :line line))))
             (escaped-p
              (setf escaped-p NIL))
             ((char= ch #\\)
              (setf escaped-p T))
             ((char= ch #\")
              (push (subseq line start i) parts)
              (setf out-p T)))
        finally (return (if (= (1- (length parts)) index)
                            (nreverse parts)
                            (error 'spotify-invalid-process-info :line line)))))

(defun command-output (command)
  (declare (optimize (speed 3)))
  (let ((output (make-array 0 :element-type 'character :fill-pointer 0 :adjustable T)))
    (with-output-to-string (stream output)
      (uiop:run-program command :output stream))
    output))

(defun istring= (a b)
  (declare (type simple-string a b))
  ;; (declare (optimize (speed 3)))
  (string= (string-upcase a) (string-upcase b)))

(defun spotify-current-track ()
  (declare (optimize (speed 3)))
  #+windows
  (loop with ws-re = (cl-ppcre:create-scanner "\\r?\\n")
        with pids = (loop with processes = (command-output "tasklist /fo \"csv\" /fi \"IMAGENAME eq Spotify.exe\" /fi \"STATUS eq running\" /m \"Windows.UI.dll\"")
                          with process-list = (cl-ppcre:split ws-re processes)
                          with headers of-type list = (parse-csv (first process-list))
                          with pid-index = (or (position "PID" headers :test #'istring=)
                                               (error 'spotify-invalid-headers :headers headers))
                          for process in (rest process-list)
                          collect (nth pid-index (parse-csv process)))
        and processes =
        (command-output
         "tasklist /fo \"csv\" /fi \"IMAGENAME eq Spotify.exe\" /fi \"STATUS eq running\" /v")
        with process-list = (cl-ppcre:split ws-re processes)
        with headers of-type list = (parse-csv (first process-list))
        with title-index = (or (position "Window Title" headers :test #'istring=)
                               (error 'spotify-invalid-headers :headers headers))
        and pid-index = (or (position "PID" headers :test #'istring=)
                            (error 'spotify-invalid-headers :headers headers))
        for process in (rest process-list)
        for title = (nth title-index (parse-csv process))
        and pid = (nth pid-index (parse-csv process))
        when (and (find pid pids :test #'istring=)
                  (not (find title '("N/A" "Spotify Premium" "Spotify Free") :test #'istring=)))
        do (return title))
  #+nx ;; TODO:
  (error "Unimplemented.")
  #+darwin ;; TODO:
  (error "Unimplemented."))
