(in-package #:media-tracker)

(defparameter *vlc-server-protocol* :http)
(defparameter *vlc-server-hostname* "127.0.0.1")
(defparameter *vlc-server-port* 9090)
(defparameter *vlc-root-path* NIL)

(defun check-vlc-parameters (page command parameters &rest keys)
  (let ((parameter-keys (loop for key of-type keyword in parameters by #'cddr collecting key)))
    (unless (= (length parameter-keys) (length keys))
      (error 'invalid-vlc-parameters :page page :command command :parameters parameters))
    (loop for key in parameter-keys
          unless (find key keys :test #'eql)
          do (error 'invalid-vlc-parameter :page page :command command :parameter key))))

(defun check-vlc-mrl-parameter (page command parameter value)
  (unless (or (pathnamep value) (stringp value)) ;; TODO: An actual test.
    (error 'invalid-vlc-parameter-value :page page :command command :name parameter :value value)))

(defun check-vlc-playlist-item-id-parameter (page command id)
  (unless (or (typep id '(unsigned-byte 16)) (and (stringp id) (cl-ppcre:scan "^\\d+$" id)))
    (error 'invalid-vlc-parameter-value :page page :command command :name :id :value id)))

(defun vlc-page-file (page)
  (declare (type keyword page))
  (case page
    (:status "status.xml")
    (:playlist "playlist.xml")
    (:browse "browse.xml")
    (:vlm "vlm.xml")
    (:vlm-cmd "vlm_cmd.xml")
    (T (error 'invalid-vlc-page :page page))))

(defun vlc-sort-mode (mode)
  (typecase mode
    (keyword
     (case mode ;; TODO: Find the rest.
       (:id 0)
       (:name 1)
       (:author 3)
       (:random 5)
       (:track-number 7)))
    (integer mode)
    (string (when (cl-ppcre:scan "^\\d+$" mode) mode))))

(defun vlc-volume-value (value)
  (typecase value
    (integer value)
    (number
     (let ((floor (floor value)))
       (when (= floor value) floor)))
    (string (when (cl-ppcre:scan "^([\\+-]?\\d+|\\d+%)$" value) value))))

(defun vlc-seek-value (value)
  (typecase value
    (integer value)
    (number
     (let ((floor (floor value)))
       (when (= floor value) floor)))
    (string
     (when (cl-ppcre:scan
            "^[+-]?(((\\d+[Hh]:)?\\d+[Mm]:\\d+[Ss])|(\\d+[Hh]:\\d+[MmSs])|(\\d+[%HhMmSs]?))$" value)
       value))))

(defun vlc-status-query (command parameters)
  (case command
    (:play
     (cond
       ((null parameters)
        (list :command "pl_play"))
       ((find :id parameters :test #'eql)
        (check-vlc-parameters :status command parameters :id)
        (check-vlc-playlist-item-id-parameter :status command (second parameters))
        (append (list :command "pl_play") parameters))
       ((find :input parameters :test #'eql)
        (check-vlc-parameters :status command parameters :input)
        (check-vlc-mrl-parameter :status command :input (second parameters))
        (append (list :command "in_play") parameters))
       (T (error 'invalid-vlc-parameters :page :status :command command :parameters parameters))))
    ((:add :enqueue)
     (check-vlc-parameters :status command parameters :input)
     (check-vlc-mrl-parameter :status command :input (second parameters))
     (append (list :command "in_enqueue") parameters))
    (:pause
     (cond
       ((null parameters)
        (list :command "pl_pause"))
       (T
        (check-vlc-parameters :status command parameters :id)
        (check-vlc-playlist-item-id-parameter :status command (second parameters))
        (append (list :command "pl_pause") parameters))))
    (:stop
     (check-vlc-parameters :status command parameters)
     (list :command "pl_stop"))
    (:next
     (check-vlc-parameters :status command parameters)
     (list :command "pl_next"))
    ((:prev :previous)
     (check-vlc-parameters :status command parameters)
     (list :command "pl_previous"))
    ((:del :delete)
     (check-vlc-parameters :status command parameters :id)
     (check-vlc-playlist-item-id-parameter :status command (second parameters))
     (append (list :command "pl_delete") parameters))
    ((:clear :empty)
     (check-vlc-parameters :status command parameters)
     (list :command "pl_empty"))
    (:sort
     (check-vlc-parameters :status command parameters :asc :mode)
     (let ((id (if (getf parameters :asc) 0 1))
           (val (vlc-sort-mode (getf parameters :mode))))
       (unless val
         (error 'invalid-vlc-parameter-value
                :page :status :command command :name :mode :value (getf parameters :mode)))
       (list :command "pl_sort" :id id :val val)))
    (:random
     (check-vlc-parameters :status command parameters)
     (list :command "pl_random"))
    (:loop
      (check-vlc-parameters :status command parameters)
      (list :command "pl_loop"))
    (:repeat
     (check-vlc-parameters :status command parameters)
     (list :command "pl_repeat"))
    ((:sd :service-discovery)
     (check-vlc-parameters :status command parameters :service)
     (let ((val (getf parameters :service)))
       (list :command "pl_sd" :val (if (stringp val) val (format NIL "~(~a~)" val)))))
    (:fullscreen
     (check-vlc-parameters :status command parameters)
     (list :command "fullscreen"))
    (:volume
     (check-vlc-parameters :status command parameters :value)
     (let ((val (vlc-volume-value (getf parameters :value))))
       (unless val
         (error 'invalid-vlc-parameter-value
                :page :status :command command :name :value :value (getf parameters :value)))
       (list :command "volume" :val val)))
    (:seek
     (check-vlc-parameters :status command parameters :time)
     (let ((val (vlc-seek-value (getf parameters :time))))
       (unless val
         (error 'invalid-vlc-parameter-value
                :page :status :command command :name :time :value (getf parameters :time)))
       (list :command "seek" :val val)))
    (T (error 'invalid-vlc-command :page :status :command command))))

(defun encode-query-parameter (part)
  (drakma:url-encode
   (typecase part
     (string part)
     (symbol (format NIL "~(~a~)" part))
     (integer (format NIL "~d" part))
     (T (format NIL "~a" part)))
   :utf-8))

(defun vlc-query (page parameters)
  (mapcar
   #'encode-query-parameter
   (case page
     (:status
      (when parameters
        (loop with command = NIL
              for key in parameters by #'cddr
              for value in (rest parameters) by #'cddr
              if (eql key :command) do (setf command value)
              else nconc (list key value) into params
              finally (return (vlc-status-query command params)))))
     ((:playlist :vlm)
      (when parameters
        (error 'invalid-vlc-parameters :page page :command NIL :parameters parameters))
      ())
     (:browse
      (check-vlc-parameters page NIL parameters :dir)
      (let ((dir (getf parameters :dir)))
        (unless (stringp dir) ;; TODO: A better check.
          (error 'invalid-vlc-parameter-value :page page :command NIL :name :dir :value dir))
        (list :dir dir)))
     (:vlm-cmd
      (check-vlc-parameters page NIL parameters :command)
      (let ((command (getf parameters :command)))
        (unless (stringp command) ;; TODO: A better check.
          (error 'invalid-vlc-parameter-value
                 :page page :command NIL :name :command :value command))
        (list :command command)))
     (T (error 'invalid-vlc-page :page page)))))

(defun vlc-request-url (page &optional parameters)
  (declare (type (or keyword string) protocol))
  (declare (type string hostname))
  (declare (type (or null string) root-path))
  (declare (type (or null (unsigned-byte 16)) port))
  (declare (type list parameters))
  (let ((file (vlc-page-file page))
        (query (vlc-query page parameters)))
    (format NIL "~(~a~)://~a~:[~;:~1:*~a~]~:[~;~1:*~a~]/requests/~a~:[~;~1:*?~{~a=~a~^&~}~]"
            *vlc-server-protocol* *vlc-server-hostname* *vlc-server-port* *vlc-root-path*
            file query)))
