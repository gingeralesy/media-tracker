(in-package #:media-tracker)

(deftype media-player () '(and keyword (or (eql :vlc) (eql :spotify) (eql :foobar2000))))

(defparameter *tracker-lock* (bt2:make-lock :name "MEDIA-TRACKER-LOCK"))
(defparameter *tracker-semaphore* (bt2:make-semaphore :name "MEDIA-TRACKER-SEMAPHORE"))
(defparameter *tracker-thread* NIL)
(defparameter *tracker-running* NIL)
(defparameter *tracker-current* "")

(defparameter *interval* 1.0)
(defparameter *timeout* 5.0)

(defun fetch-current-track (player args)
  (declare (type media-player player))
  (declare (type hash-table args))
  ;; (declare (optimize (speed 3)))
  (ecase player
    (:vlc
     (let ((*vlc-server-protocol* (gethash :protocol args *vlc-server-protocol*))
           (*vlc-server-hostname* (gethash :hostname args *vlc-server-hostname*))
           (*vlc-server-port* (gethash :port args *vlc-server-port*))
           (*vlc-root-path* (gethash :root-path args *vlc-root-path*)))
       (vlc-track-name
        (vlc-current-track :login (gethash :login args) :password (gethash :password args))
        :album (gethash :album args)
        :track-number (gethash :track-number args))))
    (:spotify (spotify-current-track))
    (:foobar2000 (foobar2000-current-track))))

(defun update-current-track (player args)
  (declare (type single-float *timeout*))
  (declare (type media-player player))
  (declare (type hash-table args))
  ;; (declare (optimize (speed 3)))
  (handler-case
      (multiple-value-bind (new-name current-name)
          (bt2:with-timeout (*timeout*)
            (values (fetch-current-track player args) *tracker-current*))
        (declare (type (or null simple-string) new-name current-name))
        (when (and new-name (or (null current-name) (string/= new-name current-name)))
          (bt2:with-lock-held (*tracker-lock*)
            (v:debug :media-tracker "Updating track to \"~a\"" new-name)
            (setf *tracker-current* new-name))))
    (bt2:timeout () (v:warn :media-tracker.update "Fetching the current track timed out."))))

(defun current-track (player &rest args)
  (declare (type media-player player))
  (fetch-current-track player (plist-hash-table args)))

(defun start-tracker (player file async-p &rest args)
  (declare (type media-player player))
  (declare (type pathname file))
  (declare (type boolean async-p))
  ;; (declare (optimize (speed 3)))
  (unless (and (bt2:lockp *tracker-lock*) (bt2:semaphorep *tracker-semaphore*))
    (v:error :media-tracker.run "Media Player Tracker is incorrectly initialised.")
    (return-from start-tracker))
  (bt2:with-lock-held (*tracker-lock*)
    (when *tracker-thread*
      (v:warn :media-tracker.run "A Media Player Tracker (async) is already running.")
      (return-from start-tracker)))
  (let* ((args (plist-hash-table args))
         (*interval* (gethash :interval args *interval*))
         (*timeout* (gethash :timeout args *timeout*)))
    (declare (type single-float *interval* *timeout*))
    (flet ((track-loop ()
             (unwind-protect
                  (loop while (bt2:with-lock-held (*tracker-lock*) *tracker-running*)
                        unless (null (update-current-track player args))
                        do (bt2:with-lock-held (*tracker-lock*)
                             (with-open-file (stream file :direction :output
                                                          :if-does-not-exist :create
                                                          :if-exists :supersede)
                               (format stream "~a" *tracker-current*)))
                        do (if async-p
                               (bt2:wait-on-semaphore *tracker-semaphore* :timeout *interval*)
                               (handler-case
                                   (progn
                                     (bt2:with-timeout (*interval*) (read-line))
                                     (bt2:with-lock-held (*tracker-lock*)
                                       (setf *tracker-running* NIL)))
                                 (bt2:timeout ()))))
               (bt2:with-lock-held (*tracker-lock*)
                 (setf *tracker-running* NIL)
                 (setf *tracker-thread* NIL)
                 (v:info :media-tracker.run "Stopped Media Player Tracker (~a)."
                         (if async-p "async" "sync"))))))
      (bt2:with-lock-held (*tracker-lock*)
        (v:info :media-tracker.run "Starting Media Player Tracker (~a)."
                (if async-p "async" "sync"))
        (unless async-p
          (v:info :media-tracker.run "Press enter to stop..."))
        (setf *tracker-running* T)
        (when async-p
          (setf *tracker-thread* (bt2:make-thread #'track-loop :name "MEDIA-TRACKER-THREAD"))))
      (unless async-p (track-loop)))))

(defun stop-tracker ()
  (bt2:with-lock-held (*tracker-lock*)
    (when (and *tracker-thread* *tracker-running*)
      (setf *tracker-running* NIL)
      (bt2:signal-semaphore *tracker-semaphore*))))
