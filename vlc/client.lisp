(in-package #:media-tracker)

;; TODO: Return some class instance rather than the XML.

(defun vlc-status (&key login password)
  (plump:parse
   (drakma:http-request
    (vlc-request-url :status)
    :basic-authorization (when (or login password) (list (or login "") (or password "")))
    :want-stream T)))

(defun vlc-add (file &key play login password)
  (plump:parse
   (drakma:http-request
    (vlc-request-url :status (list :command (if play :play :add) :input file))
    :basic-authorization (when (or login password) (list (or login "") (or password "")))
    :want-stream T)))

(defun vlc-play (&key id login password)
  (plump:parse
   (drakma:http-request
    (vlc-request-url :status (if id (list :command :play :id id) '(:command :play)))
    :basic-authorization (when (or login password) (list (or login "") (or password "")))
    :want-stream T)))

(defun vlc-stop (&key login password)
  (plump:parse
   (drakma:http-request
    (vlc-request-url :status '(:command :stop))
    :basic-authorization (when (or login password) (list (or login "") (or password "")))
    :want-stream T)))

(defun vlc-next (&key login password)
  (plump:parse
   (drakma:http-request
    (vlc-request-url :status '(:command :next))
    :basic-authorization (when (or login password) (list (or login "") (or password "")))
    :want-stream T)))

(defun vlc-prev (&key login password)
  (plump:parse
   (drakma:http-request
    (vlc-request-url :status '(:command :prev))
    :basic-authorization (when (or login password) (list (or login "") (or password "")))
    :want-stream T)))

(defun vlc-delete-track (id &key login password)
  (plump:parse
   (drakma:http-request
    (vlc-request-url :status (list :command :delete :id id))
    :basic-authorization (when (or login password) (list (or login "") (or password "")))
    :want-stream T)))

(defun vlc-clear (&key login password)
  (plump:parse
   (drakma:http-request
    (vlc-request-url :status '(:command :clear))
    :basic-authorization (when (or login password) (list (or login "") (or password "")))
    :want-stream T)))

(defun vlc-sort-tracks (ascending mode &key login password)
  (plump:parse
   (drakma:http-request
    (vlc-request-url :status (list :command :sort :asc ascending :mode mode))
    :basic-authorization (when (or login password) (list (or login "") (or password "")))
    :want-stream T)))

(defun vlc-toggle-random (&key login password)
  (plump:parse
   (drakma:http-request
    (vlc-request-url :status '(:command :random))
    :basic-authorization (when (or login password) (list (or login "") (or password "")))
    :want-stream T)))

(defun vlc-toggle-repeat (all-tracks &key login password)
  (plump:parse
   (drakma:http-request
    (vlc-request-url :status (list :command (if all-tracks :loop :repeat)))
    :basic-authorization (when (or login password) (list (or login "") (or password "")))
    :want-stream T)))

(defun vlc-toggle-service-discovery (service &key login password)
  (plump:parse
   (drakma:http-request
    (vlc-request-url :status (list :command :toggle-service-discovery :service service))
    :basic-authorization (when (or login password) (list (or login "") (or password "")))
    :want-stream T)))

(defun vlc-toggle-fullscreen (&key login password)
  (plump:parse
   (drakma:http-request
    (vlc-request-url :status '(:command :fullscreen))
    :basic-authorization (when (or login password) (list (or login "") (or password "")))
    :want-stream T)))

(defun vlc-volume (value &key login password)
  (plump:parse
   (drakma:http-request
    (vlc-request-url :status (list :command :volume :value value))
    :basic-authorization (when (or login password) (list (or login "") (or password "")))
    :want-stream T)))

(defun vlc-seek (time &key login password)
  (plump:parse
   (drakma:http-request
    (vlc-request-url :status (list :command :seek :time time))
    :basic-authorization (when (or login password) (list (or login "") (or password "")))
    :want-stream T)))

(defun vlc-playlist (&key login password)
  (plump:parse
   (drakma:http-request
    (vlc-request-url :playlist)
    :basic-authorization (when (or login password) (list (or login "") (or password "")))
    :want-stream T)))

(defun vlc-browse (directory &key login password)
  (plump:parse
   (drakma:http-request
    (vlc-request-url :status (list :dir directory))
    :basic-authorization (when (or login password) (list (or login "") (or password "")))
    :want-stream T)))

(defun vlc-vlm (&key login password)
  (plump:parse
   (drakma:http-request
    (vlc-request-url :vlm)
    :basic-authorization (when (or login password) (list (or login "") (or password "")))
    :want-stream T)))

(defun vlc-vlm-command (command &key login password)
  (plump:parse
   (drakma:http-request
    (vlc-request-url :status (list :command command))
    :basic-authorization (when (or login password) (list (or login "") (or password "")))
    :want-stream T)))
