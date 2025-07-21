(in-package #:media-tracker)

(defun spotify-current-track ()
  ;; (declare (optimize (speed 3)))
  #+windows
  (loop for title in (win-process-titles "Spotify.exe")
        when (not (find title '("N/A" "Spotify Premium" "Spotify Free") :test #'istring=))
        do (return title))
  #+nx ;; TODO:
  (error "Unimplemented.")
  #+darwin ;; TODO:
  (error "Unimplemented."))
