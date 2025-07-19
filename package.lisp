(in-package #:cl-user)
(defpackage #:media-tracker
  (:use #:cl #:alexandria)
  (:local-nicknames
   (:v :org.shirakumo.verbose))
  (:export
   :current-track
   :start-tracker
   :stop-tracker))
(in-package #:media-tracker)
