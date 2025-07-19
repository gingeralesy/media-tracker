#|
This file is a part of media-tracker
(c) 2025 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:cl-user)
(asdf:defsystem media-tracker
  :version "0.0.0"
  :license "zlib"
  :author "Janne Pakarinen <gingeralesy@gmail.com>"
  :maintainer "Janne Pakarinen <gingeralesy@gmail.com>"
  :description "A utility tool for tracking media player status."
  :components ((:file "package")
               (:file "condition" :depends-on ("package"))
               (:file "win-utils" :depends-on ("package" "condition"))
               (:file "vlc/condition" :depends-on ("package"))
               (:file "vlc/request" :depends-on ("package" "vlc/condition"))
               (:file "vlc/client" :depends-on ("package" "vlc/request"))
               (:file "vlc/tracker" :depends-on ("package" "vlc/request" "vlc/client"))
               (:file "spotify/tracker" :depends-on ("package" "win-utils"))
               (:file "foobar2000/tracker" :depends-on ("package" "win-utils"))
               (:file "tracker"
                :depends-on ("package" "vlc/request" "vlc/tracker" "spotify/tracker"
                                       "foobar2000/tracker")))
  :depends-on (:uiop
               :alexandria
               :plump
               :lquery
               :drakma
               :cl-ppcre
               :verbose
               :bordeaux-threads))
