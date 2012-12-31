(in-package :cl-user)

(defpackage :schnitt.system
  (:use :cl :asdf))

(in-package :schnitt.system)

(defsystem :schnitt
    :name "Schnittsoftware"
    :author "Manuel Odendahl <manuel@bl0rg.net>"
    :licence "BSD"
    :description "Movie editing sofware"

    :components ((:file "package")

		 (:file "helpers" :depends-on ("package"))
		 
		 ;; binary parsing
		 (:file "binary" :depends-on ("package"))
		 (:file "binary-types" :depends-on ("binary"))
		 (:file "file-chunk" :depends-on ("binary-types" "binary"))

		 ;; riff formats
		 (:file "riff" :depends-on ("binary-types" "binary" "file-chunk"))
		 (:file "wave" :depends-on ("riff" "file-chunk" "binary-types" "binary"))
		 (:file "avi" :depends-on ("wave" "riff" "file-chunk"
						  "binary-types" "binary"
						  "helpers"))

		 ;; avi manipulation
		 (:file "time" :depends-on ("package"))
		 (:file "manipulation" :depends-on ("avi" "wave" "helpers"))))