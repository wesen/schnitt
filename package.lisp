(in-package :cl-user)

(defpackage :schnitt
  (:use :common-lisp)
  (:export

   ;; wave
   wave-file
   read-wave-file
   write-wave-file
   time-to-offset
   wave-file-slice-offset
   wave-file-slice-time
   copy-wave
   concat-waves))