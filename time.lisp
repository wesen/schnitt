(in-package :schnitt)

;; time helper macro characters

(defparameter *bpm* 120.0)
(defparameter *fps* 25.0)
(defparameter *time-division* '(4 4))

(defun time->frames (time)
  (round (* time *fps*)))

(defun beat->frames (beat)
  (round (if (listp val)
	     (let* ((takt (first val))
		    (subtakt (second val))
		    (takt-beat-duration (/ 60.0 (/ *bpm* (/ 4 (second *time-division*)))))
		    (takt-duration (* takt-beat-duration (first *time-division*))))
	       (+ (* (1- takt) takt-duration)
		  (* (1- subtakt) takt-beat-duration)))
	     (* val (/ 60.0 *bpm*)))))

(set-dispatch-macro-character #\# #\%
   #'(lambda (s c n)
       (let ((char (read-char s nil (values) t))
	     (val (read s nil (values) t)))
	 (case char
	   (#\t (time->frames val))
	   (#\b (beat->frames val))
	   (t (error "unknown time dispatch character: ~A" char))))))

