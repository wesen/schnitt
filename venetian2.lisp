(in-package :cl-user)

(defparameter *erster-abschnitt-speeds*
  (list
   (append (make-constant-speed 0.3 10)
	   (accelerate2 0.3 0.04 11)
	   (make-constant-speed 0.3 10)
	   (accelerate2 0.3 0.04 11)
	   (make-constant-speed 0.3 10)
	   (accelerate2 0.3 0.04 10)
	   (accelerate2 0.3 0.04 11))
   (append (make-constant-speed 0.5 21)
	   (accelerate2 0.3 0.01 21)
	   (make-constant-speed 0.5 10)
	   (accelerate2 0.5 0.03 10)
	   (accelerate2 0.5 0.03 11))
   (append (make-constant-speed 0.5 21)
	   (make-constant-speed 0.3 10)
	   (accelerate2 0.3 0.04 11)
	   (make-constant-speed 0.3 10)
	   (accelerate2 0.5 0.03 10)
	   (accelerate2 0.5 0.03 11))
   (append (make-constant-speed 0.3 10)
	   (accelerate2 0.3 0.04 11)
	   (accelerate2 0.3 0.01 21)
	   (make-constant-speed 0.3 10)
	   (accelerate2 0.3 0.04 10)
	   (accelerate2 0.3 0.04 11))))

(defun reinrenn-grundfilm ()
  (let* ((reinrenn (repeat-loop-avi *reinrenn* 1))
	 (film
	  (schedule-avis (loop for i from 0 below 4
			       with start = 0
			       for speed = (random-erster-abschnitt-speed)
			       for segment = (beschleunig-avi (subsegment reinrenn :start start) speed)
			       collect (list segment (beat-length (1+ (* 7.0 i))))
			       do (incf start (count-speed-frames speed))))))
    film))
