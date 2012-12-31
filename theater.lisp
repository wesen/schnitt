(in-package :schnitt)

(defun make-my-avi (avi)
  (write-avi-file (make-avi-file avi)
		  "/Users/manuel/tmp.avi"))


(defvar *avi*)
(defvar *avi2*)
(defun idx1-chunks (file)
  (slot-value (second (slot-value file 'chunks)) 'entries))

(defun first-idx1-entry (file)
  (first (idx1-chunks file)))

(defun read-avi (filename)
  (make-avi (read-avi-file filename)))

#|
(setf *avi2* (read-avi "/Users/manuel/Public/collage2.avi"))
(setf *avi* (read-avi "/Users/manuel/Public/collage1.avi"))
|#

#|
(defparameter *bombe1* (read-avi "/Users/manuel/film-material/bombe1.avi"))
(defparameter *bombe2* (read-avi "/Users/manuel/film-material/bombe2.avi"))
(defparameter *flugzeug1* (read-avi "/Users/manuel/film-material/flugzeug1.avi"))
|#
(defparameter *frame* (read-avi "/Users/manuel/film-material/frame.avi"))
(defparameter *black* (read-avi "/Users/manuel/film-material/frame.avi"))
#+nil(defparameter *lkw1* (read-avi "/Users/manuel/film-material/lkw1.avi"))
#|
(defparameter *lkw3* (read-avi "/Users/manuel/film-material/lkw3.avi"))
(defparameter *wurste* (read-avi "/Users/manuel/film-material/wurste.avi"))
(defparameter *wueste2* (read-avi "/Users/manuel/film-material/wueste2.avi"))
(defparameter *uebergaenge* (read-avi "/Users/manuel/film-material/bilder.avi"))
(defparameter *popcar* (read-avi "/Users/manuel/film-material/popcar.avi"))
|#

(defun dir-avis (dir)
  (directory (format nil "/Users/manuel/Public/~A/**/*.avi" dir)))

(defun read-dir-avis (dir)
  (mapcar #'(lambda (x) (handler-case (read-avi x)
			  (t  (e) (format t "error whiler eading ~A: ~A~%" x e))))
	  (dir-avis dir)))

(defparameter *explosionen* (read-dir-avis "explosionen"))
(defparameter *hotspots* (read-dir-avis "hotspots"))
(defparameter *hotspots2* (concatenate 'list *explosionen* *hotspots*))
(defparameter *hotspots3* (read-dir-avis "hotspots-2"))
(defparameter *bestof* (read-dir-avis "bestof"))

#+nil
(defparameter *bombe-splits*
  (concatenate 'list
	       (repeat (split-avi *bombe1* 15) 10)
	       (split-avi *flugzeug1* 15)
	       (split-avi *lkw3* 15)
	       (split-avi *wurste* 15)
	       (split-avi *wueste2* 15)
	       (split-avi *popcar* 15)))
	       

#+nil
(defparameter *uebergang-split*
  (split-avi *uebergaenge* 10))

#+nil
(defun random-bombe ()
  (random-elt *bombe-splits*))

(defun random-bild (&key (len 10) (movie *avi*))
  (vid-frame-avi movie (random (length (vid-frames movie))) :len len))

(defun kaleidoscope (&key (len 3))
  (concatenate-avis (repeat-code (len)
		      (random-bild :len (1+ (random 3))))))

(defmacro repeat-code ((i) &body body)
  `(loop for idx from 0 upto ,i
	appending (list ,@body)))

(defun random-uebergang ()
  (random-elt *uebergang-split*))

#+nil(make-my-avi (concatenate-avis
	      (repeat-code (20)
		(random-bombe)
		#+nil(kaleidoscope :len 5)
		(random-uebergang)
		)))

(defmacro random-code (&body body)
  (let ((len (length body))
	(rand-value (gensym)))
    `(let ((,rand-value (random ,len)))
       (case ,rand-value
	 ,@(loop for code in body
		for i from 0
		collect `(,i ,code))))))
		

(defun accelerate (from acc steps)
  (loop repeat steps
	for start from from by acc
	collect start))

(defun accelerate2 (from acc2 steps)
  (loop for acc = 0 then (+ acc acc2)
	for speed = from then (+ speed acc)
	repeat steps
	collect speed))

(defmethod speed-avi ((avi avi) speed)
  (copy-avi :default avi :vid-frames
	    (loop with cur = 0
		  with len = (avi-num-frames avi)
	       while (< (round cur) len)
		 
		  collect (elt (vid-frames avi) (round cur))
		  while (< cur len)
		  do (incf cur speed))))

(defmethod beschleunig-avi ((avi avi) frame-speeds)
  (copy-avi :default avi :vid-frames
	    (loop with cur = 0
		  for speed in frame-speeds
	       while (< (round cur) (length (vid-frames avi)))
		  collect (elt (vid-frames avi) (round cur))
		  do (incf cur speed))))

(defun make-constant-speed (speed length)
  (make-list (round length) :initial-element speed))

(defun short-explosion (vid)
  (subsegment-avi vid :length (+ 4 (random 12))))

(defun short-explosion2 (vid)
  (subsegment-avi vid :length (+ 4 (random 7))))

(defun hotspot (num)
  (make-my-avi (concatenate-avis (repeat-code (num)
				   (let ((avi (random-elt *hotspots3*)))
				     (random-code
 				       (concatenate-avis (repeat-code ((+ 1 (random 3)))
							   (short-explosion2 avi)))
 				       #+nil(concatenate-avis (repeat-code ((+ 1 (random 3)))
							   (short-explosion avi)))
				       ;(short-explosion avi)
				       ;(short-explosion avi)
				       ;(short-explosion avi)
				       ))
				   #+nil(repeat-avi *black* (+ 3 (random 7)))
				   ))))

(defun change-avi-speed (avi speed)
  (beschleunig-avi avi (make-constant-speed speed (length (vid-frames avi)))))

(defparameter *fast-speed* 1.0)
(defparameter *slow-speed* 0.6)

(defun wobble-sequenz (avi &key (start 0))
    (concatenate-avis 
     (append

      (let ((avis (subsegment-avi-count avi 2)))
	(loop for avi in avis
	   collect (speed-avi avi (+ *slow-speed* (random *fast-speed*)))
	   collect (speed-avi (reverse-avi avi) (+ *slow-speed* (random *fast-speed*)))
	   collect (speed-avi avi  (+ *slow-speed* (random *fast-speed*)))

	     ))
      (let ((avis (subsegment-avi-count (reverse-avi avi) 2)))
	(loop for avi in avis
	   collect (speed-avi avi (+ *slow-speed* (random *fast-speed*)))
	   collect (speed-avi (reverse-avi avi) (+ *slow-speed* (random *fast-speed*)))
	   collect (speed-avi avi  (+ *slow-speed* (random *fast-speed*)))
	     )))))

(defun wobble-sequenz2 (avi)
  (concatenate-avis
   (list (speed-avi avi (+ *slow-speed* (random *fast-speed*)))
	 (speed-avi (reverse-avi avi) (+ *slow-speed* (random *fast-speed*))))))


(defun morph (avi num)
  (concatenate-avis (repeat-code (num)
		      (wobble-sequenz avi))))

(defun morph2 (avi num)
  (concatenate-avis (repeat-code (num)
		      (wobble-sequenz2 avi))))


(defun make-my-avi (avi)
  (write-avi-file (make-avi-file avi)
		  "/Users/manuel/tmp.avi"))


(defun output-morphs ()
  (loop for i from 6
       for avi in *bestof*
       do (format t "rendering ~A~%" avi)
       do (write-avi-file (make-avi-file (morph avi 8))
			  (format nil "/Users/manuel/bestof-~A-hinundher.avi" i))
       do (write-avi-file (make-avi-file (morph2 avi 15))
			  (format nil "/Users/manuel/bestof-~A.avi" i))))