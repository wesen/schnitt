(in-package :schnitt)

(defconstant +fps+ 25)

(defparameter *bpm* 144.0)

(defun bpm->frames (bpm)
  (/ (* 60 +fps+) bpm))

(defparameter *bpm-frames* (bpm->frames *bpm*))
(defparameter *16tel* (/ *bpm-frames* 4))

(defun frame-length (frames)
  (1+ (/ frames (bpm->frames *bpm*))))

(defun beat-length (beat)
  (* (1- beat) (bpm->frames *bpm*)))

(defparameter *takt-secs*  (/ (* 7 60.0) *bpm*))

(defparameter *takt-frames* (* *takt-secs* 25.0))

(defparameter *black*
  (read-avi-file "~/venetian/black.avi"))

;; 253 frames
(defparameter *aufhaeng*
  (read-avi-file "~/venetian/aufhaeng.avi"))

;; 728 frames
(defparameter *auftauch*
  (read-avi-file "~/venetian/auftauch.avi"))

;; 263 frames
(defparameter *auge*
  (read-avi-file "~/venetian/auge.avi"))

;; 223 frames
(defparameter *baumel*
  (read-avi-file "~/venetian/baumel.avi"))

;; 826 frames
(defparameter *fratze*
  (read-avi-file "~/venetian/fratze.avi"))

;; 271 frames
(defparameter *geist*
  (read-avi-file "~/venetian/geist.avi"))

;; 507 frames
(defparameter *reinrenn*
  (read-avi-file "~/venetian/reinrenn.avi"))

(defparameter *frau1*
  (read-avi-file "~/venetian/frau1.avi"))
(defparameter *frau2*
  (read-avi-file "~/venetian/frau2.avi"))
(defparameter *frau3*
  (read-avi-file "~/venetian/frau3.avi"))

(defparameter *vera1*
  (read-avi-file "~/venetian/vera1.avi"))
(defparameter *vera2*
  (read-avi-file "~/venetian/vera2.avi"))

(defparameter *hand*
  (read-avi-file "~/venetian/hand.avi"))


(defun beat->frame (beats)
  (mapcar #'beat-length beats))

(defun beat-offsets->avi-offsets (beat-offsets)
  (mapcar #'(lambda (beat-offset)
	      (list (first beat-offset)
		    (beat-length (second beat-offset))))
	  beat-offsets))

(defun make-black-background (len)
  (repeat-avi *black* len))

(defun schedule-avis (avi-offsets)
  (let* ((last-offset (first (last avi-offsets)))
	 (len (+ (avi-num-frames (first last-offset))
		 (second last-offset))))
    (insert-avis (make-black-background len) avi-offsets :insert nil :until-end nil)))


(defmethod split-avi-from-offsets ((avi avi) offsets)
  (loop with cur-start = (first offsets)
	for offset in (cdr offsets)
	collect (subsegment avi :start (round cur-start) :length (round (- offset cur-start)))
	do (setf cur-start offset)))

;; test1

(defun test-scene1 ()
  (let ((aufhaeng-segments (permutate-list (split-avi-no-shorter *aufhaeng* 18))))
    (flet ((random-aufhaeng-segment (len)
	     (subsegment (pop aufhaeng-segments) :length len))
	   (random-aufhaeng-flash (n length)
	     (subsegment (permutate-avi *aufhaeng* n) :length length))
	   (random-auge-flash (n length)
	     (subsegment (permutate-avi *auge* n) :length length)))
      (schedule-avis (beat-offsets->avi-offsets `((,(random-aufhaeng-segment 18) 1)
						  (,(random-aufhaeng-flash 1 2)  11/4)
						  (,(random-aufhaeng-segment 18) 3)
						  (,(random-aufhaeng-flash 1 2)  19/4)
						  (,(random-aufhaeng-segment 18) 20/4)
						  (,(random-auge-flash 1 2)  27/4)
						  (,(random-aufhaeng-segment 7)  28/4)
						  (,(random-auge-flash 1 2)  31/4)))))))

(defun frames-to-takt (frames)
  (/ (/ frames 25.0) *takt-secs*))


;;     1 . & . 2 . & . 3 . & . 4 . & . 5 . & . 6 . & . 7 . & . 

;; 1, 2, 3 normal
;; 4:  x               x               x       x   x   x x x x
;; 5, 6, 7 normal
;; 8:  x               x               x               x   x x
;; 9, 10, 11
;; 12: x               x               x       x   x   x x x x
;; 13, 14, 15 normal
;; 16: x               x               x


#+nil
(write-avi-file 
	  (schedule-avis (loop for i from 0 below 20
			       collect (list (test-scene1) (beat-length (1+ (* i 7.0))))))
	  "~/venetian/test-scene1.avi")

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
  (copy-avi avi :frames
	    (loop with cur = 0
		  with len = (avi-num-frames avi)
		  while (< cur len)
		  collect (elt (avi-frames avi) (round cur))
		  while (< cur len)
		  do (incf cur speed))))

(defmethod beschleunig-avi ((avi avi) frame-speeds)
  (copy-avi avi :frames
	    (loop with cur = 0
		  for speed in frame-speeds
		  collect (elt (avi-frames avi) (round cur))
		  do (incf cur speed))))

(defun make-constant-speed (speed length)
  (make-list (round length) :initial-element speed))

(defparameter *beat-frames* (beat-length 2))

(defparameter *takt-speeds*
  (append (make-constant-speed 0.3 10)
	  (accelerate2 0.3 0.04 11)
	  (make-constant-speed 0.3 10)
	  (accelerate2 0.3 0.04 11)
	  (make-constant-speed 0.3 10)
	  (accelerate2 0.3 0.04 10)
	  (accelerate2 0.3 0.04 11)))

(defun count-speed-frames (speeds)
  (round (reduce #'+ speeds :initial-value 0)))

(defun test-scene2 ()
  (let ((aufhaeng-segments (permutate-list (split-avi-no-shorter *aufhaeng* 54))))
    (flet ((random-aufhaeng-segment ()
	     (beschleunig-avi (pop aufhaeng-segments) *takt-speeds*))
	   (random-aufhaeng-flash (n length)
	     (subsegment (permutate-avi *aufhaeng* n) :length length))
	   (random-auge-flash (n length)
	     (subsegment (permutate-avi *auge* n) :length length)))
      (insert-avis (random-aufhaeng-segment) (beat-offsets->avi-offsets
					      `((,(random-auge-flash 1 2) 1)
						(,(random-auge-flash 1 2) 5)
						(,(random-aufhaeng-flash 1 5) 7)))
					      
		   :insert nil :until-end t))))

#+nil
(write-avi-file 
	  (schedule-avis (loop for i from 0 below 20
			       collect (list (test-scene2) (beat-length (1+ (* i 7.0))))))
	  "~/venetian/test-beschleunig3.avi")

;; erster abschnitt, takt 1 - 8, reinkommen
;; flashes: rumspringen, auge

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

(defun random-erster-abschnitt-speed ()
  (random-elt *erster-abschnitt-speeds*))

(defun reinrenn-flash (n length)
  (subsegment (permutate-avi *reinrenn* n) :length length))

(defun augen-flash (n length)
  (subsegment (permutate-avi *auge* n) :length length))

(defun tod-flash (n length)
  (subsegment (permutate-avi *baumel* n) :length length))

(defun laechel-flash (n length)
  (subsegment (permutate-avi *frau2* n) :length length))

(defmacro random-choice (&rest choices)
  (let ((rand (gensym)))
  `(let ((,rand (random 1.0)))
    (cond ,@(loop for choice in choices
		  for p = (first choice) then (if (eql (first choice) t)
						  1.0
						  (+ p (first choice)))
		  when (> p 1.0)
		  do (error "p can not be > 1.0")
		  collect `((< ,rand ,p) ,@(cdr choice)))))))


(defun erster-abschnitt-flash (length)
  (random-choice (0.2 (augen-flash 1 length))
		 (t (reinrenn-flash 1 length))))

(defun erster-abschnitt-simple-muster (&optional (start 0) (func #'erster-abschnitt-flash)
				       &key (p 0.6))
  (remove nil
	  `(,(when (< (random 1.0) p)
	      `(,(funcall func 2) ,(+ start 1)))
	    ,(when (< (random 1.0) p)
	      `(,(funcall func 2) ,(+ start 3)))
	    ,(when (< (random 1.0) p)
	      `(,(funcall func 2) ,(+ start 5)))
	    ,(when (< (random 1.0) p)
	     `(,(funcall func 4) ,(+ start 7))))))

(defparameter *erster-abschnitt-grundfilm* nil)
(defparameter *zweiter-abschnitt-grundfilm* nil)

(defun reinrenn-grundfilm ()
  (let* ((reinrenn (repeat-loop-avi *reinrenn* 1))
	 (film
	  (schedule-avis (loop for i from 0 below 16
			       with start = 0
			       for speed = (random-erster-abschnitt-speed)
			       for segment = (beschleunig-avi (subsegment reinrenn :start start) speed)
			       collect (list segment (beat-length (1+ (* 7.0 i))))
			       do (incf start (count-speed-frames speed))))))
    (setf *erster-abschnitt-grundfilm* (subsegment film :length (round (* 8 *takt-frames*)))
	  *zweiter-abschnitt-grundfilm* (subsegment film :start (round (* 8 *takt-frames*))
						    :length (round (* 8 *takt-frames*))))))


(defun erster-abschnitt ()
  (insert-avis *erster-abschnitt-grundfilm*
	       (beat-offsets->avi-offsets
		(append `((,(erster-abschnitt-flash 2) 3)
			  (,(erster-abschnitt-flash 2) 5)
			  (,(erster-abschnitt-flash 4) 7))
			(erster-abschnitt-simple-muster 7)
			(erster-abschnitt-simple-muster 14)
			`((,(erster-abschnitt-flash 2) 22)
			  (,(erster-abschnitt-flash 2) 24)
			  (,(erster-abschnitt-flash 2) 28)
			  (,(erster-abschnitt-flash 10) 28))
			(erster-abschnitt-simple-muster 28)
			(erster-abschnitt-simple-muster 35)
			(erster-abschnitt-simple-muster 42)
			`((,(erster-abschnitt-flash 2) 50)
			  (,(erster-abschnitt-flash 2) 52)
			  (,(erster-abschnitt-flash 2) 54)
			  (,(erster-abschnitt-flash 10) 56))))
	       :insert nil))


(defun zweiter-abschnitt-flash (length)
  (random-choice (0.1 (tod-flash 1 length))
		 (0.2 (augen-flash 1 length))
		 (0.2 (reinrenn-flash 1 length))
		 (t (laechel-flash 2 (* length 2)))))

(defun zweiter-abschnitt-simple-muster (&optional (start 0) (length 1)
					(func #'zweiter-abschnitt-flash)
					&key (p 0.6))
  
  (remove nil
	  `(,(when (< (random 1.0) p)
	      `(,(funcall func (* length 2)) ,(+ start 1)))
	    ,(when (< (random 1.0) p)
	      `(,(funcall func (* length 2)) ,(+ start 3)))
	    ,(when (< (random 1.0) p)
	      `(,(funcall func (* length 2)) ,(+ start 5)))
	    ,(when (< (random 1.0) p)
	     `(,(funcall func (* length 4)) ,(+ start 7))))))

(defun zweiter-abschnitt ()
  (insert-avis *zweiter-abschnitt-grundfilm*
	       (beat-offsets->avi-offsets
		(append (zweiter-abschnitt-simple-muster 0 2)
			(zweiter-abschnitt-simple-muster 7 2)
			(zweiter-abschnitt-simple-muster 14 2)
			`((,(zweiter-abschnitt-flash 2) 22)
			  (,(zweiter-abschnitt-flash 3) 24)
			  (,(zweiter-abschnitt-flash 4) 26)
			  (,(zweiter-abschnitt-flash 10) 28))
			(zweiter-abschnitt-simple-muster 28 2)
			(zweiter-abschnitt-simple-muster 35 2)
			(zweiter-abschnitt-simple-muster 42 2)
			`((,(zweiter-abschnitt-flash 4) 50)
			  (,(zweiter-abschnitt-flash 3) 52)
			  (,(zweiter-abschnitt-flash 2) 54)
			  (,(augen-flash 1 10) 56))))
	       :insert nil))
	       

(defun reinrenn ()
  (reinrenn-grundfilm)
  (concatenate-avis (list (erster-abschnitt) (zweiter-abschnitt))))

(defun laecheln-grundfilm (&optional (length 8))
  (schedule-avis (loop for i from 0 below length
		       with max-count = (apply #'max
					       (mapcar #'count-speed-frames
						       *erster-abschnitt-speeds*))
		       
		       for avi in (permutate-list (mapcan #'(lambda (x)
							      (split-avi-no-shorter x max-count))
							  (list *frau1* *frau2* *frau3*)))
		       
		       for speed = (random-erster-abschnitt-speed)
		       for segment = (beschleunig-avi avi speed)
		       collect (list segment (beat-length (1+ (* 7.0 i)))))))

(defun fratzen-flash (n length)
  (subsegment (permutate-avis (list *fratze* *vera1* *vera2*) n) :length length))

(defun dritter-abschnitt-flash (length)
  (random-choice (0.1 (tod-flash 1 length))
		 (0.2 (laechel-flash 1 length))
		 (0.4 (reinrenn-flash 1 length))
		 (t (fratzen-flash 2 (* length 2)))))

(defun vierter-abschnitt-flash (length)
  (random-choice (0.2 (tod-flash 1 length))
		 (0.3 (laechel-flash 1 length))
		 (0.2 (reinrenn-flash 1 length))
		 (0.2 (augen-flash 1 length))
		 (t (fratzen-flash 2 (* length 2)))))

(defun dritter-abschnitt ()
  (insert-avis (laecheln-grundfilm)
	       (beat-offsets->avi-offsets
		(append (erster-abschnitt-simple-muster 0 #'dritter-abschnitt-flash :p 0.8)
			(erster-abschnitt-simple-muster 7 #'dritter-abschnitt-flash :p 0.8)
			(erster-abschnitt-simple-muster 14 #'dritter-abschnitt-flash :p 0.8)
			`((,(dritter-abschnitt-flash 2) 22)
			  (,(dritter-abschnitt-flash 3) 24)
			  (,(dritter-abschnitt-flash 4) 26)
			  (,(augen-flash 1 10) 28))
			(erster-abschnitt-simple-muster 28 #'dritter-abschnitt-flash :p 0.8)
			(erster-abschnitt-simple-muster 35 #'dritter-abschnitt-flash :p 0.8)
			(erster-abschnitt-simple-muster 42 #'dritter-abschnitt-flash :p 0.8)
			`((,(dritter-abschnitt-flash 4) 50 #'dritter-abschnitt-flash)
			  (,(dritter-abschnitt-flash 4) 52 #'dritter-abschnitt-flash)
			  (,(dritter-abschnitt-flash 4) 54 #'dritter-abschnitt-flash)
			  (,(augen-flash 1 10) 56))))
	       :insert nil))

(defun vierter-abschnitt ()
  (insert-avis (laecheln-grundfilm 4)
	       (beat-offsets->avi-offsets
		(append (zweiter-abschnitt-simple-muster 0 2 #'vierter-abschnitt-flash :p 0.8)
			(zweiter-abschnitt-simple-muster 7 2 #'vierter-abschnitt-flash :p 0.8)
			(zweiter-abschnitt-simple-muster 14 2 #'vierter-abschnitt-flash :p 0.8)
			`((,(vierter-abschnitt-flash 4) 22)
			  (,(vierter-abschnitt-flash 4) 24)
			  (,(vierter-abschnitt-flash 4) 26)
			  (,(reinrenn-flash 1 10) 28))))
	       :insert nil))

(defun hand-flash (n length)
  (subsegment (permutate-avi *hand* n) :length length))

(defun auftauch-flash (n length)
  (subsegment (permutate-avi *auftauch* n) :length length))

(defun fuenfter-abschnitt-flash (length)
  (random-choice (0.3 (tod-flash 2 (* length 2)))
		 (0.1 (laechel-flash 1 length))
		 (0.1 (fratzen-flash 1 length))
		 (0.2 (auftauch-flash 1 length))
		 (t (hand-flash 2 (* length 2)))))


(defun auftauchen-grundfilm ()
  (let ((auftauchen (repeat-loop-avi *auftauch* 2)))
    (schedule-avis (loop for i from 0 below 12
			 with start = 0
			 for speed = (random-erster-abschnitt-speed)
			 for segment = (beschleunig-avi (subsegment auftauchen :start start) speed)
			 collect (list segment (beat-length (1+ (* 7.0 i))))
			 do (incf start (count-speed-frames speed))))))

(defun fuenfter-abschnitt ()
  (insert-avis (auftauchen-grundfilm)
	       (beat-offsets->avi-offsets
		(append (erster-abschnitt-simple-muster 0 #'fuenfter-abschnitt-flash :p 0.7)
			(erster-abschnitt-simple-muster 7 #'fuenfter-abschnitt-flash :p 0.7)
			(erster-abschnitt-simple-muster 14 #'fuenfter-abschnitt-flash :p 0.7)
			`((,(fuenfter-abschnitt-flash 2) 22)
			  (,(fuenfter-abschnitt-flash 3) 24)
			  (,(fuenfter-abschnitt-flash 4) 26)
			  (,(fuenfter-abschnitt-flash 10) 28))
			(erster-abschnitt-simple-muster 28 #'fuenfter-abschnitt-flash :p 0.7)
			(erster-abschnitt-simple-muster 35 #'fuenfter-abschnitt-flash :p 0.7)
			(erster-abschnitt-simple-muster 42 #'fuenfter-abschnitt-flash :p 0.7)
			`((,(fuenfter-abschnitt-flash 4) 50)
			  (,(fuenfter-abschnitt-flash 4) 52)
			  (,(fuenfter-abschnitt-flash 4) 54)
			  (,(hand-flash 1 10) 56))
			(erster-abschnitt-simple-muster 56 #'fuenfter-abschnitt-flash :p 0.7)
			(erster-abschnitt-simple-muster 63 #'fuenfter-abschnitt-flash :p 0.7)
			(erster-abschnitt-simple-muster 70 #'fuenfter-abschnitt-flash :p 0.7)
			`((,(fuenfter-abschnitt-flash 4) 77)			  (,(fuenfter-abschnitt-flash 4) 79)
			  (,(fuenfter-abschnitt-flash 4) 81)
			  (,(hand-flash 1 10) 83))))
			
	       :insert nil))

(defun skip2 (list)
  (loop for i on list by #'cddr
	collect (car i)))

(defun flimmer-aufhaeng-avis ()
  (mapcar #'(lambda (avi) (subsegment (flimmer-avis (split-avi avi 4) 2) :length 73))
	  (split-avi-no-shorter *auftauch* 60)))

(defun flimmer-laecheln-avis ()
  (let ((slow-avis (permutate-list
		    (mapcar #'(lambda (avi) (beschleunig-avi avi (make-constant-speed 0.3 36)))
			    (split-avi-no-shorter *frau1* 30)))))
    (loop for (avi1 avi2) on slow-avis by #'cddr
	  when (and avi1 avi2)
	  collect (interleave-avis (list avi1 avi2) 1))))

(defun flimmer-baumel-avis ()
  (mapcar #'(lambda (avi) (subsegment (flimmer-avis (permutate-avi-to-segments avi 4) 2)
				      :length 73))
	  (split-avi-no-shorter *baumel* 60)))



(defun sechster-abschnitt-grundfilm ()
  (let ((laecheln-avis (permutate-list (append (flimmer-laecheln-avis)
					       (flimmer-laecheln-avis))))
	(aufhaeng-avis (permutate-list (append (flimmer-aufhaeng-avis)
					       (flimmer-aufhaeng-avis))))
	(baumel-avis (permutate-list (append (flimmer-baumel-avis)
					     (flimmer-baumel-avis)))))
    (schedule-avis (beat-offsets->avi-offsets
		    `((,(pop laecheln-avis) 1)
		      (,(uebergang (pop laecheln-avis) (pop aufhaeng-avis) 7) ,(1+ (* 1 7)))
		      (,(pop laecheln-avis) ,(1+ (* 2 7)))
		      (,(uebergang (pop laecheln-avis) (pop aufhaeng-avis) 7) ,(1+ (* 3 7)))

		      (,(pop laecheln-avis) ,(1+ (* 4 7)))
		      (,(uebergang (pop laecheln-avis) (pop aufhaeng-avis) 7) ,(1+ (* 5 7)))
		      (,(pop aufhaeng-avis) ,(1+ (* 6 7)))
		      (,(uebergang (pop aufhaeng-avis) (pop baumel-avis) 7) ,(1+ (* 6 7)))
		      )))))

(defun baumel-flash (n length)
  (subsegment (permutate-avi *baumel* n) :length length))

(defun sechster-abschnitt-flash (length)
  (random-choice (0.4 (baumel-flash 2 (* length 2)))
		 (0.2 (laechel-flash 1 length))
		 (0.1 (fratzen-flash 1 length))
		 (t (hand-flash 2 (* length 2)))))

(defun sechster-abschnitt ()
  (insert-avis (sechster-abschnitt-grundfilm)
	       (beat-offsets->avi-offsets
		(append (erster-abschnitt-simple-muster 0 #'sechster-abschnitt-flash :p 0.5)
			(erster-abschnitt-simple-muster 7 #'sechster-abschnitt-flash :p 0.5)
			(erster-abschnitt-simple-muster 14 #'sechster-abschnitt-flash :p 0.5)
			`((,(sechster-abschnitt-flash 2) 22)
			  (,(sechster-abschnitt-flash 2) 24)
			  (,(sechster-abschnitt-flash 3) 26)
			  (,(sechster-abschnitt-flash 10) 28))
			(erster-abschnitt-simple-muster 28 #'sechster-abschnitt-flash :p 0.6)
			(erster-abschnitt-simple-muster 35 #'sechster-abschnitt-flash :p 0.7)
			(erster-abschnitt-simple-muster 42 #'sechster-abschnitt-flash :p 0.7)
			`((,(sechster-abschnitt-flash 2) 50)
			  (,(sechster-abschnitt-flash 2) 52)
			  (,(sechster-abschnitt-flash 3) 54)
			  (,(sechster-abschnitt-flash 10) 56))))
			
	       :insert nil))

(defun baumel-grundfilm ()
  (let ((baumel (repeat-loop-avi *baumel* 2)))
    (schedule-avis (loop for i from 0 below 9
			 with start = 0
			 for speed = (random-erster-abschnitt-speed)
			 for segment = (beschleunig-avi (subsegment baumel :start start) speed)
			 collect (list segment (beat-length (1+ (* 7.0 i))))
			 do (incf start (count-speed-frames speed))))))

(defun siebter-abschnitt-flash (length)
  (random-choice (0.4 (baumel-flash 2 (* length 2)))
		 (0.3 (fratzen-flash 1 length))
		 (t (hand-flash 2 (* length 2)))))

(defun siebter-abschnitt ()
  (insert-avis (baumel-grundfilm)
	       (beat-offsets->avi-offsets
		(append (zweiter-abschnitt-simple-muster 0 1 #'siebter-abschnitt-flash :p 0.7)
			(zweiter-abschnitt-simple-muster 7 1 #'siebter-abschnitt-flash :p 0.7)
			(zweiter-abschnitt-simple-muster 14 1 #'siebter-abschnitt-flash :p 0.7)
			`((,(siebter-abschnitt-flash 2) 22)
			  (,(siebter-abschnitt-flash 3) 24)
			  (,(siebter-abschnitt-flash 4) 26)
			  (,(siebter-abschnitt-flash 10) 28))
			(zweiter-abschnitt-simple-muster 28 2 #'siebter-abschnitt-flash :p 0.8)
			(zweiter-abschnitt-simple-muster 35 2 #'siebter-abschnitt-flash :p 0.8)
			(zweiter-abschnitt-simple-muster 42 2 #'siebter-abschnitt-flash :p 0.8)
			`((,(siebter-abschnitt-flash 2) 50)
			  (,(siebter-abschnitt-flash 3) 52)
			  (,(siebter-abschnitt-flash 4) 54)
			  (,(siebter-abschnitt-flash 10) 56))
			`((,(siebter-abschnitt-flash 2) 57)
			  (,(siebter-abschnitt-flash 3) 59)
			  (,(siebter-abschnitt-flash 4) 61)
			  (,(augen-flash 1 10) 63))))
			
	       :insert nil))


(defun geist-grundfilm ()
  (let ((geist (repeat-loop-avi *geist* 2)))
    (schedule-avis (loop for i from 0 below 8
			 with start = 0
			 for speed = (random-erster-abschnitt-speed)
			 for segment = (beschleunig-avi (subsegment geist :start start) speed)
			 collect (list segment (beat-length (1+ (* 7.0 i))))
			 do (incf start (count-speed-frames speed))))))

(defun achter-abschnitt-flash (length)
  (random-choice (0.3 (baumel-flash 2 (* length 2)))
		 (0.2 (reinrenn-flash 1 length))
		 (0.3 (laechel-flash 1 length))
		 (t (augen-flash 1 length))))

(defun achter-abschnitt ()
  (insert-avis (geist-grundfilm)
	       (beat-offsets->avi-offsets
		(append `((,(achter-abschnitt-flash 2) 1))
			(erster-abschnitt-simple-muster 14  #'achter-abschnitt-flash :p 0.6)
			`((,(achter-abschnitt-flash 2) 22)
			  (,(achter-abschnitt-flash 3) 24)
			  (,(achter-abschnitt-flash 4) 26)
			  (,(achter-abschnitt-flash 10) 28))
			(erster-abschnitt-simple-muster 28 #'achter-abschnitt-flash :p 0.6)
			(erster-abschnitt-simple-muster 35 #'achter-abschnitt-flash :p 0.6)
			(erster-abschnitt-simple-muster 42 #'achter-abschnitt-flash :p 0.6)
			`((,(achter-abschnitt-flash 2) 50)
			  (,(achter-abschnitt-flash 3) 52)
			  (,(achter-abschnitt-flash 4) 54)
			  (,(achter-abschnitt-flash 10) 56))))
			
	       :insert nil))

(defun flimmer-geist-avis ()
  (mapcar #'(lambda (avi) (subsegment (flimmer-avis (permutate-avi-to-segments avi 4) 2)
				      :length 73))
	  (split-avi-no-shorter *geist* 60)))

(defun neunter-abschnitt-grundfilm ()
  (let ((laecheln-avis (permutate-list (append (flimmer-laecheln-avis)
					       (flimmer-laecheln-avis))))
	(geist-avis (permutate-list (append (flimmer-geist-avis)
					       (flimmer-geist-avis))))
	(baumel-avis (permutate-list (append (flimmer-baumel-avis)
					     (flimmer-baumel-avis)))))
    (schedule-avis (beat-offsets->avi-offsets
		    `((,(pop geist-avis) 1)
		      (,(uebergang (pop laecheln-avis) (pop baumel-avis) 7) ,(1+ (* 1 7)))
		      (,(pop geist-avis) ,(1+ (* 2 7)))
		      (,(uebergang (pop laecheln-avis) (pop baumel-avis) 7) ,(1+ (* 3 7))))))))

(defun neunter-abschnitt ()
  (insert-avis (neunter-abschnitt-grundfilm)
	       (beat-offsets->avi-offsets
		(append (erster-abschnitt-simple-muster 0  #'achter-abschnitt-flash :p 0.5)
			(erster-abschnitt-simple-muster 7  #'achter-abschnitt-flash :p 0.4)
			(erster-abschnitt-simple-muster 14  #'achter-abschnitt-flash :p 0.3)
			(erster-abschnitt-simple-muster 21  #'achter-abschnitt-flash :p 0.2)))
			
	       :insert nil))

(defun movie ()
  (schedule-avis (beat-offsets->avi-offsets
		  `((,(reinrenn) 1)
		    (,(dritter-abschnitt) ,(1+ (* 16 7)))
		    (,(vierter-abschnitt) ,(1+ (* 24 7)))
		    (,(fuenfter-abschnitt) ,(1+ (* 28 7)))
		    (,(sechster-abschnitt) ,(1+ (* 40 7)))
		    (,(siebter-abschnitt) ,(1+ (* 48 7)))
		    (,(achter-abschnitt) ,(1+ (* 57 7)))
		    (,(neunter-abschnitt) ,(1+ (* 65 7)))))))
		    