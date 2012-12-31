(in-package :schnitt)

;; helpers

(defun first-elts (list num)
  (loop for i from 0 below num
	for x in list
	collect x))

(define-binary-class waveformatex (wave-fmt-chunk)
  ((extra-size le-u2)
   (extra-data (raw-bytes :size extra-size))))

(define-binary-class bitmapinfo (riff-chunk)
  ((bi-size   le-u4)
   (width  le-u4)
   (height le-u4)
   (planes le-u2)
   (bitcount le-u2)
   (compression le-u4)
   (size-image le-u4)
   (xpels-per-meter le-u4)
   (ypels-per-meter le-u4)
   (clr-used le-u4)
   (clr-important le-u4)
   (data (raw-bytes :size (- size 40)))))

(define-riff-chunk avi-stream-header "strh" ()
  ((fcc-type              fourcc)
   (fcc-handler           fourcc)
   (flags                 le-u4)
   (priority              le-u2)
   (language              le-u2)
   (initial-frames        le-u4)
   (scale                 le-u4)
   (rate                  le-u4)
   (start                 le-u4)
   (stream-length         le-u4)
   (suggested-buffer-size le-u4)
   (quality               le-u4)
   (sample-size           le-u4)
   (left                  le-u2)
   (top                   le-u2)
   (right                 le-u2)
   (bottom                le-u2)))

(define-riff-chunk avi-main-header "avih" ()
  ((ms-per-frame le-u4)
   (max-bytes-per-sec le-u4)

   (padding-granularity le-u4)
   (flags le-u4)
   (total-frames le-u4)
   (initial-frames le-u4)
   (streams le-u4)
   (suggested-buffer-size le-u4)
   (width le-u4)
   (height le-u4)
   (reserved (raw-bytes :size 16))))

;; avi file

(define-binary-class avi-file ()
  ((hdr riff-hdr)
   (hdrl riff-chunk)
   (chunks (riff-chunks :size (- (size hdr) (size hdrl) 12)))))

(defun read-avi-file (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (read-value 'avi-file in)))

(defmethod write-avi-file ((avi avi-file) file)
  (with-open-file (out file :direction :output :if-exists :supersede
		       :element-type '(unsigned-byte 8))
    (write-object avi out)))

(defclass avi ()
  ((avi-hdr :initarg :avi-hdr :reader avi-hdr)
   (avi-vid-strh :initarg :avi-vid-strh :accessor avi-vid-strh)
   (avi-vid-strf :initarg :avi-vid-strf :accessor avi-vid-strf) 
   (avi-aud-strh :initarg :avi-aud-strh :accessor avi-aud-strh)
   (avi-aud-strf :initarg :avi-aud-strf :accessor avi-aud-strf)
   (vid-frames :initarg :vid-frames :accessor vid-frames)
   (aud-frames :initarg :aud-frames :accessor aud-frames)))

(defmethod avi-vid-duration ((avi avi))
  (/ (* (ms-per-frame (avi-hdr avi))
	(length (vid-frames avi))) 1000000.0))

(defun find-riff-lists (type chunks)
  (remove-if-not #'(lambda (chunk) (and (typep chunk 'riff-list)
			       (string-equal (list-type chunk) type)))
	chunks))

(defun find-riff-chunks (id chunks)
  (remove-if-not #'(lambda (chunk) (string-equal (id chunk) id)) chunks))

(defmethod make-avi ((file avi-file))
  (with-slots (chunks hdrl) file
    (let* ((movi (first (find-riff-lists "movi" chunks)))
	   (avih (first (find-riff-chunks "avih" (chunks hdrl))))
	   (strls (find-riff-lists "strl" (chunks hdrl)))
	   (aud-frames (remove-if-not #'(lambda (chunk)
					  (string-equal (subseq (id chunk) 2) "wb"))
				      (chunks movi)))
	   (vid-frames (remove-if-not #'(lambda (chunk)
					  (or (string-equal (subseq (id chunk) 2) "db")
					      (string-equal (subseq (id chunk) 2) "dc")))
				      (chunks movi)))
	   aud-strh aud-strf vid-strh vid-strf)
      (loop for strl in strls
	    for strh = (first (find-riff-chunks "strh" (chunks strl)))
	    for strf = (first (find-riff-chunks "strf" (chunks strl)))
;	    do (format t "strh ~A strf ~A~%" strh strf)
	    when (string-equal (fcc-type strh) "auds")
	    do (setf aud-strh strh aud-strf strf)
	    when (string-equal (fcc-type strh) "vids")
	    do (setf vid-strh strh vid-strf strf))
      (make-instance 'avi :vid-frames vid-frames
		     :aud-frames aud-frames
		     :avi-hdr avih
		     :avi-vid-strf vid-strf
		     :avi-vid-strh vid-strh
		     :avi-aud-strf aud-strf
		     :avi-aud-strh aud-strh))))

(defmethod aud-stream-p ((avi avi))
  (with-slots (avi-aud-strh avi-aud-strf aud-frames) avi
    (and avi-aud-strh avi-aud-strf aud-frames)))

(defmethod vid-stream-p ((avi avi))
  (with-slots (avi-vid-strh avi-vid-strf vid-frames) avi
    (and avi-vid-strh avi-vid-strf vid-frames)))

(defun make-riff-list (type chunks)
  (make-instance 'riff-list :id "LIST" :list-type type :chunks chunks :size 0))

(defmethod avi-num-stream ((avi avi))
  (+ (if (vid-stream-p avi) 1 0)
     (if (aud-stream-p avi) 1 0)))

(defmethod avi-aud-stream-num ((avi avi))
  (if (vid-stream-p avi) 1 0))

(defmethod avi-vid-stream-num ((avi avi))
  0)

(defmethod update-riff-id ((chunk generic-riff-chunk) (avi avi))
  (let ((type (subseq (id chunk) 2)))
    (cond ((or (string-equal type "db")
	       (string-equal type "dc"))
	   (make-instance 'generic-riff-chunk
			  :id (format nil "~2,'0D~A" (avi-vid-stream-num avi) type)
			  :size (size chunk)
			  :data (data chunk)))
	  ((string-equal type "wb")
	   (make-instance 'generic-riff-chunk
			  :id (format nil "~2,'0D~A" (avi-aud-stream-num avi) type)
			  :size (size chunk)
			  :data (data chunk)))
	  (t (make-instance 'generic-riff-chunk :id (id chunk)
			    :size (size chunk)
			    :data (data chunk))))))

(defmethod avi-interleave-frames ((avi avi))
  (with-slots (aud-frames vid-frames) avi
    (cond ((and (vid-stream-p avi)
		(null aud-frames))
	   (mapcar #'(lambda (frame) (update-riff-id frame avi)) vid-frames))
	  ((and (aud-stream-p avi)
		(null vid-frames))
	   (mapcar #'(lambda (frame) (update-riff-id frame avi)) aud-frames))
	  (t (loop for vid on vid-frames by #'(lambda (x) (nthcdr 25 x))
		   for aud in aud-frames
		   appending (mapcar #'(lambda (frame) (update-riff-id frame avi))
				     (first-elts vid 25))
		   collect (update-riff-id aud avi))))))

(define-binary-class idx1-entry ()
  ((id    fourcc)
   (flags le-u4)
   (pos   le-u4)
   (size  le-u4)))

(define-binary-type idx1-entries (size)
  (:reader (in)
     (loop with to-read = size
	   while (plusp to-read)
	   for entry = (read-value 'idx1-entry in)
	   while entry
	   do (decf to-read 16)
	   collect entry))
  (:writer (out entries)
    (declare (ignore size))
    (dolist (entry entries)
      (write-value 'idx1-entry out entry))))

(define-riff-chunk idx1-chunk "idx1" ()
  ((entries (idx1-entries :size size))))

(defun make-idx1 (frames)
  (let* ((entries (loop with pos = 4
			for frame in frames
			collect (make-instance 'idx1-entry :id (id frame)
					       :flags 16
					       :size (size frame)
					       :pos pos)
			do (incf pos (+ 8 (size frame))))))
    (make-instance 'idx1-chunk :id "idx1" :size 0 :entries entries)))

(defmethod make-avi-file ((avi avi))
  (let* ((hdr (make-instance 'riff-hdr :id "RIFF" :riff-type "AVI " :size 0))
	 (avi-hdr (copy-object (avi-hdr avi)))
	 (avi-aud-strh (copy-object (avi-aud-strh avi)))
	 (avi-vid-strh (copy-object (avi-vid-strh avi)))
	 (aud-hdr (when (aud-stream-p avi)
		    (let* ((aud-size (reduce #'+ (aud-frames avi) :key #'size :initial-value 0))
			   (length (/ aud-size (scale avi-aud-strh))))
		      (setf (stream-length avi-aud-strh) length)
		      (make-riff-list "strl" (list avi-aud-strh (avi-aud-strf avi))))))
	 (vid-hdr (when (vid-stream-p avi)
		    (setf (stream-length avi-vid-strh) (length (vid-frames avi)))
		    (make-riff-list "strl" (list (copy-object (avi-vid-strh avi))
						 (avi-vid-strf avi)))))
	 (hdrl (make-riff-list "hdrl" (cons avi-hdr (remove nil (list vid-hdr aud-hdr)))))
	 (frames (avi-interleave-frames avi))
	 (movi (make-riff-list "movi" frames)))
    (setf (total-frames avi-hdr) (length frames))
    (make-instance 'avi-file
		   :hdr  hdr
		   :hdrl hdrl
		   :chunks (list movi (make-idx1 frames)))))
