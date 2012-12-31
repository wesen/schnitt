(in-package :schnitt)

;; audio manipulation

(defmethod make-avi-aud-hdrs ((wave wave-file))
  (let ((fmt (first (find-riff-chunks "fmt " (chunks wave))))
	(data (first (find-riff-chunks "data" (chunks wave)))))
    (list (make-instance 'avi-stream-header :id "strh" :size 0
			 :fcc-type "auds" :fcc-handler "    "
			 :flags 0 :priority 0
			 :language 0 :initial-frames 0
			 :start 0
			 :scale (block-align fmt)
			 :rate (* (block-align fmt) (samples-per-sec fmt))
			 :stream-length (/ (size data) (block-align fmt))
			 :suggested-buffer-size (+ 8 (* (block-align fmt) (samples-per-sec fmt)))
			 :quality 0 :sample-size (block-align fmt)
			 :left 0 :top 0 :right 0 :bottom 0)
	  (make-instance 'waveformatex
			 :id "strf" :size 0
			 :format-tag (format-tag fmt)
			 :channels (channels fmt)
			 :samples-per-sec (samples-per-sec fmt)
			 :avg-bytes-per-sec (avg-bytes-per-sec fmt)
			 :block-align (block-align fmt)
			 :bits-per-sample (bits-per-sample fmt)
			 :extra-size 0
			 :extra-data (make-array 0)))))

(defmethod split-wave-data ((wave wave-file) &key (start-secs 0) length-secs segment-len-secs)
  (let* ((fmt (first (find-riff-chunks "fmt " (chunks wave))))
	 (data (data (first (find-riff-chunks "data" (chunks wave)))))
	 (start-byte (chunk-position data)))
    (with-slots (samples-per-sec block-align) fmt
      (let* ((start-sample (round (* start-secs samples-per-sec)))
	     (end-sample (if length-secs
			     (round (min (/ (size data) block-align)
					 (* (+ start-secs length-secs) samples-per-sec)))
			     (round (/ (size data) block-align))))
	     (segments (if segment-len-secs
			   (loop for end from start-sample by (* segment-len-secs samples-per-sec)
				 for seg-end = (min end-sample end)
				 with start = nil 
				 when (and start end)
				 collect (list (* block-align (round start))
					       (* block-align (round (- end start))))
				 do (setf start end)
				 until (>= seg-end end-sample))
			   (list (list (* block-align start-sample)
				       (* block-align end-sample))))))
	(mapcar #'(lambda (segment)
		    (make-instance 'generic-riff-chunk
				   :id "00wb"
				   :size (second segment)
				   :data (make-instance
					  'file-chunk
					  :filename (filename data)
					  :position (+ start-byte (first segment))
					  :size (second segment))))
		segments)))))

(defmethod avi-insert-wave ((avi avi) (wave wave-file) segments)
  (destructuring-bind (strh strf) (make-avi-aud-hdrs wave)
    (setf (avi-aud-strh avi) strh
	  (avi-aud-strf avi) strf
	  (aud-frames avi) segments)))

(defmethod avi-add-audio ((avi avi) (wave wave-file) &key (start 0.0))
  (let ((segments (split-wave-data wave :start-secs start
				   :length-secs (avi-vid-duration avi)
				   :segment-len-secs 1.0)))
    (avi-insert-wave avi wave segments)))

(defmethod extract-wave ((avi avi))
  (let ((strf (avi-aud-strf avi)))
    (make-instance 'wave-file :hdr (make-instance 'riff-hdr :id "RIFF" :riff-type "WAVE"
						  :size 0)
		   :chunks
		   (list
		    (make-instance 'wave-fmt-chunk
				   :id "fmt "
				   :size 0
				   :format-tag (format-tag strf)
				   :channels (channels strf)
				   :samples-per-sec (samples-per-sec strf)
				   :avg-bytes-per-sec (avg-bytes-per-sec strf)
				   :block-align (block-align strf)
				   :bits-per-sample (bits-per-sample strf))
		    (make-instance 'generic-riff-chunk
				   :id "data"
				   :size 0
				   :data (make-instance 'multi-file-chunk
							:chunks (mapcar #'data
									(aud-frames avi))))))))

(defmethod strip-wave ((avi avi))
  (with-slots (avi-aud-strf avi-aud-strh aud-frames) avi
    (setf avi-aud-strh nil
	  avi-aud-strf nil
	  aud-frames nil)))

;; frame manipulation, won't work on audio

(defun concatenate-avis (avis)
  (copy-avi :default (first avis)
	    :vid-frames (reduce #'nconc (mapcar #'(lambda (avi)
						    (copy-list (vid-frames avi))) avis))))

(defmethod subsegment-avi-count ((avi avi) count)
  (let ((len (round (/ (length (vid-frames avi)) count))))
    (loop for start from 0 upto (1- (length (vid-frames avi))) by len
       collect (subsegment-avi avi :start start :length len))))

(defmethod subsegment-avi ((avi avi) &key start length random)
  (let ((duration (length (vid-frames avi))))
    (if random
	(progn
	  (unless start
	    (setf start (floor (random duration))))
	  (unless length
	    (setf length (floor (random (- duration start))))))
	(unless start
	  (setf start 0)))
    (setf length (min length (- duration start)))
    (copy-avi :default avi
	      :vid-frames (subseq (vid-frames avi) start
				  (when length (+ start length))))))

(defmethod split-avi ((avi avi) block-size)
  (let ((groups (group-by (vid-frames avi) block-size)))
    (mapcar #'(lambda (frames)
		(copy-avi :default avi
			  :vid-frames frames)) groups)))

(defmethod reverse-avi ((avi avi))
  (copy-avi :default avi
	    :vid-frames (reverse (vid-frames avi))))

(defmethod permutate-avi ((avi avi) block-size)
  (concatenate-avis (permutate-list (split-avi avi block-size))))

(defun permutate-avis (avis block-size)
  (concatenate-avis (permutate-list (mapcan #'(lambda (avi)
						(split-avi avi block-size)) avis))))

(defmethod crop-avi ((avi avi) &key (start 0) length)
  (subsegment-avi avi :start start :length length))

(defmethod repeat-loop-avi ((avi avi) repetitions)
  (copy-avi :default avi
	    :vid-frames (repeat-loop (vid-frames avi) repetitions)))

(defmethod repeat-avi ((avi avi) repetitions)
  (concatenate-avis (repeat (list avi) repetitions)))

(defmethod insert-avis ((grundavi avi) avi-offsets &key (overwrite nil)
			(until-end t))
  (concatenate-avis
   (let* ((grund-pos 0)
	  (cur-pos 0)
	  (res (loop for (subavi offset) in avi-offsets
		     for length = (avi-num-frames subavi)
		     when (> offset cur-pos)
		     collect (subsegment-avi grundavi :start grund-pos
					     :length (round (- offset cur-pos)))
		     when (> offset cur-pos)
		     do (incf grund-pos (round (- offset cur-pos)))
		     (incf cur-pos (round (- offset cur-pos)))

		     collect subavi
		     do (incf cur-pos length)

		     when overwrite
		     do (incf grund-pos length))))
     (if until-end
	 (append res (list (subsegment-avi grundavi :start grund-pos)))
	 res))))

(defun vid-frame (avi n)
  (elt (vid-frames avi) n))

(defun vid-frame-avi (avi n &key (len 1))
  (copy-avi :default avi
	    :vid-frames (repeat (list (vid-frame avi n)) len)))