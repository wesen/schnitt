(in-package :schnitt)


;; helpers


;; AVI file

(defclass avi ()
  ((type :initform nil
	 :accessor avi-type
	 :initarg :type)

   (vid-strh :initform nil :accessor avi-vid-strh
	     :initarg :vid-strh)
   (vid-avih :initform nil :accessor avi-vid-avih
	 :initarg :vid-avih)
   (vid-strf :initform nil :accessor avi-vid-strf
	  :initarg :vid-strf)

   (aud-strh :initform nil :accessor avi-aud-strh
	     :initarg :aud-strh)
   (aud-avih :initform nil :accessor avi-aud-avih
	     :initarg :aud-avih)
   (aud-strf :initform nil :accessor avi-aud-strf
	  :initarg :aud-strf)

   (size :initform 0
	 :accessor avi-size
	 :initarg :size)
   
   (frames :initform nil
	   :initarg :frames
	   :accessor avi-frames)
   (audio-frames :initform nil
		 :initarg :audio-frames
		 :accessor audio-frames)
   
   (pathname :initform nil
	     :accessor avi-pathname
	     :initarg :pathname)))

(defparameter +wave-format-pcm+ 1)

(defmethod avi-num-frames ((avi avi))
  (length (avi-frames avi)))

(defmethod copy-avi ((avi avi) &key (frames :noframes) strh type strf avih (class 'avi))
  (make-instance class
	 :type (or type (avi-type avi))
	 :strh (or strh (avi-strh avi))
	 :vid-strf (or strf (avi-vid-strf avi))
	 :vid-avih (or avih (avi-vid-avih avi))
	 :frames (if (eq frames :noframes)
		     (avi-frames avi)
		     frames)))

(defmethod print-object ((avi avi) s)
  (print-unreadable-object (avi s :type t :identity (unless (avi-pathname avi) t))
    (format s "\"~A\" (~A frames)" (or (avi-pathname avi)
				       "virtual avi file")
	    (avi-num-frames avi))))

(defmethod copy-frames ((avi avi))
  (copy-list (avi-frames avi)))

(defmethod concatenate-avis (avis)
  (copy-avi (first avis) :frames (mapcan #'copy-frames avis)))

(defmethod subsegment ((avi avi) &key start length random)
  (let ((len (length (avi-frames avi))))
    (if random
	(progn
	  (unless start
	    (setf start (random len)))
	  (unless length
	    (setf length (random (- len  start)))))
	(progn (unless start (setf start 0))))
    (copy-avi avi :frames (subseq (avi-frames avi) start (when length (+ start length))))))

(defmethod split-avi ((avi avi) n)
  (let ((groups (group-by (avi-frames avi) n)))
    (mapcar #'(lambda (frames)
		(copy-avi avi :frames frames)) groups)))


;; read AVI file

(defun read-avi-file (filename)
  (with-open-file (s filename :element-type '(unsigned-byte 8)
		     :direction :input)
    (let ((avi (make-instance 'avi
			      :pathname filename)))
      (read-riff-hdr avi s)
      (read-list avi s (avi-size avi))
      avi)))

(defun make-fourcc (seq)
  (coerce (mapcar #'code-char (coerce seq 'list)) 'string))

(defun make-long (seq)
  (loop for i from 0 below 4
	sum (* (elt seq i) (expt 256 i))))

(defun make-short (seq)
  (loop for i from 0 below 2
	sum (* (elt seq i) (expt 256 i))))

(defun read-chunk (avi s &optional max-size)
  (let ((chunk-arr (make-array 8 :initial-element 0 :element-type '(unsigned-byte 8))))
    (read-sequence chunk-arr s)
    (let ((type (make-fourcc (subseq chunk-arr 0 4)))
	  (size (make-long (subseq chunk-arr 4 8))))
      (when (and max-size (> size max-size))
	(error "Chunk size is incorrect"))
      (cond ((string-equal type "LIST")
	     (let ((list-arr (make-array 4 :initial-element 0 :element-type '(unsigned-byte 8))))
	       (read-sequence list-arr s)
	       (let ((list-type (make-fourcc list-arr)))
		 (cond ((string-equal list-type "movi")
			(read-movi avi s (- size 4)))
		       (t (read-list avi s (- size 4))))
		 (incf size 4))))
	    ((string-equal type "strh")
	     (read-strh avi s))
	    ((string-equal type "avih")
	     (read-avih avi s))
	    ((string-equal type "strf")
	     (read-strf avi s))
	    ((string-equal type "00dc")
	     (let ((avi-offset (file-position s)))
	       (push (list (avi-pathname avi) avi-offset size) (avi-frames avi))
	       (file-position s (+ avi-offset size))))
	    (t ; (warn "Unknown chunk ~A at offset ~A" type (file-position s))
	       (file-position s (+ (file-position s) size))))
      (+ size 8))))

(defun read-movi (avi s size)
  (read-list avi s size)
  (setf (avi-frames avi)
	(nreverse (avi-frames avi))))

(defun read-strf (avi s)
  (let ((strf (make-array 40 :initial-element 0 :element-type '(unsigned-byte 8))))
    (read-sequence strf s)
    (setf (avi-vid-strf avi) strf)
    40))

(defun read-avih (avi s)
  (let ((avih (make-array 56 :initial-element 0 :element-type '(unsigned-byte 8))))
    (read-sequence avih s)
    (setf (avi-vid-avih avi) avih)
    56))

(defun read-strh (avi s)
  (let ((strh (make-array 56 :initial-element 0 :element-type '(unsigned-byte 8))))
    (read-sequence strh s)
    (setf (avi-strh avi) strh)
    56))

(defun read-list (avi s list-size)
  (let ((cur-size 0))
    (loop while (< cur-size list-size)
	  do (incf cur-size (read-chunk avi s (- list-size cur-size))))
    list-size))

(defun read-riff-hdr (avi s)
  (let ((hdr-arr (make-array 12 :initial-element 0 :element-type '(unsigned-byte 8))))
    (read-sequence hdr-arr s)
    (unless (string-equal (make-fourcc (subseq hdr-arr 0 4)) "RIFF")
      (error "~A is not an AVI file" (avi-pathname avi)))
    (setf (avi-type avi) (make-long (subseq hdr-arr 8 12))
	  (avi-size avi) (make-long (subseq hdr-arr 4 8)))))


;; write AVI file


(defun write-fourcc (fourcc s)
  (write-sequence (mapcar #'char-code (coerce fourcc 'list)) s))


(defun long->seq (long)
  (loop for i from 0 below 4
	collect (mod (floor (/ long (expt 256 i))) 256)))

(defun write-long (long s)
  (write-sequence (long->seq long) s))

(defun write-short (short s)
  (write-sequence (loop for i from 0 below 2
			collect (mod (floor (/ short (expt 256 i))) 256)) s))

(defun write-chunk (s type data)
  (write-fourcc type s)
  (write-long (length data) s)
  (write-sequence data s)
  (+ 8 (length data)))

(defun write-list (s type chunks)
  (write-fourcc "LIST" s)
  (let ((pos (file-position s)))
    (write-long 0 s)
    (write-fourcc type s)
    (let ((length (loop for chunk in chunks
			sum (cond ((string-equal (first chunk) "LIST")
				   (write-list s (second chunk) (third chunk)))
				  ((string-equal (first chunk) "00dc")
				   (write-frame s (second chunk) (third chunk)
						(fourth chunk)))
				  (t (write-chunk s (first chunk) (second chunk)))))))
      (let ((end-pos (file-position s)))
	(file-position s pos)
;	(warn "list length is ~A" (+ 4 length))
	(write-long (+ 4 length) s)
	(file-position s end-pos))
      (+ 12 length))))

(defun write-frame (s filename offset length)
  (with-open-file (in filename :direction :input
		      :element-type '(unsigned-byte 8))
    (write-fourcc "00dc" s)
    (write-long length s)
    (file-position in offset)
    (let* ((buffer-size 65536)
	   (c (make-array buffer-size :element-type '(unsigned-byte 8))))
;      (warn "writing frame of ~a bytes~%" length)
      (loop for i from 0 below length by buffer-size
	    for l = (min buffer-size (- length i))
	    do (read-sequence c in :end l)
;	    (warn "writing ~A bytes, i ~A" l i)
	    (write-sequence c s :end l)))
    (+ length 8)))


;; XXX nil frames durch komplett schwarzes ersetzen

(defmethod write-avi-file ((avi avi) filename)
  (let ((len 12))
    (with-open-file (s filename :element-type '(unsigned-byte 8)
		       :if-does-not-exist :create
		       :if-exists :supersede
		       :direction :output)
      (write-fourcc "RIFF" s)
      (let ((pos (file-position s))
	    frames-pos)
	(write-long 0 s)
	(write-long (avi-type avi) s)
	(loop for i in (long->seq (avi-num-frames avi))
	      for j from 0
	      do (setf (aref (avi-avih avi) (+ 16 j)) i
		       (aref (avi-strh avi) (+ 32 j)) i))
	(incf len
	      (write-list s "hdrl" `(("avih" ,(avi-vid-avih avi))
				     ("LIST" "strl"
				      (("strh" ,(avi-vid-strh avi))
				       ("strf" ,(avi-vid-strf avi)))))))
	(incf len
	      (write-list s "movi" (let ((cur-pos 4))
				     (mapcar #'(lambda (data)
						 (let ((len (third data)))
						   (push (list cur-pos len) frames-pos)
						   (incf cur-pos (+ 8 len)))
						 `("00dc" ,@data))
					     (avi-frames avi)))))
	(setf frames-pos (nreverse frames-pos))

	;; XXX idx1
	(incf len
	      (write-chunk s "idx1"
			   (coerce (mapcan #'(lambda (pos-len)
					       (append '(#x30 #x30 #x64 #x63) ; 00dc RIFF header
						       '(#x10 #x00 #x00 #x00) ; FLAGS
						       (long->seq (first pos-len))
						       (long->seq (second pos-len))))
					   frames-pos) 'array)))
	
	
	
	(file-position s pos)
;	(warn "len is ~A" len)
	(write-long len s))
      )))

(defun write-avi-files (avis pathname)
  (loop for avi in avis
	for i from 0
	for out = (make-pathname :defaults pathname
				      :name (format nil "~A-~A"
						    (pathname-name pathname) i))
	do (format t "pathname: ~A~%" out)
	do (write-avi-file avi out)
	collect out))

;; avi manipulation functions, always non-destructive

(defmethod reverse-avi ((avi avi))
  (copy-avi avi :frames (reverse (avi-frames avi))))

(defmethod permutate-avi ((avi avi) n)
  (concatenate-avis (permutate-list (split-avi avi n))))

(defmethod permutate-avi-to-segments ((avi avi) n)
  (permutate-list (split-avi avi n)))

(defmethod permutate-avis (avis n)
  (concatenate-avis (permutate-list (mapcan #'(lambda (avi) (split-avi avi n)) avis))))

(defmethod permutate-avis-to-segments (avis n)
  (permutate-list (mapcan #'(lambda (avi) (split-avi avi n)) avis)))

(defmethod crop-avi ((avi avi) &key (start 0) length)
  (subsegment avi :start start :length length))

(defun flimmern (groups n)
  (loop for group1-2 on groups by #'cddr
	for group1 = (car group1-2)
	for group2 = (cadr group1-2)
	appending (if (and group1 group2)
		      (list group1
			    (repeat (list (first group2)
					  (car (last group1))) n)
			    group2)
		      (list group1))))

(defmethod flimmer-avis (avis n)
  (copy-avi (first avis) :frames (flatten1  (flimmern (mapcar #'avi-frames avis) n))))

(defmethod repeat-loop-avi ((avi avi) n)
  (copy-avi avi :frames (repeat-loop (avi-frames avi) n)))

(defmethod repeat-avi ((avi avi) n)
  (concatenate-avis (repeat (list avi) n)))

(defmethod insert-avis ((grundavi avi) avi-offsets &key (insert t) (until-end t))
  (concatenate-avis
   (let* ((grund-pos 0)
	  (cur-pos 0)
	  (res
	   (loop for (subavi offset) in avi-offsets
		 for length = (avi-num-frames subavi)
;		 do (format t "grund-pos ~A, offset ~A cur-pos ~A length ~A~%" grund-pos offset cur-pos length)
		 when (> offset cur-pos)
		 collect (subsegment grundavi :start grund-pos :length (round (- offset cur-pos)))
		 when (> offset cur-pos)
		 do (incf grund-pos (round (- offset cur-pos)))
		 (incf cur-pos (round (- offset cur-pos)))

		 collect subavi
		 do (incf cur-pos length)
		 (when (not insert)
		   (incf grund-pos length)))))
     (if until-end
	 (append res (list (subsegment grundavi :start grund-pos)))
	 res))))

(defmethod insert-random-avis (grundavi avis &key (insert t) (until-end t))
  (let ((len (avi-num-frames grundavi)))
    (insert-avis grundavi (sort (mapcar #'(lambda (avi) (list avi (random len))) avis) #'<
				:key #'second)
		 :insert insert :until-end until-end)))

(defmethod split-avi-no-shorter ((avi avi) length)
  (remove length (split-avi avi length) :test #'> :key #'avi-num-frames))

(defmethod insert-flashes ((avi avi) long-length permutate-length flash-length flash-num)
  (let* ((long-segments (split-avi-no-shorter avi long-length))
	 (short-segments (mapcar #'(lambda (avi)
				     (subseq
				      (split-avi (permutate-avi avi permutate-length) flash-length)
				      0 flash-num)) long-segments)))
    (concatenate-avis (mapcar #'insert-random-avis long-segments short-segments))))

(defun interleave (lists)
  (loop for i from 0 below (length (first lists))
	appending (mapcar #'(lambda (x) (elt x i)) lists)))

(defmethod interleave-avis (avis n)
  (concatenate-avis (interleave (mapcar #'(lambda (avi) (split-avi avi n)) avis))))

(defmethod repeat-frames-avi ((avi avi) size p &key (n 1))
  (copy-avi avi :frames (repeat-block (avi-frames avi) size p :n n)))

(defmethod skip-frames-avi ((avi avi) p &key (n 1))
  (copy-avi avi :frames (skip-elt (avi-frames avi) p :n n)))

;; tools

(defmethod zerleg-avi-file-in-kleine (infile outmask long short)
  (let* ((avi (read-avi-file infile))
	 (long-segments (mapcar #'(lambda (avi) (permutate-avi avi short))
				(split-avi avi long))))
    (write-avi-files long-segments outmask)))


(defmethod uebergang ((avi1 avi) (avi2 avi) blocklen &key (repeat 1))
  (let* ((avi1s (split-avi-no-shorter avi1 blocklen))
	 (avi2s (split-avi-no-shorter avi2 blocklen))
	 (len2s (subseq (repeat-loop (gen-list blocklen :start 0 :repeat repeat)
				     (/ (avi-num-frames avi1) blocklen))
			0 (length avi1s)))
	 (last (subsegment avi1 :start (reduce #'+ avi1s :initial-value 0
					       :key #'avi-num-frames))))
    (concatenate-avis (append
		       (loop for seg1 in avi1s
			     for seg2 in avi2s
			     for len2 in len2s
			     for len1 = (round (/ (- blocklen len2) 2))
;			     do (format t "~A ~A ~A~%" len1 len2 len1)
			     collect (subsegment seg1 :length len1)
			     collect (subsegment seg2 :start len1 :length len2)
			     collect (subsegment seg1 :start (+ len1 len2)))
		       (list last)))))
;; music

(defun bpm->frames (bpm)
  (/ (* 60 +fps+) bpm))

(defconstant +fps+ 25)

(defparameter *bpm* 144.0)

(defun frame-length (frames)
  (1+ (/ frames (bpm->frames *bpm*))))

(defun beat-length (beat)
  (* (1- beat) (bpm->frames *bpm*)))

(defun beat->frame (beats)
  (mapcar #'beat-length beats))

(defun beat-offsets->avi-offsets (beat-offsets)
  (mapcar #'(lambda (beat-offset)
	      (list (first beat-offset)
		    (beat-length (second beat-offset))))
	  beat-offsets))

