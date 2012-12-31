(in-package :schnitt)

;; file chunks

(defclass file-chunk ()
  ((filename       :initarg :filename
		   :reader filename)
   (chunk-position :initarg :position
		   :reader chunk-position)
   (size           :initarg :size
		   :reader size)))

(defmethod print-object ((chunk file-chunk) stream)
  (print-unreadable-object (chunk stream :type t :identity t)
    (with-slots (filename chunk-position size) chunk
      (format stream "~A.~A (~A - ~A)"
	      (pathname-name filename)
	      (pathname-type filename)
	      chunk-position (+ size chunk-position)))))

(defmethod file-chunk-slice ((chunk file-chunk) &key (start 0) length)
  (let* ((wstart (chunk-position chunk))
	 (wlength (size chunk))
	 (wend (+ wlength wstart)))
    (when (> start wlength)
      (error "start has to be between ~A and ~A" wstart wlength))
    (unless length
      (setf length (- wend start)))
    (when (> length (- wend start))
      (error "length has to be inferior to ~A" (- wend start)))
    (make-instance 'file-chunk :filename (filename chunk)
		   :position (+ wstart start)
		   :size length)))

(defclass multi-file-chunk ()
  ((chunks :initarg :chunks :accessor chunks)))

(defmethod read-value ((type (eql 'file-chunk)) in &key size)
  (let* ((pos (file-position in))
	 (res (make-instance 'file-chunk :filename (pathname in)
			     :position pos
			     :size size)))
    (file-position in (+ pos size))
    res))

(defmethod write-value ((type (eql 'file-chunk)) out value &key size)
  (declare (ignore size))
  (write-object value out))

(defmethod write-object progn ((chunk file-chunk) out)
  (with-open-file (in (filename chunk) :direction :input
		      :element-type '(unsigned-byte 8))
    (file-position in (chunk-position chunk))
    (let* ((buffer-size 65536)
	   (c (make-array buffer-size :element-type '(unsigned-byte 8))))
      (loop with length = (size chunk)
	    for i from 0 below length by buffer-size
	    for l = (min buffer-size (- length i))
	    do (read-sequence c in :end l)
	    (write-sequence c out :end l)))))

(defmethod write-object progn  ((multichunk multi-file-chunk) out)
  (dolist (chunk (chunks multichunk))
    (write-object chunk out)))


(defclass reverse-file-chunk ()
  ((filename       :initarg :filename
		   :reader filename)
   (chunk-position :initarg :position
		   :reader chunk-position)
   (size           :initarg :size
		   :reader size)
   (elm-size :initarg :elm-size :initform 1 :reader elm-size)))

(defun reverse-sequence (seq &key (end (length seq)) (elm-size 1))
  (let ((res (make-array end :element-type (array-element-type seq)))
	(start (- end elm-size)))
    (loop for cur from (- end elm-size) downto 0 by elm-size
	  do (dotimes (i elm-size)
	       (setf (aref res (+ i (- start cur)))
		     (aref seq (+ i cur)))))
    res))


(defmethod write-value ((type (eql 'reverse-file-chunk)) out value &key size)
  (declare (ignore size))
  (write-object value out))

(defmethod write-object progn ((chunk reverse-file-chunk) out)
   (with-open-file (in (filename chunk) :direction :input
		       :element-type '(unsigned-byte 8))
     (let* ((cur-end (+ (chunk-position chunk)
			(size chunk)))
	    (elm-size (elm-size chunk))
	    (buffer-size (* elm-size (round 65536 elm-size)))
	    (buffer (make-array buffer-size :element-type '(unsigned-byte 8))))
       (loop with length = (size chunk)
	     for end from (1- length) downto buffer-size by buffer-size
	     for start = (max 0 (- end buffer-size))
	     do 
	     (file-position in start)
	     (read-sequence buffer in :end (- end start))
	     (let ((rev (reverse-sequence buffer :end (- end start) :elm-size elm-size)))
	       (write-sequence rev out :end (- end start)))))))
  