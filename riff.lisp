(in-package :schnitt)

(define-binary-type fourcc ()
  (iso-8859-1-string :length 4))

(define-binary-type pstring ()
  (:reader (in)
    (let ((len (read-byte in)))
      (read-value 'iso-8859-1-string in :length len)))
  (:writer (out string)
    (write-byte (length string) out)
    (write-value 'iso-8859-1-string out string :length (length string))))

;; riff chunk

(define-tagged-binary-class riff-chunk ()
  ((id   fourcc)
   (size le-u4))
  (:dispatch (find-riff-class id)))

(defmethod read-value :around ((type (eql 'riff-chunk)) in &key)
  (let ((pos (file-position in))
	(obj (call-next-method))
	(final-pos (file-position in)))
    (when (not (= (- final-pos pos)
		  (+ 8 (size obj))))
      #-nil(error "RIFF chunk has been incorrectly read: ~A, ~a bytes read, should be ~a~%" obj
	     (- final-pos pos) (+ 8 (size obj))))
    obj))

(defmethod write-object :around ((chunk riff-chunk) out)
  (let* ((pos (file-position out))
	 (res (call-next-method))
	 (final-pos (file-position out))
	 (size (- final-pos pos 8)))
    (setf (size chunk) size)
    (file-position out (+ pos 4))
    (write-value 'le-u4 out size)
    (file-position out final-pos)
    res))

(defmethod print-object ((chunk riff-chunk) stream)
  (print-unreadable-object (chunk stream :type t)
    (if (slot-boundp chunk 'size)
	(format stream "~S (~A bytes)" (id chunk) (size chunk))))
	(format stream "~S" (id chunk)))

(define-binary-class generic-riff-chunk (riff-chunk)
  ((data (file-chunk :size size))))

(define-binary-class raw-riff-chunk (riff-chunk)
  ((data (raw-bytes :size size))))

(defvar *riff-dispatch* nil)

(defvar *current-chunks* nil)

(defun find-riff-class (id)
  (cond ((string-equal id "strf")
	 (let* ((current-strh (first (find-riff-chunks "strh" *current-chunks*)))
		(type (fcc-type current-strh)))
	   (cond ((string-equal type "auds")
		  'waveformatex)
		 ((string-equal type "vids")
		  'bitmapinfo)
		 (t 'riff-chunk))))
	(t (or (cadr (assoc id *riff-dispatch* :test #'string-equal))
	       'generic-riff-chunk))))

(defmacro define-riff-chunk (name id (&rest superclasses) &rest body)
  `(progn
    (define-binary-class ,name ,(or superclasses '(riff-chunk)) ,@body)
    (pushnew (list ,id ',name) *riff-dispatch* :key #'car :test #'string-equal)))

(define-binary-class riff-hdr ()
  ((id fourcc)
   (size le-u4)
   (riff-type fourcc)))

(define-binary-type riff-chunks (size)
  (:reader (in)
    (let (*current-chunks*)
      (loop with to-read = size
	    while (plusp to-read)
	    for chunk = (read-value 'riff-chunk in)
	    while chunk
	    do (decf to-read (+ 8 (size chunk)))
	    collect chunk into *current-chunks*
;	    do (format t "pos ~A chunk ~A, size ~A, to-read ~A~%"
;		       (file-position in) chunk (size chunk) to-read)
	    finally (loop repeat (1- to-read) do (read-byte in)) (return *current-chunks*))))
  (:writer (out chunks)
    (loop with to-write = size
	  for chunk in chunks
	  do (write-value 'riff-chunk out chunk)
	  (decf to-write (+ 8 (size chunk)))
	  finally (loop repeat to-write do (write-byte 0 out)))))

(define-riff-chunk riff-list "LIST" ()
  ((list-type fourcc)
   (chunks (riff-chunks :size (- (size (current-binary-object)) 8)))))

(defmethod print-object ((chunk riff-list) stream)
  (print-unreadable-object (chunk stream :type t)
    (if (slot-boundp chunk 'size)
	(format stream "~S (~A bytes, ~a chunks)" (list-type chunk) (size chunk)
		(length (chunks chunk)))
	(format stream "~S (~a chunks)" (list-type chunk) (length (chunks chunk))))))

(define-binary-class riff-file ()
  ())

(defun find-riff-lists (type chunks)
  (remove-if-not #'(lambda (chunk) (and (typep chunk 'riff-list)
			       (string-equal (list-type chunk) type)))
	chunks))

(defun find-riff-chunks (id chunks)
  (remove-if-not #'(lambda (chunk) (string-equal (id chunk) id)) chunks))

