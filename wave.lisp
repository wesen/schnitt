(in-package :schnitt)

(define-riff-chunk wave-fmt-chunk "fmt " ()
  ((format-tag        le-u2)
   (channels          le-u2)
   (samples-per-sec   le-u4)
   (avg-bytes-per-sec le-u4)
   (block-align       le-u2)
   (bits-per-sample   le-u2)))

(define-binary-class wave-file (riff-file)
  ((hdr riff-hdr)
   (chunks (riff-chunks :size (- (size hdr) 4)))))

(defun read-wave-file (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (read-value 'wave-file in)))

(defmethod write-wave-file ((wave wave-file) file)
  (with-open-file (out file :direction :output :if-exists :supersede
		       :element-type '(unsigned-byte 8))
    (write-object wave out)))

(defmethod wave-data ((wave wave-file))
  (let ((data-chunk (first (find-riff-chunks "data" (chunks wave)))))
    (when data-chunk
      (data data-chunk))))

(defmethod wave-fmt ((wave wave-file))
  (first (find-riff-chunks "fmt " (chunks wave))))

(defmethod time-to-offset ((wave wave-file) time)
  (let* ((fmt (wave-fmt wave))
	 (samples (samples-per-sec fmt))
	 (blocksize (block-align fmt)))
    (* blocksize (round (* time samples blocksize) blocksize))))

(defmethod wave-file-slice-offset ((wave wave-file) &key (start 0) length)
  (copy-wave :default wave :data (file-chunk-slice (wave-data wave) :start start :length length)))

(defmethod wave-file-slice-time ((wave wave-file) &key (start 0.0) length)
  (wave-file-slice-offset wave
			  :start (time-to-offset wave start)
			  :length (time-to-offset wave length)))
			  
(defun copy-wave (&key default data fmt)
  (when default
    (unless data
      (setf data (wave-data default)))
    (unless fmt
      (setf fmt (wave-fmt default))))
  (make-instance 'wave-file
		 :hdr (make-instance 'riff-hdr :id "RIFF" :size 0 :riff-type "WAVE")
		 :chunks (list fmt (make-instance 'generic-riff-chunk :id "data" :size 0
						  :data data))))

(defmethod wave-duration ((wave wave-file))
  (let* ((fmt (wave-fmt wave))
	 (block-align (block-align fmt))
	 (samples-per-sec (samples-per-sec fmt))
	 (data (wave-data wave)))
    (/ (size data) (* block-align samples-per-sec))))
			  
;; sequence waves XXX

(defmethod write-object :around ((wave riff-file) out)
  (let ((pos (file-position out))
	(res (call-next-method))
	(final-pos (file-position out)))
    (file-position out (+ pos 4))
    (write-value 'le-u4 out (- final-pos pos 8))
    (file-position out final-pos)
    res))

(defun concat-waves (waves)
  (copy-wave :default (first waves)
	     :data (make-instance 'multi-file-chunk
				  :chunks (mapcar #'wave-data waves))))

(defmethod split-wave ((wave wave-file) offsets)
  (loop for (time duration) in offsets
	collect (wave-file-slice-time wave :start time :length duration)))


(defmethod reverse-wave ((wave wave-file))
  (let ((data (wave-data wave))
	(fmt (wave-fmt wave)))
    (copy-wave :default wave
	       :data (make-instance 'reverse-file-chunk :filename (filename data)
				    :position (chunk-position data)
				    :size (size data)
				    :elm-size (block-align fmt)))))