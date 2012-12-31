(in-package :schnitt)

(defun group-by (list n)
  (do ((i 0 (1+ i))
       (l list (cdr l))
       group res)
      ((null l)
       (when group (push (nreverse group) res))
       (nreverse res))
    (push (first l) group)
    (when (= (length group) n)
      (push (nreverse group) res)
      (setf group nil))))

(defun random-elt (list)
  (elt list (random (length list))))

(defun flatten1 (list)
  (let (res)
    (dolist (l list)
      (if (listp l)
	  (dolist (l2 l)
	    (push l2 res))
	  (push l res)))
    (nreverse res)))

(defun permutate-list (list)
  (let* ((arr (coerce list 'array))
	 (length (length arr)))
    (dotimes (i length)
      (let* ((i1 (random length))
	     (i2 (random length))
	     (tmp (aref arr i1)))
	(setf (aref arr i1) (aref arr i2)
	      (aref arr i2) tmp)))
    (coerce arr 'list)))

(defun repeat (list n)
  (loop repeat n
	appending list))

(defun repeat-elt (list p &key (n 1))
  (loop for i in list
	collect i
	when (< (random 1.0) p)
	appending (make-list n :initial-element i)))

(defun skip-elt (list p &key (n 1))
  (do ((l list)
       res)
      ((null l) (nreverse res))
    (if (< (random 1.0) p)
	(setf l (nthcdr n l))
	(progn
	  (push (car l) res)
	  (setf l (cdr l))))))

(defun repeat-block (list size p &key (n 1))
  (do ((l list)
       res)
      ((null l) (nreverse res))
    (if (< (random 1.0) p)
	(let ((block (first-elts l size)))
	  (dolist (b block)
	    (dotimes (i (1+ n))
	      (push b res)))
	  (setf l (nthcdr size l)))
	(progn
	  (push (car l) res)
	  (setf l (cdr l))))))

(defun repeat-loop (list n)
  (let ((reverse (cdr (reverse (cdr list)))))
    (loop repeat n
	  appending list
	  appending reverse)))

(defun gen-list (len &key (start 0) (step 1) (repeat 1))
  (let (res)
    (loop for i from start below (+ start len) by step
	   do (dotimes (x repeat)
		(push i res)))
    (nreverse res)))
  
(defun first-elts (list num)
  (loop for i from 0 below num
	for x in list
	collect x))

