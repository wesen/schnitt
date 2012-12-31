(in-package :schnitt)

(defun as-keyword (sym)
  (intern (string sym) :keyword))

(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defun mklist (x) (if (listp x) x (list x)))

(defun normalize-slot-spec (spec)
  (list (first spec) (mklist (second spec))))

(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-gensyms (symbols &rest body)
    `(let ,(mapcar #'(lambda (sym) `(,sym (gensym))) symbols)
      ,@body)))
  
(defgeneric read-value (type stream &key)
  (:documentation "Read a value of the given type from the stream."))

(defgeneric write-value (type stream value &key)
  (:documentation "Write a value as the given type to the stream."))

(defgeneric read-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Read a value of the given type from the stream."))

(defgeneric write-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "Write a value as the given type to the stream."))


(defun slot->write-value (spec stream)
  (destructuring-bind (name (type &rest args))
      (normalize-slot-spec spec)
    `(write-value ',type ,stream ,name ,@args)))

(defun slot->copy-slot (name)
  (list (as-keyword name) name))

(defun direct-slots (name)
  (copy-list (get name 'slots)))

(defun inherited-slots (name)
  (loop for super in (get name 'superclasses)
	nconc (direct-slots super)
	nconc (inherited-slots super)))

(defun all-slots (name)
  (nconc (direct-slots name)
	 (inherited-slots name)))

(defun new-class-all-slots (slots superclasses)
  (nconc (mapcan #'all-slots superclasses)
	 (mapcar #'first slots)))

(defgeneric copy-object (object))

(defmethod copy-object ((list list))
  (copy-list list))

(defmacro define-generic-binary-class (name (&rest superclasses) slots read-method)
  (with-gensyms (objectvar streamvar)
    `(progn
      (eval-when (:compile-toplevel :load-toplevel :execute)
	(setf (get ',name 'slots) ',(mapcar #'first slots))
	(setf (get ',name 'superclasses) ',superclasses))

      (defclass ,name ,superclasses
	,(mapcar #'slot->defclass-slot slots))

      (defmethod copy-object ((,objectvar ,name))
	(with-slots ,(new-class-all-slots slots superclasses) ,objectvar
	  (make-instance ',name
			 ,@(mapcan #'slot->copy-slot (new-class-all-slots slots superclasses)))))

      ,read-method

      (defmethod write-object progn ((,objectvar ,name) ,streamvar)
	 (declare (ignorable ,streamvar))
	 (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
	   ,@(mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))

(defmacro define-binary-class (name (&rest superclasses) slots)
  (with-gensyms (objectvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
      (defmethod read-object progn ((,objectvar ,name) ,streamvar)
	 (declare (ignorable ,streamvar))
	 (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
	   ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))))))

(defun slot->binding (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(,name (read-value ',type ,stream ,@args))))

(defun slot->keyword-arg (spec)
  (let ((name (first spec)))
    `(,(as-keyword name) ,name)))

(defmacro define-tagged-binary-class (name (&rest superclasses) slots &rest options)
  (with-gensyms (typevar objectvar streamvar)
    `(define-generic-binary-class ,name ,superclasses ,slots
      (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
	(let* ,(mapcar #'(lambda (x) (slot->binding x streamvar)) slots)
	  (let ((,objectvar (make-instance
			     ,@(or (cdr (assoc :dispatch options))
				   (error "Must supply :dispatch form."))
			     ,@(mapcan #'slot->keyword-arg slots))))
	    (read-object ,objectvar ,streamvar)
	    ,objectvar))))))

(defmacro define-binary-type (name (&rest args) &body spec)
  (ecase (length spec)
    (1
     (with-gensyms (type stream value)
       (destructuring-bind (derived-from &rest derived-args) (mklist (first spec))
	 `(progn
	   (defmethod read-value ((,type (eql ',name)) ,stream &key ,@args)
	     (read-value ',derived-from ,stream ,@derived-args))
	   (defmethod write-value ((,type (eql ',name)) ,stream ,value &key ,@args)
	     (write-value ',derived-from ,stream ,value ,@derived-args))))))
    (2
     (with-gensyms (type)
       `(progn
         ,(destructuring-bind ((in) &body body) (rest (assoc :reader spec))
	   `(defmethod read-value ((,type (eql ',name)) ,in &key ,@args)
	     ,@body))
	 ,(destructuring-bind ((out value) &body body) (rest (assoc :writer spec))
           `(defmethod write-value ((,type (eql ',name)) ,out ,value &key ,@args)
	     ,@body)))))))

(defmethod read-value ((type symbol) stream &key)
  (let ((object (make-instance type)))
    (read-object object stream)
    object))

(defmethod write-value ((type symbol) stream value &key)
  (assert (typep value type))
  (write-object value stream))

(defvar *in-progress-objects* nil)

(defmethod read-object :around (object stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))

(defmethod write-object :around (object stream)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))

(defun current-binary-object () (first *in-progress-objects*))

(defun parent-of-type (type)
  (find-if #'(lambda (x) (typep x type)) *in-progress-objects*))

