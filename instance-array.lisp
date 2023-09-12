(in-package :krma)

(defstruct (instance-array
	     (:include foreign-adjustable-array)
	     (:constructor make-instance-array
			   (&optional
			    (allocated-count +draw-list-alloc-size+)
			    (foreign-type :unsigned-int)
			    (foreign-type-size (foreign-type-size foreign-type))
			    (fill-pointer 0)
			    (bytes (make-array (* (ash foreign-type-size -2)
						  (cl:the fixnum allocated-count))
					       :element-type '(unsigned-byte 32)))))))

(defcstruct 2d-vertex
  (oid :unsigned-int)
  (col :unsigned-int)
  (x :float)
  (y :float))

(defun (setf 2d-vertex-oid) (value bytes offset)
  (declare (type (unsigned-byte 32) value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct 2d-vertex)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct 2d-vertex) 'oid) -2)))))
	value))


(defun (setf 2d-vertex-col) (value bytes offset)
  (declare (type (unsigned-byte 32) value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct 2d-vertex)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct 2d-vertex) 'col) -2)))))
	value))

(defun (setf 2d-vertex-x) (value bytes offset)
  (declare (type single-float value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct 2d-vertex)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct 2d-vertex) 'x) -2)))))
	(float-features:single-float-bits value)))


(defun (setf 2d-vertex-y) (value bytes offset)
  (declare (type single-float value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct 2d-vertex)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct 2d-vertex) 'y) -2)))))
	(float-features:single-float-bits value)))

(defcstruct 3d-vertex
  (oid :unsigned-int)
  (col :unsigned-int)
  (x :float)
  (y :float)
  (z :float))

(defun (setf 3d-vertex-oid) (value bytes offset)
  (declare (type (unsigned-byte 32) value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct 3d-vertex)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct 3d-vertex) 'oid) -2)))))
	value))


(defun (setf 3d-vertex-col) (value bytes offset)
  (declare (type (unsigned-byte 32) value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct 3d-vertex)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct 3d-vertex) 'col) -2)))))
	value))

(defun (setf 3d-vertex-x) (value bytes offset)
  (declare (type single-float value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct 3d-vertex)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct 3d-vertex) 'x) -2)))))
	(float-features:single-float-bits value)))


(defun (setf 3d-vertex-y) (value bytes offset)
  (declare (type single-float value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct 3d-vertex)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct 3d-vertex) 'y) -2)))))
	(float-features:single-float-bits value)))

(defun (setf 3d-vertex-z) (value bytes offset)
  (declare (type single-float value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct 3d-vertex)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct 3d-vertex) 'z) -2)))))
	(float-features:single-float-bits value)))

(defstruct (2d-vertex-instance-array
            (:include instance-array)
            (:constructor make-2d-vertex-instance-array
		(&optional
		   (allocated-count +draw-list-alloc-size+)
		   (foreign-type '(:struct 2d-vertex))
		   (foreign-type-size (foreign-type-size foreign-type))
		   (fill-pointer 0)
		   (bytes (make-array (* (cl:the (integer 0 #.(ash most-positive-fixnum -9)) allocated-count)  (ash (cl:the (integer 0 512) foreign-type-size) -2)) :element-type '(unsigned-byte 32)))))))

(defun 2d-vertex-instance-array-push-extend (2d-vertex-instance-array ub32-oid sf-x sf-y ub32-color)
  "Function for adding 2d-vertex to a 2d-vertex-instance-array."
  (declare (type 2d-vertex-instance-array 2d-vertex-instance-array))
  (declare (type single-float sf-x sf-y))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (let ((vertex-type-size-uint (load-time-value (ash (foreign-type-size '(:struct 2d-vertex)) -2)))
	(vertex-type-size (load-time-value (foreign-type-size '(:struct 2d-vertex)))))
    (declare (type (integer 0 512) vertex-type-size-uint vertex-type-size))
    (with-slots (bytes fill-pointer allocated-count) 2d-vertex-instance-array
      (let ((fp fill-pointer)
	    (alloc-count allocated-count))
	(declare (type fixnum fp))
	(declare (type (integer 0 #.(ash most-positive-fixnum -10)) alloc-count))
	(unless (< fp alloc-count)
	  (let ((new-count (* 2 alloc-count)))
	    (declare (type fixnum new-count))
	    (let ((new-array (make-array (* new-count vertex-type-size-uint) :element-type '(unsigned-byte 32)))
		  (old-array bytes))
	      #+SBCL(sb-sys:with-pinned-objects (new-array old-array)
		(memcpy (sb-sys:vector-sap new-array)
			(sb-sys:vector-sap old-array)
			(* (cl:the (integer 0 #.(ash most-positive-fixnum -9)) fill-pointer)
			   (cl:the (integer 0 512) vertex-type-size))))
	      #+CCL
	      (ccl::%copy-ivector-to-ivector old-array 0 new-array 0
					     (* (cl:the (integer 0 #.(ash most-positive-fixnum -9)) fill-pointer)
						(cl:the (integer 0 512) vertex-type-size)))
	      (setf bytes new-array)
	      (setf allocated-count new-count))))
	;; setf foreign-slot-value has got to be slow
	;; especially when it can't reason about the type at compile time.
	(setf (2d-vertex-oid bytes fp) ub32-oid
	      (2d-vertex-col bytes fp) ub32-color
	      (2d-vertex-x bytes fp) sf-x
              (2d-vertex-y bytes fp) sf-y)
        (prog1 fp
          (incf fill-pointer))))))

(defstruct (3d-vertex-instance-array
            (:include instance-array)
            (:constructor make-3d-vertex-instance-array
		(&optional
		   (allocated-count +draw-list-alloc-size+)
		   (foreign-type '(:struct 3d-vertex))
		   (foreign-type-size (foreign-type-size foreign-type))
		   (fill-pointer 0)
		   (bytes (make-array (* (cl:the (integer 0 #.(ash most-positive-fixnum -9)) allocated-count)  (ash (cl:the (integer 0 512) foreign-type-size) -2)) :element-type '(unsigned-byte 32)))))))

(defun 3d-vertex-instance-array-push-extend (3d-vertex-instance-array ub32-oid sf-x sf-y sf-z ub32-color)
  "Function for adding 3d-vertex to a 3d-vertex-instance-array."
  (declare (type 3d-vertex-instance-array 3d-vertex-instance-array))
  (declare (type single-float sf-x sf-y sf-z))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (let ((vertex-type-size-uint (load-time-value (ash (foreign-type-size '(:struct 3d-vertex)) -2)))
	(vertex-type-size (load-time-value (foreign-type-size '(:struct 3d-vertex)))))
    (declare (type (integer 0 512) vertex-type-size vertex-type-size-uint))
    (with-slots (bytes fill-pointer allocated-count) 3d-vertex-instance-array
      (let ((fp fill-pointer)
	    (alloc-count allocated-count))
	(declare (type fixnum fp))
	(declare (type (integer 0 #.(ash most-positive-fixnum -10)) alloc-count))
	(unless (< fp alloc-count)
	  (let ((new-count (* 2 alloc-count)))
	    (declare (type fixnum new-count))
	    (let ((new-array (make-array (* new-count vertex-type-size-uint) :element-type '(unsigned-byte 32)))
		  (old-array bytes))
	      #+sbcl(sb-sys:with-pinned-objects (new-array old-array)
		(memcpy (sb-sys:vector-sap new-array)
			(sb-sys:vector-sap old-array)
			(* (cl:the (integer 0 #.(ash most-positive-fixnum -9)) fill-pointer)
			   (cl:the (integer 0 512) vertex-type-size))))
	      #+CCL
	      (ccl::%copy-ivector-to-ivector old-array 0 new-array 0
					     (* (cl:the (integer 0 #.(ash most-positive-fixnum -9)) fill-pointer)
						(cl:the (integer 0 512) vertex-type-size)))
	      (setf bytes new-array)
	      (setf allocated-count new-count))))
	(setf (3d-vertex-oid bytes fp) ub32-oid
	      (3d-vertex-col bytes fp) ub32-color
	      (3d-vertex-x bytes fp) sf-x
              (3d-vertex-y bytes fp) sf-y
	      (3d-vertex-z bytes fp) sf-z)
        (prog1 fp
          (incf fill-pointer))))))

(defclass instance-list-mixin ()
  ((array
    :accessor instance-list-array
    :initarg :array)
   (memory
    :accessor instance-list-memory
    :initform nil
    :initarg :memory)
   (size-aligned
    :accessor instance-list-size-aligned
    :initform nil)))

(defclass polyline-instance-list-mixin (instance-list-mixin)
  ())

(defmethod instance-list-count ((instance-list polyline-instance-list-mixin))
  (1- (foreign-array-fill-pointer (instance-list-array instance-list))))

(defclass 2d-polyline-instance-list (polyline-instance-list-mixin)
  ((array :initform (make-2d-vertex-instance-array))))

(defclass 3d-polyline-instance-list (polyline-instance-list-mixin)
  ((array :initform (make-3d-vertex-instance-array))))

(defun initialize-instance-list-buffer (dpy instance-list)
  (let ((array (instance-list-array instance-list)))

    (let ((array-size (* (foreign-array-fill-pointer array)
			 (foreign-array-foreign-type-size array))))

      (flet ((mmap-buffer (buffer lisp-array size memory-resource aligned-size)
	       (unless (zerop size)
		 (let ((memory (allocated-memory buffer))
                       (offset (vk::memory-resource-offset memory-resource))
                       (device (vk::device buffer)))
                   (with-foreign-object (pp-dst :pointer)

                     (check-vk-result (vkMapMemory (h device) (h memory) offset aligned-size 0 pp-dst))

		     (unwind-protect
			  (let ((p-dst (mem-aref pp-dst :pointer)))
			    #+sbcl
			    (sb-sys:with-pinned-objects (lisp-array)
			      (vk::memcpy p-dst (sb-sys:vector-sap lisp-array) size))
			    #+ccl
			    (ccl::%copy-ivector-to-ptr lisp-array 0 p-dst 0 size)
			    
			    (with-foreign-object (p-range '(:struct VkMappedMemoryRange))
			      (zero-struct p-range '(:struct VkMappedMemoryRange))
			      
			      (with-foreign-slots ((%vk::sType
						    %vk::memory
						    %vk::size
						    %vk::offset)
						   p-range (:struct VkMappedMemoryRange))
				
				(setf %vk::sType VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
				      %vk::memory (h memory)
				      %vk::size aligned-size
				      %vk::offset offset))
			      
			      (check-vk-result (vkFlushMappedMemoryRanges (h device) 1 p-range))))
			      
		       (vkUnmapMemory (h device) (h memory)))

		     (values))))))

	(let ((new-size-aligned (vk::aligned-size array-size)))

	  (unless (instance-list-size-aligned instance-list)
	    (setf (instance-list-memory instance-list)
		  (vk::acquire-memory-sized (vk::memory-pool dpy) new-size-aligned :host-visible))
	    (setf (instance-list-size-aligned instance-list) new-size-aligned))

        (unless (zerop array-size)
          (let* ((old-size-aligned (instance-list-size-aligned instance-list))
                 (memory-resource))
	    
            (setq memory-resource
		  (if (> new-size-aligned old-size-aligned)
		      (if (instance-list-memory instance-list)
			  (progn (vk::release-memory-resource dpy (instance-list-memory instance-list))
				 (setf (instance-list-memory instance-list)
				       (vk::acquire-memory-sized dpy new-size-aligned :host-visible)))
			  (setf (instance-list-memory instance-list)
				(vk::acquire-memory-sized dpy new-size-aligned :host-visible)))
		      (if (instance-list-memory instance-list)
			  (instance-list-memory instance-list)
			  (setf (instance-list-memory instance-list)
				(vk::acquire-memory-sized dpy new-size-aligned :host-visible)))))
	    
	    (setf (instance-list-size-aligned instance-list) new-size-aligned)
	    
            (mmap-buffer (vk::memory-resource-buffer memory-resource)
                         (foreign-array-bytes array) array-size memory-resource
                         new-size-aligned)))))))
  
  (values))
