(in-package :krma)

(declaim (optimize (speed 3) (safety 0) (debug 3))
	 (sb-ext:muffle-conditions sb-ext:compiler-note))

(defstruct (foreign-adjustable-array
	    (:conc-name "FOREIGN-ARRAY-"))
  (bytes nil :type (or simple-array null))
  (fill-pointer 0 :type fixnum)
  (allocated-count 0 :type fixnum)
  (foreign-type nil)
  (foreign-type-size 1 :type (integer 0 512))) ;; foreign-type-size can't be more than 512

(declaim (type sb-sys:system-area-pointer foreign-array-ptr)
	 (inline foreign-array-ptr))
(defun foreign-array-ptr (foreign-adjustable-array)
  (declare (type foreign-adjustable-array foreign-adjustable-array))
  (sb-sys:vector-sap (foreign-array-bytes foreign-adjustable-array)))

(defstruct (index-array
	    (:include foreign-adjustable-array)
	    (:constructor make-index-array
		(&optional
		   (allocated-count +draw-list-alloc-size+)
		   (foreign-type :unsigned-short)
		   (foreign-type-size (foreign-type-size foreign-type))
		   (fill-pointer 0)
		   (bytes (make-array (cl:the fixnum allocated-count) :element-type '(unsigned-byte 16)))))))

(declaim (inline index-array-push-extend))
(defun index-array-push-extend (index-array index)
  (declare (type index-array index-array))
  (declare (type (unsigned-byte 32) index))
  (block push-extend
    (let ((err nil))
      (flet ((upgrade-index-array! (c)
               (when err
		 (signal c))
               (with-slots (bytes
                            allocated-count
                            fill-pointer
                            foreign-type
                            foreign-type-size)
                   index-array
		 (let ((new-arr (make-array allocated-count :element-type '(unsigned-byte 32)))
		       (old-arr bytes))
		   (declare (type (simple-array (unsigned-byte 16)) old-arr))
                   (loop for i from 0 below fill-pointer ;; copy the indexes
			 do (setf (aref new-arr i) (aref old-arr i)))
                   (setf foreign-type :unsigned-int
			 foreign-type-size (load-time-value (foreign-type-size :unsigned-int))
			 bytes new-arr
			 err t)
                   (values)))))
	
	(with-slots (bytes
                     allocated-count
                     fill-pointer
                     foreign-type
                     foreign-type-size)
            index-array
	  
	  (let ((fp fill-pointer)
		(alloc-count allocated-count)
		(ftype foreign-type))
	    (declare (type fixnum fp))
	    (declare (type (integer 0 #.(ash most-positive-fixnum -3)) alloc-count))
	    (unless (< fp alloc-count)
	      (let* ((new-count (* 2 alloc-count)))
		(declare (type fixnum new-count))
		(let ((new-array
			(ecase ftype
			  (:unsigned-short (make-array new-count :element-type '(unsigned-byte 16)))
			  (:unsigned-int (make-array new-count :element-type '(unsigned-byte 32)))))
		      (old-array bytes))
		  (sb-sys:with-pinned-objects (new-array old-array)
		    (memcpy (sb-sys:vector-sap new-array)
			    (sb-sys:vector-sap old-array)
			    (* (cl:the (integer 0 #.(ash most-positive-fixnum -2)) fill-pointer)
			       (cl:the (integer 0 4) foreign-type-size))))
		  (setf bytes new-array)
		  (setf allocated-count new-count))))

	       (tagbody
		retry
		  (handler-case

		      (ecase ftype
			(:unsigned-short (setf (aref (cl:the (simple-array (unsigned-byte 16)) bytes) fp) index))
			(:unsigned-int (setf (aref (cl:the (simple-array (unsigned-byte 32)) bytes) fp) index)))
		    
		    (type-error (c)
		      (format t "~&upgrading index array from unsigned-short to unsigned-int.")
		      (finish-output)
		      (upgrade-index-array! c)
		      (go retry)))
	       
		  (return-from push-extend
		    (prog1 fp
		      (incf fill-pointer))))))))))

(defstruct (vertex-array (:include foreign-adjustable-array)))

(defcstruct standard-vertex
  (oid :unsigned-int)
  (col :unsigned-int))

;; we use a textured vertex for non-textured (standard) use to be able to use the same shader and draw-lists for simplicity
;; we simply put the tex coordinates of a white pixel and blend as if we were drawing text
(defcstruct textured-2d-vertex
  (oid :unsigned-int)
  (col :unsigned-int)
  (x :float)
  (y :float)
  (u :float)
  (v :float))

(declaim (inline (setf standard-vertex-oid)))
(defun (setf standard-vertex-oid) (value bytes offset)
  (declare (type (unsigned-byte 32) value))
  (declare (type fixnum offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (ash (+ offset (cl:the fixnum (load-time-value (foreign-slot-offset '(:struct standard-vertex) 'oid)))) -2))
	value))

(declaim (inline (setf standard-vertex-col)))
(defun (setf standard-vertex-col) (value bytes offset)
  (declare (type (unsigned-byte 32) value))
  (declare (type fixnum offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (ash (+ offset (cl:the fixnum (load-time-value (foreign-slot-offset '(:struct standard-vertex) 'col)))) -2))
	value))


(defstruct (textured-2d-vertex-array
            (:include vertex-array)
            (:constructor make-textured-2d-vertex-array
		(&optional
		   (allocated-count +draw-list-alloc-size+)
		   (foreign-type '(:struct textured-2d-vertex))
		   (foreign-type-size (foreign-type-size foreign-type))
		   (fill-pointer 0)
		   (bytes (make-array allocated-count :element-type '(unsigned-byte 32)))))))

(declaim (inline (setf textured-2d-vertex-x)))
(defun (setf textured-2d-vertex-x) (value bytes offset)
  (declare (type single-float value))
  (declare (type fixnum offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (ash (+ offset (cl:the fixnum (load-time-value (foreign-slot-offset '(:struct textured-2d-vertex) 'x)))) -2))
	(float-features:single-float-bits value)))

(declaim (inline (setf textured-2d-vertex-y)))
(defun (setf textured-2d-vertex-y) (value bytes offset)
  (declare (type single-float value))
  (declare (type fixnum offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (ash (+ offset (cl:the fixnum (load-time-value (foreign-slot-offset '(:struct textured-2d-vertex) 'y)))) -2))
	(float-features:single-float-bits value)))

(declaim (inline (setf textured-2d-vertex-u)))
(defun (setf textured-2d-vertex-u) (value bytes offset)
  (declare (type single-float value))
  (declare (type fixnum offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (ash (+ offset (cl:the fixnum (load-time-value (foreign-slot-offset '(:struct textured-2d-vertex) 'u)))) -2))
	(float-features:single-float-bits value)))

(declaim (inline (setf textured-2d-vertex-v)))
(defun (setf textured-2d-vertex-v) (value bytes offset)
  (declare (type single-float value))
  (declare (type fixnum offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (ash (+ offset (cl:the fixnum (load-time-value (foreign-slot-offset '(:struct textured-2d-vertex) 'v)))) -2))
	(float-features:single-float-bits value)))

(declaim (inline textured-2d-vertex-array-push-extend))
(defun textured-2d-vertex-array-push-extend (textured-2d-vertex-array ub32-oid sf-x sf-y sf-u sf-v ub32-color)
  (declare (type textured-2d-vertex-array textured-2d-vertex-array))
  (declare (type single-float sf-x sf-y sf-u sf-v))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (let ((vertex-type-size (load-time-value (foreign-type-size '(:struct textured-2d-vertex)))))
    (declare (type (integer 0 512) vertex-type-size))
    (with-slots (bytes fill-pointer allocated-count) textured-2d-vertex-array
      (let ((fp fill-pointer)
	    (alloc-count allocated-count))
	(declare (type fixnum fp))
	(declare (type (integer 0 #.(ash most-positive-fixnum -10)) alloc-count))
	(unless (< fp alloc-count)
	  (let ((new-count (* 2 alloc-count)))
	    (declare (type fixnum new-count))
	    (let ((new-array (make-array (* new-count vertex-type-size) :element-type '(unsigned-byte 32)))
		  (old-array bytes))
	      (sb-sys:with-pinned-objects (new-array old-array)
		(memcpy (sb-sys:vector-sap new-array)
			(sb-sys:vector-sap old-array)
			(* (cl:the (integer 0 #.(ash most-positive-fixnum -9)) fill-pointer)
			   (cl:the (integer 0 512) vertex-type-size))))
	      (setf bytes new-array)
	      (setf allocated-count new-count))))
	;; setf foreign-slot-value has got to be slow
	;; especially when it can't reason about the type at compile time.
	(setf (standard-vertex-col bytes fp) ub32-oid
	      (standard-vertex-col bytes fp) ub32-color
	      (textured-2d-vertex-x bytes fp) sf-x
              (textured-2d-vertex-y bytes fp) sf-y
              (textured-2d-vertex-u bytes fp) sf-u
              (textured-2d-vertex-v bytes fp) sf-v)
        (prog1 fill-pointer
          (incf fp))))))

(defun standard-2d-vertex-array-push-extend (vertex-array ub32-oid sf-x sf-y ub32-color)
  (declare (type textured-2d-vertex-array vertex-array))
  (declare (type single-float sf-x sf-y))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (textured-2d-vertex-array-push-extend vertex-array ub32-oid sf-x sf-y +tex-white-pixel-u+ +tex-white-pixel-v+ ub32-color))

(defcstruct textured-3d-vertex
  (oid :unsigned-int)
  (col :unsigned-int)
  (x :float)
  (y :float)
  (z :float)
  (u :float)
  (v :float))

(defcstruct textured-3d-vertex-with-normal
  (oid :unsigned-int)
  (col :unsigned-int)
  (x :float)
  (y :float)
  (z :float)
  (u :float)
  (v :float)
  (nx :float)
  (ny :float)
  (nz :float))

(defstruct (textured-3d-vertex-array
            (:include vertex-array)
            (:constructor make-textured-3d-vertex-array
		(&optional
		   (allocated-count +draw-list-alloc-size+)
		   (foreign-type '(:struct textured-3d-vertex))
		   (foreign-type-size (foreign-type-size foreign-type))
		   (fill-pointer 0)
		   (bytes (make-array (* (cl:the (integer 0 #.(ash most-positive-fixnum -9)) allocated-count) (cl:the (integer 0 512) foreign-type-size)) :element-type '(unsigned-byte 32)))))))

(declaim (inline (setf textured-3d-vertex-x)))
(defun (setf textured-3d-vertex-x) (value bytes offset)
  (declare (type single-float value))
  (declare (type fixnum offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (ash (+ offset (cl:the fixnum (load-time-value (foreign-slot-offset '(:struct textured-3d-vertex) 'x)))) -2))
	(float-features:single-float-bits value)))

(declaim (inline (setf textured-3d-vertex-y)))
(defun (setf textured-3d-vertex-y) (value bytes offset)
  (declare (type single-float value))
  (declare (type fixnum offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (ash (+ offset (cl:the fixnum (load-time-value (foreign-slot-offset '(:struct textured-3d-vertex) 'y)))) -2))
	(float-features:single-float-bits value)))

(declaim (inline (setf textured-3d-vertex-z)))
(defun (setf textured-3d-vertex-z) (value bytes offset)
  (declare (type single-float value))
  (declare (type fixnum offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (ash (+ offset (cl:the fixnum (load-time-value (foreign-slot-offset '(:struct textured-3d-vertex) 'z)))) -2))
	(float-features:single-float-bits value)))

(declaim (inline (setf textured-3d-vertex-u)))
(defun (setf textured-3d-vertex-u) (value bytes offset)
  (declare (type single-float value))
  (declare (type fixnum offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (ash (+ offset (cl:the fixnum (load-time-value (foreign-slot-offset '(:struct textured-3d-vertex) 'u)))) -2))
	(float-features:single-float-bits value)))

(declaim (inline (setf textured-3d-vertex-v)))
(defun (setf textured-3d-vertex-v) (value bytes offset)
  (declare (type single-float value))
  (declare (type fixnum offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (ash (+ offset (cl:the fixnum (load-time-value (foreign-slot-offset '(:struct textured-3d-vertex) 'v)))) -2))
	(float-features:single-float-bits value)))

(declaim (inline textured-3d-array-push-extend))
(defun textured-3d-vertex-array-push-extend (textured-3d-vertex-array ub32-oid sf-x sf-y sf-z sf-u sf-v ub32-color)
  (declare (type textured-3d-vertex-array textured-3d-vertex-array))
  (declare (type single-float sf-x sf-y sf-u sf-v))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (let ((vertex-type-size (load-time-value (foreign-type-size '(:struct textured-3d-vertex)))))
    (declare (type (integer 0 512) vertex-type-size))
    (with-slots (bytes fill-pointer allocated-count) textured-3d-vertex-array
      (let ((fp fill-pointer)
	    (alloc-count allocated-count))
	(declare (type fixnum fp))
	(declare (type (integer 0 #.(ash most-positive-fixnum -10)) alloc-count))
	(unless (< fp alloc-count)
	  (let ((new-count (* 2 alloc-count)))
	    (declare (type fixnum new-count))
	    (let ((new-array (make-array (* new-count vertex-type-size) :element-type '(unsigned-byte 32)))
		  (old-array bytes))
	      (sb-sys:with-pinned-objects (new-array old-array)
		(memcpy (sb-sys:vector-sap new-array)
			(sb-sys:vector-sap old-array)
			(* (cl:the (integer 0 #.(ash most-positive-fixnum -9)) fill-pointer)
			   (cl:the (integer 0 512) vertex-type-size))))
	      (setf bytes new-array)
	      (setf allocated-count new-count))))
	(setf (standard-vertex-col bytes fp) ub32-oid
	      (standard-vertex-col bytes fp) ub32-color
	      (textured-3d-vertex-x bytes fp) sf-x
              (textured-3d-vertex-y bytes fp) sf-y
	      (textured-3d-vertex-z bytes fp) sf-z
              (textured-3d-vertex-u bytes fp) sf-u
              (textured-3d-vertex-v bytes fp) sf-v)
        (prog1 fill-pointer
          (incf fp))))))

;;(declaim (inline standard-3d-vertex-array-push-extend))
(defun standard-3d-vertex-array-push-extend (vertex-array ub32-oid sf-x sf-y sf-z ub32-color)
  (declare (type textured-3d-vertex-array vertex-array))
  (declare (type single-float sf-x sf-y sf-z))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (textured-3d-vertex-array-push-extend
   vertex-array ub32-oid sf-x sf-y sf-z +tex-white-pixel-u+ +tex-white-pixel-v+ ub32-color))



(defstruct (textured-3d-vertex-with-normal-array
            (:include vertex-array)
            (:constructor make-textured-3d-vertex-with-normal-array
		(&optional
		   (allocated-count +draw-list-alloc-size+)
		   (foreign-type '(:struct textured-3d-vertex-with-normal))
		   (foreign-type-size (foreign-type-size foreign-type))
		   (fill-pointer 0)
		   (bytes (make-array (* (cl:the (integer 0 #.(ash most-positive-fixnum -9)) allocated-count) (cl:the (integer 0 512) foreign-type-size)) :element-type '(unsigned-byte 32)))))))


(declaim (inline (setf textured-3d-vertex-with-normal-nx)))
(defun (setf textured-3d-vertex-with-normal-nx) (value bytes offset)
  (declare (type single-float value))
  (declare (type fixnum offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (ash (+ offset (cl:the fixnum (load-time-value (foreign-slot-offset '(:struct textured-3d-vertex-with-normal) 'nx)))) -2))
	(float-features:single-float-bits value)))

(declaim (inline (setf textured-3d-vertex-with-normal-ny)))
(defun (setf textured-3d-vertex-with-normal-ny) (value bytes offset)
  (declare (type single-float value))
  (declare (type fixnum offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (ash (+ offset (cl:the fixnum (load-time-value (foreign-slot-offset '(:struct textured-3d-vertex-with-normal) 'ny)))) -2))
	(float-features:single-float-bits value)))

(declaim (inline (setf textured-3d-vertex-with-normal-nz)))
(defun (setf textured-3d-vertex-with-normal-nz) (value bytes offset)
  (declare (type single-float value))
  (declare (type fixnum offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (ash (+ offset (cl:the fixnum (load-time-value (foreign-slot-offset '(:struct textured-3d-vertex-with-normal) 'nz)))) -2))
	(float-features:single-float-bits value)))

(declaim (inline textured-3d-vertex-with-normal-array-push-extend))
(defun textured-3d-vertex-with-normal-array-push-extend (textured-3d-vertex-with-normal-array ub32-oid sf-x sf-y sf-z sf-nx sf-ny sf-nz sf-u sf-v ub32-color)
  (declare (type textured-3d-vertex-with-normal-array textured-3d-vertex-with-normal-array))
  (declare (type single-float sf-x sf-y sf-u sf-v sf-nx sf-ny sf-nz))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (let ((vertex-type-size (load-time-value (foreign-type-size '(:struct textured-3d-vertex-with-normal)))))
    (declare (type (integer 0 512) vertex-type-size))
    (with-slots (bytes fill-pointer allocated-count) textured-3d-vertex-with-normal-array
      (let ((fp fill-pointer)
	    (alloc-count allocated-count))
	(declare (type fixnum fp))
	(declare (type (integer 0 #.(ash most-positive-fixnum -10)) alloc-count))
	(unless (< fp alloc-count)
	  (let ((new-count (* 2 alloc-count)))
	    (declare (type fixnum new-count))
	    (let ((new-array (make-array (* new-count vertex-type-size) :element-type '(unsigned-byte 32)))
		  (old-array bytes))
	      (sb-sys:with-pinned-objects (new-array old-array)
		(memcpy (sb-sys:vector-sap new-array)
			(sb-sys:vector-sap old-array)
			(* (cl:the (integer 0 #.(ash most-positive-fixnum -9)) fill-pointer)
			   (cl:the (integer 0 512) vertex-type-size))))
	      (setf bytes new-array)
	      (setf allocated-count new-count))))
	(setf (standard-vertex-col bytes fp) ub32-oid
	      (standard-vertex-col bytes fp) ub32-color
	      (textured-3d-vertex-x bytes fp) sf-x
              (textured-3d-vertex-y bytes fp) sf-y
	      (textured-3d-vertex-z bytes fp) sf-z
              (textured-3d-vertex-u bytes fp) sf-u
              (textured-3d-vertex-v bytes fp) sf-v
              (textured-3d-vertex-with-normal-nx bytes fp) sf-nx
              (textured-3d-vertex-with-normal-ny bytes fp) sf-ny
	      (textured-3d-vertex-with-normal-nz bytes fp) sf-nz)
        (prog1 fill-pointer
          (incf fp))))))

(declaim (inline standard-3d-vertex-with-normal-array-push-extend))
(defun standard-3d-vertex-with-normal-array-push-extend (vertex-array
							 ub32-oid
							 sf-x sf-y sf-z
							 sf-nx sf-ny sf-nz
							 ub32-color)
  (declare (type textured-3d-vertex-with-normal-array vertex-array))
  (declare (type single-float sf-x sf-y sf-z sf-nx sf-ny sf-nz))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (textured-3d-vertex-with-normal-array-push-extend
   vertex-array ub32-oid sf-x sf-y sf-z sf-nx sf-ny sf-nz +tex-white-pixel-u+ +tex-white-pixel-v+ ub32-color))
