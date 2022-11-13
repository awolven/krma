(in-package :krma)

;;(declaim (optimize (speed 3) (safety 0) (debug 3)))

(defstruct (foreign-adjustable-array
             (:conc-name "FOREIGN-ARRAY-"))
  (ptr +nullptr+ :type sb-sys:system-area-pointer)
  (fill-pointer 0 :type fixnum)
  (allocated-count +draw-list-alloc-size+ :type fixnum)
  (foreign-type nil)
  (foreign-type-size 1 :type (integer 0 512))) ;; foreign-type-size can't be more than 512

(defstruct (index-array
             (:include foreign-adjustable-array)
             (:constructor make-index-array
			   (&optional
			    (allocated-count +draw-list-alloc-size+)
			    (foreign-type :unsigned-short)
			    (fill-pointer 0)
			    (ptr (foreign-alloc :unsigned-short :count (cl:the fixnum allocated-count)))
			    (foreign-type-size (foreign-type-size foreign-type))))))

;;(declaim (inline index-array-push-extend))
(defun index-array-push-extend (index-array index)
  (declare (type index-array index-array))
  (declare (type (unsigned-byte 32) index))
  (block push-extend
    (let ((err nil))
      (flet ((upgrade-index-array! ()
               (when err
		 (error "unknown type error in index-array-push-extend"))
               (with-slots (ptr
                            allocated-count
                            fill-pointer
                            foreign-type
                            foreign-type-size)
                   index-array
		 (let ((new-ptr (foreign-alloc :unsigned-int :count allocated-count)))
                   (loop for i from 0 below fill-pointer ;; copy the indexes
			 do (setf (mem-aref new-ptr :unsigned-int i)
                                  (mem-aref ptr :unsigned-short i)))
                   (foreign-free ptr)
                   (setf foreign-type :unsigned-int
			 foreign-type-size (load-time-value (foreign-type-size :unsigned-int))
			 ptr new-ptr
			 err t)
                   (values)))))
	(with-slots (ptr
                     allocated-count
                     fill-pointer
                     foreign-type
                     foreign-type-size)
            index-array

	  (unless (< fill-pointer allocated-count)
            (let* ((new-count (* 2 allocated-count)))
	      (declare (type fixnum new-count))
	      (let ((new-array (foreign-alloc foreign-type :count new-count))
		    (old-array ptr))
		(memcpy new-array old-array
			(* (cl:the (integer 0 #.(floor most-positive-fixnum 512)) fill-pointer)
			   (cl:the (integer 0 512) foreign-type-size)))
		(setf ptr new-array)
		(setf allocated-count new-count)
		(foreign-free old-array))))

	  (tagbody
	   retry
             (handler-case
		 (setf (mem-aref (foreign-array-ptr index-array)
				 (foreign-array-foreign-type index-array)
				 (foreign-array-fill-pointer index-array))
                       index)
               (type-error ()
		 (format t "~&upgrading index array from unsigned-short to unsigned-int.")
		 (finish-output)
		 (upgrade-index-array!)
		 (go retry)))
	     (return-from push-extend
	       (prog1 fill-pointer
		 (incf fill-pointer)))))))))

(defstruct (vertex-array (:include foreign-adjustable-array)))

;; we use a textured vertex for non-textured (standard) use to be able to use the same shader and draw-lists for simplicity
;; we simply put the tex coordinates of a white pixel and blend as if we were drawing text
(defcstruct textured-2d-vertex
    (x :float)
  (y :float)
  (u :float)
  (v :float)
  (col :uint32))

(defstruct (textured-2d-vertex-array
             (:include vertex-array)
             (:constructor make-textured-2d-vertex-array
			   (&optional
			    (allocated-count +draw-list-alloc-size+)
			    (foreign-type '(:struct textured-2d-vertex))
			    (fill-pointer 0)
			    (ptr (foreign-alloc foreign-type :count allocated-count))
			    (foreign-type-size (foreign-type-size foreign-type))))))

(declaim (inline (setf textured-2d-vertex-x)))
(defun (setf textured-2d-vertex-x) (value ptr)
  (declare (type single-float value))
  (declare (type sb-sys:system-area-pointer ptr))
  (cffi::mem-set value ptr :float (load-time-value (foreign-slot-offset '(:struct textured-2d-vertex) 'x))))

(declaim (inline (setf textured-2d-vertex-y)))
(defun (setf textured-2d-vertex-y) (value ptr)
  (declare (type single-float value))
  (declare (type sb-sys:system-area-pointer ptr))
  (cffi::mem-set value ptr :float (load-time-value (foreign-slot-offset '(:struct textured-2d-vertex) 'y))))

(declaim (inline (setf textured-2d-vertex-u)))
(defun (setf textured-2d-vertex-u) (value ptr)
  (declare (type single-float value))
  (declare (type sb-sys:system-area-pointer ptr))
  (cffi::mem-set value ptr :float (load-time-value (foreign-slot-offset '(:struct textured-2d-vertex) 'u))))

(declaim (inline (setf textured-2d-vertex-v)))
(defun (setf textured-2d-vertex-v) (value ptr)
  (declare (type single-float value))
  (declare (type sb-sys:system-area-pointer ptr))
  (cffi::mem-set value ptr :float (load-time-value (foreign-slot-offset '(:struct textured-2d-vertex) 'v))))

(declaim (inline (setf textured-2d-vertex-col)))
(defun (setf textured-2d-vertex-col) (value ptr)
  (declare (type (unsigned-byte 32) value))
  (declare (type sb-sys:system-area-pointer ptr))
  (cffi::mem-set value ptr :unsigned-int (load-time-value (foreign-slot-offset '(:struct textured-2d-vertex) 'col))))

(declaim (inline textured-2d-vertex-array-push-extend))
(defun textured-2d-vertex-array-push-extend (textured-2d-vertex-array sf-x sf-y sf-u sf-v ub32-color)
  (declare (type textured-2d-vertex-array textured-2d-vertex-array))
  (declare (type single-float sf-x sf-y sf-u sf-v))
  (declare (type (unsigned-byte 32) ub32-color))
  (let ((vertex-type '(:struct textured-2d-vertex))
        (vertex-type-size (load-time-value (foreign-type-size '(:struct textured-2d-vertex)))))
    (with-slots (ptr fill-pointer allocated-count) textured-2d-vertex-array
      (unless (< fill-pointer allocated-count)
        (let ((new-count (* 2 allocated-count)))
	  (declare (type fixnum new-count))
	  (let ((new-array (foreign-alloc vertex-type :count new-count))
		(old-array ptr))
	    (memcpy new-array old-array
		    (* (cl:the (integer 0 #.(floor most-positive-fixnum 512)) fill-pointer)
		       (cl:the (integer 0 512) vertex-type-size)))
	    (setf ptr new-array)
	    (setf allocated-count new-count)
	    (foreign-free old-array))))
      (let ((ptr (mem-aptr ptr vertex-type fill-pointer)))
        ;; setf foreign-slot-value has got to be slow
        ;; especially when it can't reason about the type at compile time.
        (setf (textured-2d-vertex-x ptr) sf-x
              (textured-2d-vertex-y ptr) sf-y
              (textured-2d-vertex-u ptr) sf-u
              (textured-2d-vertex-v ptr) sf-v
              (textured-2d-vertex-col ptr) ub32-color)
        (prog1 fill-pointer
          (incf fill-pointer))))))

(defun standard-2d-vertex-array-push-extend (vertex-array sf-x sf-y ub32-color)
  (declare (type textured-2d-vertex-array vertex-array))
  (declare (type single-float sf-x sf-y))
  (declare (type (unsigned-byte 32) ub32-color))
  (textured-2d-vertex-array-push-extend vertex-array sf-x sf-y +tex-white-pixel-u+ +tex-white-pixel-v+ ub32-color))

(defcstruct textured-3d-vertex
    (x :float)
  (y :float)
  (z :float)
  (u :float)
  (v :float)
  (col :uint32))

(defcstruct textured-3d-vertex-with-normal
    (x :float)
  (y :float)
  (z :float)
  (u :float)
  (v :float)
  (col :uint32)
  (nx :float)
  (ny :float)
  (nz :float))

(defstruct (textured-3d-vertex-array
             (:include vertex-array)
             (:constructor make-textured-3d-vertex-array
			   (&optional
			    (allocated-count +draw-list-alloc-size+)
			    (foreign-type '(:struct textured-3d-vertex))
			    (fill-pointer 0)
			    (ptr (foreign-alloc foreign-type :count allocated-count))
			    (foreign-type-size (foreign-type-size foreign-type))))))

(declaim (inline (setf textured-2d-vertex-x)))
(defun (setf textured-3d-vertex-x) (value ptr)
  (declare (type single-float value))
  (declare (type sb-sys:system-area-pointer ptr))
  (cffi::mem-set value ptr :float (load-time-value (foreign-slot-offset '(:struct textured-3d-vertex-with-normal) 'x))))

(declaim (inline (setf textured-2d-vertex-y)))
(defun (setf textured-3d-vertex-y) (value ptr)
  (declare (type single-float value))
  (declare (type sb-sys:system-area-pointer ptr))
  (cffi::mem-set value ptr :float (load-time-value (foreign-slot-offset '(:struct textured-3d-vertex-with-normal) 'y))))

(declaim (inline (setf textured-2d-vertex-z)))
(defun (setf textured-3d-vertex-z) (value ptr)
  (declare (type single-float value))
  (declare (type sb-sys:system-area-pointer ptr))
  (cffi::mem-set value ptr :float (load-time-value (foreign-slot-offset '(:struct textured-3d-vertex-with-normal) 'z))))

(declaim (inline (setf textured-2d-vertex-u)))
(defun (setf textured-3d-vertex-u) (value ptr)
  (declare (type single-float value))
  (declare (type sb-sys:system-area-pointer ptr))
  (cffi::mem-set value ptr :float (load-time-value (foreign-slot-offset '(:struct textured-3d-vertex-with-normal) 'u))))

(declaim (inline (setf textured-2d-vertex-v)))
(defun (setf textured-3d-vertex-v) (value ptr)
  (declare (type single-float value))
  (declare (type sb-sys:system-area-pointer ptr))
  (cffi::mem-set value ptr :float (load-time-value (foreign-slot-offset '(:struct textured-3d-vertex-with-normal) 'v))))

(declaim (inline (setf textured-2d-vertex-col)))
(defun (setf textured-3d-vertex-col) (value ptr)
  (declare (type (unsigned-byte 32) value))
  (declare (type sb-sys:system-area-pointer ptr))
  (cffi::mem-set value ptr :uint32 (load-time-value (foreign-slot-offset '(:struct textured-3d-vertex-with-normal) 'col))))

;;(declaim (inline textured-3d-array-push-extend))
(defun textured-3d-vertex-array-push-extend (vertex-array sf-x sf-y sf-z sf-u sf-v ub32-color)
  (declare (type textured-3d-vertex-array vertex-array))
  (declare (type single-float sf-x sf-y sf-z sf-u sf-v))
  (declare (type (unsigned-byte 32) ub32-color))
  (let ((vertex-type '(:struct textured-3d-vertex))
        (vertex-type-size (load-time-value (foreign-type-size '(:struct textured-3d-vertex)))))
    (with-slots (ptr fill-pointer allocated-count) vertex-array
      (unless (< fill-pointer allocated-count)
        (let ((new-count (* 2 allocated-count)))
	  (declare (type fixnum new-count))
	  (let ((new-array (foreign-alloc vertex-type :count new-count))
		(old-array ptr))
	    (memcpy new-array old-array
		    (* (cl:the (integer 0 #.(floor most-positive-fixnum 512)) fill-pointer)
		       (cl:the (integer 0 512) vertex-type-size)))
	    (setf ptr new-array)
	    (setf allocated-count new-count)
	    (foreign-free old-array))))
      (let ((ptr (mem-aptr ptr vertex-type fill-pointer)))
        (setf (textured-3d-vertex-x ptr) sf-x
              (textured-3d-vertex-y ptr) sf-y
              (textured-3d-vertex-z ptr) sf-z
              (textured-3d-vertex-u ptr) sf-u
              (textured-3d-vertex-v ptr) sf-v
              (textured-3d-vertex-col ptr) ub32-color)
        (prog1 fill-pointer
          (incf fill-pointer))))))

;;(declaim (inline standard-3d-vertex-array-push-extend))
(defun standard-3d-vertex-array-push-extend (vertex-array sf-x sf-y sf-z ub32-color)
  (declare (type textured-3d-vertex-array vertex-array))
  (declare (type single-float sf-x sf-y sf-z))
  (declare (type (unsigned-byte 32) ub32-color))
  (textured-3d-vertex-array-push-extend
   vertex-array sf-x sf-y sf-z +tex-white-pixel-u+ +tex-white-pixel-v+ ub32-color))



(defstruct (textured-3d-vertex-with-normal-array
             (:include vertex-array)
             (:constructor make-textured-3d-vertex-with-normal-array
			   (&optional
			    (allocated-count +draw-list-alloc-size+)
			    (foreign-type '(:struct textured-3d-vertex-with-normal))
			    (fill-pointer 0)
			    (ptr (foreign-alloc foreign-type :count allocated-count))
			    (foreign-type-size (foreign-type-size foreign-type))))))

(declaim (inline (setf textured-2d-vertex-with-normal-nx)))
(defun (setf textured-3d-vertex-with-normal-nx) (value ptr)
  (declare (type single-float value))
  (declare (type sb-sys:system-area-pointer ptr))
  (cffi::mem-set value ptr :float (load-time-value (foreign-slot-offset '(:struct textured-3d-vertex-with-normal) 'nx))))

(declaim (inline (setf textured-2d-vertex-with-normal-ny)))
(defun (setf textured-3d-vertex-with-normal-ny) (value ptr)
  (declare (type single-float value))
  (declare (type sb-sys:system-area-pointer ptr))
  (cffi::mem-set value ptr :float (load-time-value (foreign-slot-offset '(:struct textured-3d-vertex-with-normal) 'ny))))

(declaim (inline (setf textured-2d-vertex-with-normal-nz)))
(defun (setf textured-3d-vertex-with-normal-nz) (value ptr)
  (declare (type single-float value))
  (declare (type sb-sys:system-area-pointer ptr))
  (cffi::mem-set value ptr :float (load-time-value (foreign-slot-offset '(:struct textured-3d-vertex-with-normal) 'nz))))

(declaim (inline textured-3d-vertex-with-normal-array-push-extend))
(defun textured-3d-vertex-with-normal-array-push-extend (vertex-array
							 sf-x sf-y sf-z
							 sf-nx sf-ny sf-nz
							 sf-u sf-v
							 ub32-color)
  (declare (type textured-3d-vertex-with-normal-array vertex-array))
  (declare (type single-float sf-x sf-y sf-z sf-nx sf-ny sf-nz sf-u sf-v))
  (declare (type (unsigned-byte 32) ub32-color))
  (let ((vertex-type '(:struct textured-3d-vertex-with-normal))
        (vertex-type-size (load-time-value (foreign-type-size '(:struct textured-3d-vertex-with-normal)))))
    (with-slots (ptr fill-pointer allocated-count) vertex-array
      (unless (< fill-pointer allocated-count)
        (let* ((new-count (* 2 allocated-count)))
	  (declare (type fixnum new-count))
	  (let ((new-array (foreign-alloc vertex-type :count new-count))
		(old-array ptr))
	    

	    (memcpy new-array old-array (* (cl:the (integer 0 #.(floor most-positive-fixnum 512)) fill-pointer)
					   (cl:the (integer 0 512) vertex-type-size)))
	    (setf ptr new-array)
	    (setf allocated-count new-count)
	    (foreign-free old-array))))
      (let ((ptr (mem-aptr ptr vertex-type fill-pointer)))
        (setf (textured-3d-vertex-x ptr) sf-x
              (textured-3d-vertex-y ptr) sf-y
              (textured-3d-vertex-z ptr) sf-z
              (textured-3d-vertex-u ptr) sf-u
              (textured-3d-vertex-v ptr) sf-v
              (textured-3d-vertex-col ptr) ub32-color
              (textured-3d-vertex-with-normal-nx ptr) sf-nx
              (textured-3d-vertex-with-normal-ny ptr) sf-ny
              (textured-3d-vertex-with-normal-nz ptr) sf-nz)
	(prog1 fill-pointer
          (incf fill-pointer))))))

;;(declaim (inline standard-3d-vertex-with-normal-array-push-extend))
(defun standard-3d-vertex-with-normal-array-push-extend (vertex-array
							 sf-x sf-y sf-z
							 sf-nx sf-ny sf-nz
							 ub32-color)
  (declare (type textured-3d-vertex-with-normal-array vertex-array))
  (declare (type single-float sf-x sf-y sf-z sf-nx sf-ny sf-nz))
  (declare (type (unsigned-byte 32) ub32-color))
  (textured-3d-vertex-with-normal-array-push-extend
   vertex-array sf-x sf-y sf-z sf-nx sf-ny sf-nz +tex-white-pixel-u+ +tex-white-pixel-v+ ub32-color))
