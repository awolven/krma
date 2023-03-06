(in-package :krma)

(eval-when (:compile-toplevel :load-toplevel)
  (when *muffle-compilation-notes*
    #+sbcl(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))))

(eval-when (:compile-toplevel :load-toplevel)
  (when krma::*debug*
    (declaim (optimize (safety 3) (debug 3))))
  (unless krma::*debug*
    (declaim (optimize (speed 3) (safety 0) (debug 0)))
    (declaim (inline index-array-push-extend))
    (declaim (inline (setf textured-2d-vertex-oid)))
    (declaim (inline (setf textured-2d-vertex-col)))
    (declaim (inline (setf textured-2d-vertex-x)))
    (declaim (inline (setf textured-2d-vertex-y)))
    (declaim (inline (setf textured-2d-vertex-u)))
    (declaim (inline (setf textured-2d-vertex-v)))
    (declaim (inline textured-2d-vertex-array-push-extend))
    (declaim (inline (setf textured-3d-vertex-oid)))
    (declaim (inline (setf textured-3d-vertex-col)))
    (declaim (inline (setf textured-3d-vertex-x)))
    (declaim (inline (setf textured-3d-vertex-y)))
    (declaim (inline (setf textured-3d-vertex-z)))
    (declaim (inline (setf textured-3d-vertex-u)))
    (declaim (inline (setf textured-3d-vertex-v)))
    (declaim (inline textured-3d-array-push-extend))
    (declaim (inline standard-3d-vertex-array-push-extend))
    (declaim (inline (setf textured-3d-vertex-with-normal-oid)))
    (declaim (inline (setf textured-3d-vertex-with-normal-col)))
    (declaim (inline (setf textured-3d-vertex-with-normal-x)))
    (declaim (inline (setf textured-3d-vertex-with-normal-y)))
    (declaim (inline (setf textured-3d-vertex-with-normal-z)))
    (declaim (inline (setf textured-3d-vertex-with-normal-u)))
    (declaim (inline (setf textured-3d-vertex-with-normal-v)))
    (declaim (inline (setf textured-3d-vertex-with-normal-nx)))
    (declaim (inline (setf textured-3d-vertex-with-normal-ny)))
    (declaim (inline (setf textured-3d-vertex-with-normal-nz)))
    (declaim (inline textured-3d-vertex-with-normal-array-push-extend))
    (declaim (inline standard-3d-vertex-with-normal-array-push-extend))))


(defstruct (foreign-adjustable-array
	    (:conc-name "FOREIGN-ARRAY-"))
  (bytes nil :type (or simple-array null))
  (fill-pointer 0 :type fixnum)
  (allocated-count 0 :type fixnum)
  (foreign-type nil)
  (foreign-type-size 1 :type (integer 0 512))) ;; foreign-type-size can't be more than 512

(defstruct (index-array
	    (:include foreign-adjustable-array)
	    (:constructor make-index-array
		(&optional
		   (allocated-count +draw-list-alloc-size+)
		   (foreign-type :unsigned-short)
		   (foreign-type-size (foreign-type-size foreign-type))
		   (fill-pointer 0)
		   (bytes (make-array (cl:the fixnum allocated-count) :element-type '(unsigned-byte 16)))))))


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
		  #+SBCL(sb-sys:with-pinned-objects (new-array old-array)
		    (cffi-sys:with-pointer-to-vector-data (new-ptr new-array)
		      (cffi-sys:with-pointer-to-vector-data (old-ptr old-array)
			(memcpy new-ptr old-ptr
				(* (cl:the (integer 0 #.(ash most-positive-fixnum -2)) fill-pointer)
				   (cl:the (integer 0 4) foreign-type-size))))))
		  #+CCL
		  (ccl::%copy-ivector-to-ivector old-array 0 new-array 0
						 (* (cl:the (integer 0 #.(ash most-positive-fixnum -2)) fill-pointer)
						    (cl:the (integer 0 4) foreign-type-size)))
		  (setf bytes new-array)
		  (setf allocated-count new-count))))

	       (tagbody
		retry
		  (handler-case

		      (setf (aref bytes fp) index)
		    
		    (type-error (c)		      
		      (format t "~&upgrading index array from unsigned-short to unsigned-int.")
		      (finish-output)
		      (upgrade-index-array! c)
		      (go retry)))
	       
		  (return-from push-extend
		    (prog1 fp
		      (incf fill-pointer))))))))))

(defstruct (vertex-array (:include foreign-adjustable-array)))

;; we use a textured vertex for non-textured (standard) use to be able to use the same shader and draw-lists for simplicity
;; we simply put the tex coordinates of a white pixel and blend as if we were drawing text
(defcstruct textured-2d-vertex
  (oid :unsigned-int)
  (col :unsigned-int)
  (x :float)
  (y :float)
  (u :float)
  (v :float))

(setf (documentation 'symbol 'textured-2d-vertex)
      "The name of a foreign 2d vertex structure for points, lines and triangles.")


(defun (setf textured-2d-vertex-oid) (value bytes offset)
  (declare (type (unsigned-byte 32) value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct textured-2d-vertex)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct textured-2d-vertex) 'oid) -2)))))
	value))


(defun (setf textured-2d-vertex-col) (value bytes offset)
  (declare (type (unsigned-byte 32) value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct textured-2d-vertex)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct textured-2d-vertex) 'col) -2)))))
	value))

(defstruct (textured-2d-vertex-array
            (:include vertex-array)
            (:constructor make-textured-2d-vertex-array
		(&optional
		   (allocated-count +draw-list-alloc-size+)
		   (foreign-type '(:struct textured-2d-vertex))
		   (foreign-type-size (foreign-type-size foreign-type))
		   (fill-pointer 0)
		   (bytes (make-array (* (cl:the (integer 0 #.(ash most-positive-fixnum -9)) allocated-count)  (ash (cl:the (integer 0 512) foreign-type-size) -2)) :element-type '(unsigned-byte 32)))))))


(defun (setf textured-2d-vertex-x) (value bytes offset)
  (declare (type single-float value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct textured-2d-vertex)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct textured-2d-vertex) 'x) -2)))))
	(float-features:single-float-bits value)))


(defun (setf textured-2d-vertex-y) (value bytes offset)
  (declare (type single-float value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct textured-2d-vertex)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct textured-2d-vertex) 'y) -2)))))
	(float-features:single-float-bits value)))


(defun (setf textured-2d-vertex-u) (value bytes offset)
  (declare (type single-float value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct textured-2d-vertex)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct textured-2d-vertex) 'u) -2)))))
	(float-features:single-float-bits value)))


(defun (setf textured-2d-vertex-v) (value bytes offset)
  (declare (type single-float value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct textured-2d-vertex)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct textured-2d-vertex) 'v) -2)))))
	(float-features:single-float-bits value)))


(defun textured-2d-vertex-array-push-extend (textured-2d-vertex-array ub32-oid sf-x sf-y sf-u sf-v ub32-color)
  "Function for adding textured-2d-vertex to a textured-2d-vertex-array."
  (declare (type textured-2d-vertex-array textured-2d-vertex-array))
  (declare (type single-float sf-x sf-y sf-u sf-v))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (let ((vertex-type-size-uint (load-time-value (ash (foreign-type-size '(:struct textured-2d-vertex)) -2)))
	(vertex-type-size (load-time-value (foreign-type-size '(:struct textured-2d-vertex)))))
    (declare (type (integer 0 512) vertex-type-size-uint vertex-type-size))
    (with-slots (bytes fill-pointer allocated-count) textured-2d-vertex-array
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
	(setf (textured-2d-vertex-oid bytes fp) ub32-oid
	      (textured-2d-vertex-col bytes fp) ub32-color
	      (textured-2d-vertex-x bytes fp) sf-x
              (textured-2d-vertex-y bytes fp) sf-y
              (textured-2d-vertex-u bytes fp) sf-u
              (textured-2d-vertex-v bytes fp) sf-v)
        (prog1 fp
          (incf fill-pointer))))))

(defun standard-2d-vertex-array-push-extend (vertex-array ub32-oid sf-x sf-y ub32-color)
  "Function calls `textured-2d-vertex-array-push-extend' and is a shortcut for adding vertices which show no texture."
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

(setf (documentation 'symbol 'textured-3d-vertex)
      "The name of a foreign 3d vertex structure for points, lines and triangles.")

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

(setf (documentation 'symbol 'textured-3d-vertex-with-normal)
      "The name of a foreign 3d vertex structure for triangles with different shading styles.")

(defstruct (textured-3d-vertex-array
            (:include vertex-array)
            (:constructor make-textured-3d-vertex-array
		(&optional
		   (allocated-count +draw-list-alloc-size+)
		   (foreign-type '(:struct textured-3d-vertex))
		   (foreign-type-size (foreign-type-size foreign-type))
		   (fill-pointer 0)
		   (bytes (make-array (* (cl:the (integer 0 #.(ash most-positive-fixnum -9)) allocated-count) (ash (cl:the (integer 0 512) foreign-type-size) -2)) :element-type '(unsigned-byte 32)))))))



(defun (setf textured-3d-vertex-oid) (value bytes offset)
  (declare (type (unsigned-byte 32) value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct textured-3d-vertex)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct textured-3d-vertex) 'oid) -2)))))
	value))


(defun (setf textured-3d-vertex-col) (value bytes offset)
  (declare (type (unsigned-byte 32) value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct textured-3d-vertex)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct textured-3d-vertex) 'col) -2)))))
	value))


(defun (setf textured-3d-vertex-x) (value bytes offset)
  (declare (type single-float value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct textured-3d-vertex)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct textured-3d-vertex) 'x) -2)))))
	(float-features:single-float-bits value)))


(defun (setf textured-3d-vertex-y) (value bytes offset)
  (declare (type single-float value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct textured-3d-vertex)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct textured-3d-vertex) 'y) -2)))))
	(float-features:single-float-bits value)))


(defun (setf textured-3d-vertex-z) (value bytes offset)
  (declare (type single-float value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct textured-3d-vertex)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct textured-3d-vertex) 'z) -2)))))
	(float-features:single-float-bits value)))


(defun (setf textured-3d-vertex-u) (value bytes offset)
  (declare (type single-float value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct textured-3d-vertex)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct textured-3d-vertex) 'u) -2)))))
	(float-features:single-float-bits value)))


(defun (setf textured-3d-vertex-v) (value bytes offset)
  (declare (type single-float value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct textured-3d-vertex)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct textured-3d-vertex) 'v) -2)))))
	(float-features:single-float-bits value)))


(defun textured-3d-vertex-array-push-extend (textured-3d-vertex-array ub32-oid sf-x sf-y sf-z sf-u sf-v ub32-color)
  "Function for adding textured-3d-vertex to a textured-3d-vertex-array."
  (declare (type textured-3d-vertex-array textured-3d-vertex-array))
  (declare (type single-float sf-x sf-y sf-u sf-v))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (let ((vertex-type-size-uint (load-time-value (ash (foreign-type-size '(:struct textured-3d-vertex)) -2)))
	(vertex-type-size (load-time-value (foreign-type-size '(:struct textured-3d-vertex)))))
    (declare (type (integer 0 512) vertex-type-size vertex-type-size-uint))
    (with-slots (bytes fill-pointer allocated-count) textured-3d-vertex-array
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
	(setf (textured-3d-vertex-oid bytes fp) ub32-oid
	      (textured-3d-vertex-col bytes fp) ub32-color
	      (textured-3d-vertex-x bytes fp) sf-x
              (textured-3d-vertex-y bytes fp) sf-y
	      (textured-3d-vertex-z bytes fp) sf-z
              (textured-3d-vertex-u bytes fp) sf-u
              (textured-3d-vertex-v bytes fp) sf-v)
        (prog1 fp
          (incf fill-pointer))))))


(defun standard-3d-vertex-array-push-extend (vertex-array ub32-oid sf-x sf-y sf-z ub32-color)
  "Function calls `textured-3d-vertex-array-push-extend' and is a shortcut for adding vertices which show no texture."
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
		   (bytes (make-array (* (cl:the (integer 0 #.(ash most-positive-fixnum -9)) allocated-count)  (ash (cl:the (integer 0 512) foreign-type-size) -2)) :element-type '(unsigned-byte 32)))))))


(defun (setf textured-3d-vertex-with-normal-oid) (value bytes offset)
  (declare (type (unsigned-byte 32) value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct textured-3d-vertex-with-normal)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct textured-3d-vertex-with-normal) 'oid) -2)))))
	value))


(defun (setf textured-3d-vertex-with-normal-col) (value bytes offset)
  (declare (type (unsigned-byte 32) value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct textured-3d-vertex-with-normal)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct textured-3d-vertex-with-normal) 'col) -2)))))
	value))


(defun (setf textured-3d-vertex-with-normal-x) (value bytes offset)
  (declare (type single-float value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct textured-3d-vertex-with-normal)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct textured-3d-vertex-with-normal) 'x) -2)))))
	(float-features:single-float-bits value)))


(defun (setf textured-3d-vertex-with-normal-y) (value bytes offset)
  (declare (type single-float value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct textured-3d-vertex-with-normal)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct textured-3d-vertex-with-normal) 'y) -2)))))
	(float-features:single-float-bits value)))


(defun (setf textured-3d-vertex-with-normal-z) (value bytes offset)
  (declare (type single-float value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct textured-3d-vertex-with-normal)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct textured-3d-vertex-with-normal) 'z) -2)))))
	(float-features:single-float-bits value)))


(defun (setf textured-3d-vertex-with-normal-u) (value bytes offset)
  (declare (type single-float value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct textured-3d-vertex-with-normal)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct textured-3d-vertex-with-normal) 'u) -2)))))
	(float-features:single-float-bits value)))


(defun (setf textured-3d-vertex-with-normal-v) (value bytes offset)
  (declare (type single-float value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct textured-3d-vertex-with-normal)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct textured-3d-vertex-with-normal) 'v) -2)))))
	(float-features:single-float-bits value)))


(defun (setf textured-3d-vertex-with-normal-nx) (value bytes offset)
  (declare (type single-float value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 152) (load-time-value (ash (foreign-type-size '(:struct textured-3d-vertex-with-normal)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct textured-3d-vertex-with-normal) 'nx) -2)))))
	(float-features:single-float-bits value)))


(defun (setf textured-3d-vertex-with-normal-ny) (value bytes offset)
  (declare (type single-float value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct textured-3d-vertex-with-normal)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct textured-3d-vertex-with-normal) 'ny) -2)))))
	(float-features:single-float-bits value)))


(defun (setf textured-3d-vertex-with-normal-nz) (value bytes offset)
  (declare (type single-float value))
  (declare (type (integer 0 #.(ash most-positive-fixnum -9)) offset))
  (declare (type (simple-array (unsigned-byte 32)) bytes))
  (setf (aref bytes (+ (* offset (cl:the (integer 0 512) (load-time-value (ash (foreign-type-size '(:struct textured-3d-vertex-with-normal)) -2))))
		       (cl:the fixnum (load-time-value (ash (foreign-slot-offset '(:struct textured-3d-vertex-with-normal) 'nz) -2)))))
	(float-features:single-float-bits value)))


(defun textured-3d-vertex-with-normal-array-push-extend (textured-3d-vertex-with-normal-array ub32-oid sf-x sf-y sf-z sf-nx sf-ny sf-nz sf-u sf-v ub32-color)
  "Function for adding textured-3d-vertex-with-normal to a textured-3d-vertex-with-normal-array."
  (declare (type textured-3d-vertex-with-normal-array textured-3d-vertex-with-normal-array))
  (declare (type single-float sf-x sf-y sf-u sf-v sf-nx sf-ny sf-nz))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (let ((vertex-type-size-uint (load-time-value (ash (foreign-type-size '(:struct textured-3d-vertex-with-normal)) -2)))
	(vertex-type-size (load-time-value (foreign-type-size '(:struct textured-3d-vertex-with-normal)))))
    (declare (type (integer 0 512) vertex-type-size vertex-type-size-uint))
    (with-slots (bytes fill-pointer allocated-count) textured-3d-vertex-with-normal-array
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
	(setf (textured-3d-vertex-with-normal-oid bytes fp) ub32-oid
	      (textured-3d-vertex-with-normal-col bytes fp) ub32-color
	      (textured-3d-vertex-with-normal-x bytes fp) sf-x
              (textured-3d-vertex-with-normal-y bytes fp) sf-y
	      (textured-3d-vertex-with-normal-z bytes fp) sf-z
              (textured-3d-vertex-with-normal-u bytes fp) sf-u
              (textured-3d-vertex-with-normal-v bytes fp) sf-v
              (textured-3d-vertex-with-normal-nx bytes fp) sf-nx
              (textured-3d-vertex-with-normal-ny bytes fp) sf-ny
	      (textured-3d-vertex-with-normal-nz bytes fp) sf-nz)
        (prog1 fp
          (incf fill-pointer))))))


(defun standard-3d-vertex-with-normal-array-push-extend (vertex-array
							 ub32-oid
							 sf-x sf-y sf-z
							 sf-nx sf-ny sf-nz
							 ub32-color)
  "Function calls `textured-3d-vertex-with-normal-array-push-extend' and is a shortcut for adding vertices with normals which show no texture."
  (declare (type textured-3d-vertex-with-normal-array vertex-array))
  (declare (type single-float sf-x sf-y sf-z sf-nx sf-ny sf-nz))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (textured-3d-vertex-with-normal-array-push-extend
   vertex-array ub32-oid sf-x sf-y sf-z sf-nx sf-ny sf-nz +tex-white-pixel-u+ +tex-white-pixel-v+ ub32-color))
