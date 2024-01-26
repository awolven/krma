(in-package :krma)

(eval-when (:compile-toplevel :load-toplevel)
  (when *muffle-compilation-notes*
    #+sbcl(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))))

(eval-when (:compile-toplevel :load-toplevel)
  (when krma::*debug*
    (declaim (optimize (safety 3) (debug 3))))
  (unless krma::*debug*
    (declaim (optimize (speed 3) (safety 0) (debug 0)))))

(defvar *media* nil)

(defun default-medium ()
  (car *media*))

(defcstruct 3DMatrix
  (m00 :float)
  (m10 :float)
  (m20 :float)
  (m30 :float)

  (m01 :float)
  (m11 :float)
  (m21 :float)
  (m31 :float)

  (m02 :float)
  (m12 :float)
  (m22 :float)
  (m32 :float)

  (m03 :float)
  (m13 :float)
  (m23 :float)
  (m33 :float))

(defun copy-matrix-to-foreign (lisp-matrix p-matrix)
  ;; arrays in 3d-math are column major internally
  ;; matrices in glsl are also column major
  (let ((array (3dm::marr lisp-matrix)))
    (loop for i from 0 below 4
	  do (loop for j from 0 below 4
		   do (setf (mem-aref p-matrix :float (+ j (* i 4)))
			    (clampf (aref array (+ j (* i 4)))))))
    (values)))

(defun copy-float-to-foreign (float p-float)
  ;; arrays in 3d-math are column major internally
  ;; matrices in glsl are also column major
  (setf (mem-aref p-float :float) (clampf float))
  (values))

#+NIL
(defun copy-matrix-to-foreign (lisp-matrix p-matrix)
  (let ((array (3dm.f::marr4 lisp-matrix)))
    (sb-sys:with-pinned-objects (array)
      (vk::memcpy p-matrix (sb-sys:vector-sap array) (load-time-value (* 16 (foreign-type-size :float))))))
  (values))

(defcstruct vertex-uniform-buffer
  (view (:struct 3DMatrix))
  (proj (:struct 3DMatrix))
  (vproj (:struct 3DMatrix))
  (width :float)
  (height :float)
  (near :float)
  (far :float))

(defcstruct fragment-uniform-buffer
  (lights (:array (:struct light) 10))
  (num-lights :unsigned-int)
  (scene-ambient :unsigned-int)
  (bucket-capacity :unsigned-int)
  (table-capacity :unsigned-int)
  (bucket-pointer :uint64)
  (table-pointer :uint64)
  (bucket-counter :uint64))
  

(defun update-fragment-uniform-buffer (pipeline scene window current-frame)
  (let ((lights (scene-lights scene)))
    (with-foreign-object (p-stage '(:struct fragment-uniform-buffer))
      (let ((p-lights (foreign-slot-pointer p-stage '(:struct fragment-uniform-buffer) 'lights)))
	(loop  for i from 0 for light in lights
	       when (typep light 'light-mixin)
	       do (let ((p-light (mem-aptr p-lights '(:struct light) i)))
		    (with-slots (position
				 diffuse
				 specular
				 constant-attenuation
				 linear-attenuation
				 quadratic-attenuation
				 spot-cutoff
				 spot-exponent
				 spot-direction)
			light
		      (setf (foreign-slot-value p-light '(:struct light) 'pos-x) (clampf (vx position))
			    (foreign-slot-value p-light '(:struct light) 'pos-y) (clampf (vy position))
			    (foreign-slot-value p-light '(:struct light) 'pos-z) (clampf (vz position))
			    (foreign-slot-value p-light '(:struct light) 'pos-w) (typecase light
										   (directional-light 0.0f0)
										   (light-mixin 1.0f0)
										   (t 0.0f0))
			    (foreign-slot-value p-light '(:struct light) 'diffuse) (canonicalize-color diffuse)
			    (foreign-slot-value p-light '(:struct light) 'specular) (canonicalize-color specular)
			    (foreign-slot-value p-light '(:struct light) 'constant-attenuation) (clampf constant-attenuation)
			    (foreign-slot-value p-light '(:struct light) 'linear-attenuation) (clampf linear-attenuation)
			    (foreign-slot-value p-light '(:struct light) 'quadratic-attenuation) (clampf quadratic-attenuation)
			    (foreign-slot-value p-light '(:struct light) 'spot-cutoff) (clampf spot-cutoff)
			    (foreign-slot-value p-light '(:struct light) 'spot-exponent) (clampf spot-exponent)
			    (foreign-slot-value p-light '(:struct light) 'spot-direction-x) (clampf (vx spot-direction))
			    (foreign-slot-value p-light '(:struct light) 'spot-direction-y) (clampf (vy spot-direction))
			    (foreign-slot-value p-light '(:struct light) 'spot-direction-z) (clampf (vz spot-direction)))))
	       finally (setf (foreign-slot-value p-stage '(:struct fragment-uniform-buffer) 'num-lights) i)
		       (setf (foreign-slot-value p-stage '(:struct fragment-uniform-buffer) 'scene-ambient) (canonicalize-color
													     (scene-ambient scene)))
	     (setf (foreign-slot-value p-stage '(:struct fragment-uniform-buffer) 'bucket-capacity) 1024)
	     (setf (foreign-slot-value p-stage '(:struct fragment-uniform-buffer) 'table-capacity) 1024)
	     (setf (foreign-slot-value p-stage '(:struct fragment-uniform-buffer) 'bucket-pointer)
		   (aref (krma-selection-set-buckets-pointers window) current-frame))
	     (setf (foreign-slot-value p-stage '(:struct fragment-uniform-buffer) 'table-pointer)
		   (aref (krma-selection-set-table-pointers window) current-frame))
	     (setf (foreign-slot-value p-stage '(:struct fragment-uniform-buffer) 'bucket-counter)
		   (aref (krma-selection-set-counter-pointers window) current-frame))
	     
			     
	     
		       (copy-uniform-buffer-memory (default-logical-device pipeline)
						   p-stage
						   (allocated-memory (pipeline-fragment-uniform-buffer pipeline))
						   (load-time-value (foreign-type-size '(:struct fragment-uniform-buffer))))))))
  (values))

(defclass krma-essential-scene-mixin (clim:medium)
  ((im-draw-data :accessor im-draw-data)
   (rm-draw-data :accessor rm-draw-data)
   (lights :initform (list (make-instance 'directional-light)) :accessor scene-lights)
   
   (3d-camera-projection-matrix)
   (3d-camera-view-matrix)
   (2d-camera-projection-matrix)
   (2d-camera-view-matrix)

   (ambient :initform *default-scene-ambient* :accessor scene-ambient)

   (children :initform () :accessor node-children))
  (:documentation "Absract base class for scenes in krma. Define your own scene classes with this mixin as a superclass."))

#+sbcl
(defun finalize-scene (scene)
  (let ((draw-data (im-draw-data scene)))
    (sb-ext:finalize scene
                     #'(lambda ()
                         (%purge-im-groups-1 draw-data))
                     :dont-save t)))

#+ccl
(defun finalize-scene (scene))

(defmethod initialize-instance :after ((instance krma-essential-scene-mixin) &rest initargs &key (display (default-display))
				       &allow-other-keys)
  (declare (ignore initargs))
  (setf (im-draw-data instance) (make-immediate-mode-draw-data "IM Draw Data" display))
  (setf (rm-draw-data instance) (make-array 2 :initial-contents
					    (list
					     (make-retained-mode-draw-data "RM Draw Data 0" display)
					     (make-retained-mode-draw-data "RM Draw Data 1" display))))
  (finalize-scene instance)
  (values))

(defclass standard-scene (krma-essential-scene-mixin)
  ()
  (:documentation "A concrete scene class based on krma-essential-scene-mixin used in krma-test-frame-manager."))


(defmethod render-scene ((scene krma-essential-scene-mixin)
			 window viewport dpy command-buffer rm-draw-data im-draw-data)
  (render-3d-scene scene window viewport dpy command-buffer rm-draw-data im-draw-data)
  (vkCmdNextSubpass (h command-buffer) VK_SUBPASS_CONTENTS_INLINE)
  (render-2d-scene scene window viewport dpy command-buffer rm-draw-data im-draw-data)
  (values))

(defmethod render-2d-scene ((scene krma-essential-scene-mixin)
			    window viewport dpy command-buffer rm-draw-data im-draw-data)
  "The default method to render krma scenes.  You can define your own methods for your own scene classes and call-next-method if you like."

  ;; todo: think about having separate clos objects for 3d-scene and 2d-scene
  
  (let ((device (default-logical-device dpy))
	(pipeline-store (krma-pipeline-store dpy)))

    (with-slots (x y width height 2d-camera 3d-camera) viewport

      (let ((2d-camera-projection-matrix (camera-proj-matrix 2d-camera))
	    (2d-camera-view-matrix (camera-view-matrix 2d-camera)))    

	(loop for (p dl) on (2d-cmd-oriented-combinations pipeline-store rm-draw-data) by #'cddr
	      do (render-draw-list-cmds p rm-draw-data dl dpy device command-buffer
					scene
					window
					2d-camera-view-matrix 2d-camera-projection-matrix
					viewport 0.0f0 +select-box-2d-depth+))

	(loop for (p dl) on (2d-draw-list-oriented-combinations pipeline-store rm-draw-data) by #'cddr
	      do (render-draw-list p rm-draw-data dl dpy device command-buffer
				   scene
				   window
				   2d-camera-view-matrix 2d-camera-projection-matrix
				   viewport  0.0f0 +select-box-2d-depth+))

	(loop for (p dl) on (2d-cmd-oriented-combinations pipeline-store im-draw-data) by #'cddr
	      do (render-draw-list-cmds p im-draw-data dl dpy device command-buffer
					scene
					window
					2d-camera-view-matrix 2d-camera-projection-matrix
					viewport 0.0f0 +select-box-2d-depth+))

	(loop for (p dl) on (2d-draw-list-oriented-combinations pipeline-store im-draw-data) by #'cddr
	      do (render-draw-list p im-draw-data dl dpy device command-buffer
				   scene
				   window
				   2d-camera-view-matrix 2d-camera-projection-matrix
				   viewport 0.0f0 +select-box-2d-depth+))
	(values)))))

(defmethod render-3d-scene ((scene krma-essential-scene-mixin)
			    window viewport dpy command-buffer rm-draw-data im-draw-data)
  "The default method to render krma scenes.  You can define your own methods for your own scene classes and call-next-method if you like."

  ;; todo: think about having separate clos objects for 3d-scene and 2d-scene
  
  (let ((device (default-logical-device dpy))
	(pipeline-store (krma-pipeline-store dpy)))

    (with-slots (x y width height 2d-camera 3d-camera) viewport

      (let ((3d-camera-projection-matrix (camera-proj-matrix 3d-camera))
	    (3d-camera-view-matrix (camera-view-matrix 3d-camera))
	    (near (camera-near 3d-camera))
	    (far (camera-far 3d-camera)))

	;;(print 2d-camera-projection-matrix)
	;;(print 2d-camera-view-matrix)

	(loop for (p dl) on (3d-draw-list-oriented-combinations pipeline-store rm-draw-data dpy) by #'cddr
	      do (render-draw-list p rm-draw-data dl dpy device command-buffer
				   scene
				   window
				   3d-camera-view-matrix 3d-camera-projection-matrix
				   viewport near far))
	
	(loop for (p dl) on (3d-cmd-oriented-combinations pipeline-store rm-draw-data dpy) by #'cddr
	      do (render-draw-list-cmds p rm-draw-data dl dpy device command-buffer
					scene
					window
					3d-camera-view-matrix 3d-camera-projection-matrix
					viewport near far))

	(loop for (p dl) on (3d-draw-list-oriented-combinations pipeline-store im-draw-data dpy) by #'cddr
	      do (render-draw-list p im-draw-data dl dpy device command-buffer
				   scene
				   window
				   3d-camera-view-matrix 3d-camera-projection-matrix
				   viewport near far))

	(loop for (p dl) on (3d-cmd-oriented-combinations pipeline-store im-draw-data dpy) by #'cddr
	      do (render-draw-list-cmds p im-draw-data dl dpy device command-buffer
					scene
					window
					3d-camera-view-matrix 3d-camera-projection-matrix
					viewport near far))	

      (values)))))

;; 2d-point
(defun medium-add-2d-point-primitive (medium group model-matrix point-size color x y &optional (object-id 0) (elevation 0))
  "Retained-mode function, returns a handle for a 2d point primitive.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), point-size should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  and x and y must be real numbers.  Dispatches actual work to render thread.  To delete the point, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real x y))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (or mat4 null) model-matrix))
  (declare (type atom group))
  ;; we try to run code which potentially errors outside of render-thread
  ;; the body of rm-dispatch-to-render-thread-with-handle becomes a closure
  (setq color (canonicalize-color color))
  (setq x (clampf x))
  (setq y (clampf y))
  (setq elevation (clampf elevation))
  (setq point-size (clampf point-size))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-2d-point-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) point-size color elevation x y)))

(defun medium-add-2d-point (medium group point-size color x y &optional (object-id 0) (elevation 0))
  "Retained-mode function, returns no values.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, point-size should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  and x and y must be real numbers.  Dispatches actual work to render thread.  To delete the point, you must delete the entire group."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real x y))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (setq x (clampf x))
  (setq y (clampf y))
  (setq elevation (clampf elevation))
  (setq point-size (clampf point-size))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%draw-data-add-2d-point draw-data object-id group point-size color elevation x y)))

(defun medium-draw-2d-point (medium group point-size color x y &optional (object-id 0) (elevation 0))
  "Immediate-mode function, returns no values.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, point-size should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  and x and y must be real numbers.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real x y point-size))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq point-size (clampf point-size))
  (let ((draw-data (im-draw-data medium)))
    (declare (type standard-draw-data draw-data))
    (%draw-data-draw-2d-point draw-data object-id group point-size (canonicalize-color color) (clampf elevation) (clampf x) (clampf y))))

;; 3d-point
(defun medium-add-3d-point-primitive (medium group model-matrix point-size color x y z &optional (object-id 0) (elevation 0))
  "Retained-mode function, returns a handle for a 3d point primitive.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), point-size should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, and x, y and z must be real numbers. Dispatches actual work to render thread.  To delete the point, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real x y z point-size))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (setq x (clampf x))
  (setq y (clampf y))
  (setq z (clampf z))
  (setq elevation (clampf elevation))
  (setq point-size (clampf point-size))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-3d-point-primitive
     draw-data handle object-id group (when model-matrix (mcopy model-matrix)) point-size color elevation x y z)))

(defun medium-add-3d-point (medium group point-size color x y z &optional (object-id 0))
  "Retained-mode function, adds a point to retained-mode draw-lists, returns no values.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, point-size should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer,  and x and y must be real numbers.  Dispatches actual work to render thread.  To delete the point, you must delete the entire group."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real x y z point-size))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (setq x (clampf x))
  (setq y (clampf y))
  (setq z (clampf z))
  (setq point-size (clampf point-size))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%draw-data-add-3d-point draw-data object-id group point-size color x y z)))

(defun medium-draw-3d-point (medium group point-size color x y z &optional (object-id 0))
  "Immediate-mode function, draws a 3d point, returns no values.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, point-size should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer,  and x, y and z must be real numbers.  Performs work in current thread, which should be the render thread."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real x y z))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data medium)))
    (%draw-data-draw-3d-point draw-data object-id group (clampf point-size)
			      (canonicalize-color color) (clampf x) (clampf y) (clampf z))))

;; 2d-line
(defun medium-add-2d-line-primitive (medium group model-matrix line-thickness color x0 y0 x1 y1 &optional (object-id 0) (elevation 0))
  "Retained-mode function, returns a handle for a 2d line primitive.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  x0, y0 and x1, y1  must be real numbers which represent the endpoints of the line.  Dispatches actual work to render thread.  To delete the line segment, you must delete the primitive using the handle."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real x0 y0 x1 y1 line-thickness))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (setq x0 (clampf x0))
  (setq y0 (clampf y0))
  (setq x1 (clampf x1))
  (setq y1 (clampf y1))
  (setq elevation (clampf elevation))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-2d-line-primitive
     draw-data handle object-id group (when model-matrix (mcopy model-matrix)) line-thickness color elevation x0 y0 x1 y1)))

(defun medium-add-2d-line (medium group line-thickness color x0 y0 x1 y1 &optional (object-id 0) (elevation 0))
  "Retained-mode function, adds a 2d line segment to draw lists.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom,  line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  x0, y0 and x1, y1  must be real numbers which represent the endpoints of the line.  Dispatches actual work to render thread.  To delete the line segment, you must delete the entire group."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real x0 y0 x1 y1 line-thickness))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (setq x0 (clampf x0))
  (setq y0 (clampf y0))
  (setq x1 (clampf x1))
  (setq y1 (clampf y1))
  (setq line-thickness (clampf line-thickness))
  (setq elevation (clampf elevation))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%draw-data-add-2d-line draw-data object-id group line-thickness color elevation x0 y0 x1 y1)))

(defun medium-draw-2d-line (medium group line-thickness color x0 y0 x1 y1 &optional (object-id 0) (elevation 0))
  "Immediate-mode function, draws a 2d line segment.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  x0, y0 and x1, y1  must be real numbers which represent the endpoints of the line.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real x0 y0 x1 y1 line-thickness))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data medium)))
    (%draw-data-draw-2d-line draw-data object-id group (clampf line-thickness)
			     (canonicalize-color color) (clampf elevation) (clampf x0) (clampf y0) (clampf x1) (clampf y1))))

;; 3d-line
(defun medium-add-3d-line-primitive (medium group model-matrix line-thickness color x0 y0 z0 x1 y1 z1 &optional (object-id 0))
  "Retained-mode function, returns a handle for a 3d line primitive.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  x0, y0, z0 and x1, y1, z1  must be real numbers which represent the endpoints of the line.  Dispatches actual work to render thread.  To delete the line segment, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real x0 y0 z0 x1 y1 z1 line-thickness))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (setq x0 (clampf x0))
  (setq y0 (clampf y0))
  (setq z0 (clampf z0))
  (setq x1 (clampf x1))
  (setq y1 (clampf y1))
  (setq z1 (clampf z1))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-3d-line-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) line-thickness color x0 y0 z0 x1 y1 z1)))

(defun medium-add-3d-line (medium group line-thickness color x0 y0 z0 x1 y1 z1 &optional (object-id 0))
  "Retained-mode function, adds a 3d line segment to the draw lists.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom,  line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  x0, y0, z0 and x1, y1, z1  must be real numbers which represent the endpoints of the line.  Dispatches actual work to render thread.  To delete the line you must delete the entire group."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real x0 y0 z0 x1 y1 z1 line-thickness))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (setq x0 (clampf x0))
  (setq y0 (clampf y0))
  (setq z0 (clampf z0))
  (setq x1 (clampf x1))
  (setq y1 (clampf y1))
  (setq z1 (clampf z1))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%draw-data-add-3d-line draw-data object-id group line-thickness color x0 y0 z0 x1 y1 z1)))

(defun medium-draw-3d-line (medium group line-thickness color x0 y0 z0 x1 y1 z1 &optional (object-id 0))
  "Retained-mode function, adds a 3d line segment to the draw lists.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom,  line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  x0, y0, z0 and x1, y1, z1  must be real numbers which represent the endpoints of the line.  Dispatches actual work to render thread.  To delete the line you must delete the entire group."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real x0 y0 x0 x1 y1 z1 line-thickness))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data medium)))
    (%draw-data-draw-3d-line draw-data object-id group (clampf line-thickness) (canonicalize-color color)
			     (clampf x0) (clampf y0) (clampf z0)
			     (clampf x1) (clampf y1) (clampf z1))))

;; 2d-polyline
(defun medium-add-2d-polyline-primitive (medium group model-matrix closed? line-thickness color vertices &optional (object-id 0) (elevation 0))
  "Retained-mode function, returns a handle for a 2d polyline primitive.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  vertices should be of the form (list x0 y0 x1 y1 ... xn yn) where the x's and the y's are vertex points of the polyline and must be real numbers.   Dispatches actual work to render thread.  To delete the polyline, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (setq line-thickness (clampf line-thickness))
  (setq elevation (clampf elevation))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-2d-polyline-primitive
     draw-data handle object-id group (when model-matrix (mcopy model-matrix)) closed? line-thickness color elevation vertices)))

(defun medium-add-2d-polyline (medium group closed? line-thickness color vertices &optional (object-id 0) (elevation 0))
  "Retained-mode function, adds a 2d polyline to the draw lists.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom,  closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  vertices should be of the form (list x0 y0 x1 y1 ... xn yn) where the x's and the y's are vertex points of the polyline and must be real numbers.   Dispatches actual work to render thread.  To delete the polyline, you must delete the entire group."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (setq line-thickness (clampf line-thickness))
  (setq elevation (clampf elevation))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%draw-data-add-2d-polyline draw-data object-id group closed? line-thickness color elevation vertices)))

(defun medium-draw-2d-polyline (medium group closed? line-thickness color vertices &optional (object-id 0) (elevation 0))
  "Immediate-mode function, draws a 2d polyline.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom,  closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  vertices should be of the form (list x0 y0 x1 y1 ... xn yn) where the x's and the y's are vertex points of the polyline and must be real numbers.   Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data medium)))
    (%draw-data-draw-2d-polyline
     draw-data object-id group closed? (clampf line-thickness) (canonicalize-color color) (clampf elevation) vertices)))

;; 2d-triangle
(defun medium-add-2d-triangle-primitive (medium group model-matrix line-thickness color x0 y0 x1 y1 x2 y2 &optional (object-id 0) (elevation 0))
  "Retained-mode function, returns a handle for a 2d triangle outline primitive.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity),  line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  x0, y0, x1, y1, x2 and y2 are the three vertex coordinates of the triangle and must be real numbers.   Dispatches actual work to render thread.  To delete the triangle, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real x0 y0 x1 y1 x2 y2 line-thickness))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (setq x0 (clampf x0))
  (setq y0 (clampf y0))
  (setq x1 (clampf x1))
  (setq y1 (clampf y1))
  (setq x2 (clampf x2))
  (setq y2 (clampf y2))
  (setq line-thickness (clampf line-thickness))
  (setq elevation (clampf elevation))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-2d-polyline-primitive draw-data handle object-id group
					  (when model-matrix (mcopy model-matrix)) t line-thickness color elevation
					  (list x0 y0 x1 y1 x2 y2))))

(defun medium-add-2d-triangle (medium group line-thickness color x0 y0 x1 y1 x2 y2 &optional (object-id 0) (elevation 0))
  "Retained-mode function, adds a 2d triangle outline to the draw lists, returns no values.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an a non-null atom,  line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  x0, y0, x1, y1, x2 and y2 are the three vertex coordinates of the triangle and must be real numbers.   Dispatches actual work to render thread.  To delete the triangle, you must delete the entire group."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real x0 y0 x1 y1 x2 y2 line-thickness))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (setq x0 (clampf x0))
  (setq y0 (clampf y0))
  (setq x1 (clampf x1))
  (setq y1 (clampf y1))
  (setq x2 (clampf x2))
  (setq y2 (clampf y2))
  (setq line-thickness (clampf line-thickness))
  (setq elevation (clampf elevation))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%draw-data-add-2d-polyline draw-data object-id group t line-thickness color elevation
				(list x0 y0 x1 y1 x2 y2))))

(defun medium-draw-2d-triangle (medium group line-thickness color x0 y0 x1 y1 x2 y2 &optional (object-id 0) (elevation 0))
  "Immediate-mode function, draws a 2d triangle outline, returns no values.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an a non-null atom,  line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  x0, y0, x1, y1, x2 and y2 are the three vertex coordinates of the triangle and must be real numbers.   Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real x0 y0 x1 y1 x2 y2 line-thickness))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data medium)))
    (%draw-data-draw-2d-polyline draw-data object-id group t (clampf line-thickness) (canonicalize-color color) (clampf elevation)
				 (list x0 y0 x1 y1 x2 y2))))

;; 2d-rectangle
(defun medium-add-2d-rectangle-primitive (medium group model-matrix line-thickness color x0 y0 x1 y1 &optional (object-id 0) (elevation 0))
  "Retained-mode function, returns a handle for a 2d rectangle outline primitive.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity),  line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  x0, y0, x1, and y1 are the top-left and bottom-right corners of the rectangle and must be real numbers.   Dispatches actual work to render thread.  To delete the rectangle, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real x0 y0 x1 y1 line-thickness))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (setq x0 (clampf x0))
  (setq y0 (clampf y0))
  (setq x1 (clampf x1))
  (setq y1 (clampf y1))
  (setq line-thickness (clampf line-thickness))
  (setq elevation (clampf elevation))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-2d-polyline-primitive
     draw-data handle object-id group (when model-matrix (mcopy model-matrix)) t line-thickness color elevation
     (list x0 y0 x0 y1 x1 y1 x1 y0))))


(defun medium-add-2d-rectangle (medium group line-thickness color x0 y0 x1 y1 &optional (object-id 0) (elevation 0))
  "Retained-mode function, adds a 2d rectangle outline to the draw lists, returns no values.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an a non-null atom,  line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  x0, y0, x1, and y1 are the top-left and bottom-right corners of the rectangle and must be real numbers.   Dispatches actual work to render thread.  To delete the rectangle, you must delete the entire group."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real x0 y0 x1 y1 line-thickness))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (setq x0 (clampf x0))
  (setq y0 (clampf y0))
  (setq x1 (clampf x1))
  (setq y1 (clampf y1))
  (setq line-thickness (clampf line-thickness))
  (setq elevation (clampf elevation))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%draw-data-add-2d-polyline draw-data object-id group t line-thickness color elevation
				(list x0 y0 x0 y1 x1 y1 x1 y0))))

(defun medium-draw-2d-rectangle (medium group line-thickness color x0 y0 x1 y1 &optional (object-id 0) (elevation 0))
  "Immediate-mode function, draws a 2d rectangle outline, returns no values.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an a non-null atom,  line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  x0, y0, x1, and y1 are the top-left and bottom-right corners of the rectangle and must be real numbers.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real x0 y0 x1 y1 line-thickness))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data medium)))
    (setq x0 (clampf x0))
    (setq y0 (clampf y0))
    (setq x1 (clampf x1))
    (setq y1 (clampf y1))
    (%draw-data-draw-2d-polyline draw-data object-id group t (clampf line-thickness) (canonicalize-color color) (clampf elevation)
				 (list x0 y0 x0 y1 x1 y1 x1 y0))))

;; multicolor-2d-polyline
(defun medium-add-multicolor-2d-polyline-primitive (medium group model-matrix closed? line-thickness vertices &optional (object-id 0) (elevation 0))
  "Retained-mode function, returns a handle for a multicolored 2d polyline primitive.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number.  vertices should be of the form (list x0 y0 color0 x1 y1 color1 ... xn yn colorn) where the x's and the y's are vertex points of the polyline and must be real numbers, color values can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.   Dispatches actual work to render thread.  To delete the polyline, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq line-thickness (clampf line-thickness))
  (setq elevation (clampf elevation))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-multicolor-2d-polyline-primitive
     draw-data handle object-id group (when model-matrix (mcopy model-matrix)) closed? line-thickness elevation vertices)))

(defun medium-add-multicolor-2d-polyline (medium group closed? line-thickness vertices &optional (object-id 0) (elevation 0))
  "Retained-mode function, adds a multicolored 2d polyline to the draw lists, returns no values.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom,  closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number.  vertices should be of the form (list x0 y0 color0 x1 y1 color1 ... xn yn colorn) where the x's and the y's are vertex points of the polyline and must be real numbers, color values can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.   Dispatches actual work to render thread.  To delete the polyline, you must delete the entire group."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (declare (type (and atom t) group))
  (setq line-thickness (clampf line-thickness))
  (setq elevation (clampf elevation))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%draw-data-add-multicolor-2d-polyline draw-data object-id group closed? line-thickness elevation vertices)))

(defun medium-draw-multicolor-2d-polyline (medium group closed? line-thickness vertices &optional (object-id 0) (elevation 0))
  "Immediate-mode function, draws a multicolored 2d polyline, returns no values.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom,  closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number.  vertices should be of the form (list x0 y0 color0 x1 y1 color1 ... xn yn colorn) where the x's and the y's are vertex points of the polyline and must be real numbers, color values can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data medium)))
    (%draw-data-draw-multicolor-2d-polyline draw-data object-id group closed? (clampf line-thickness) (clampf elevation) vertices)))

(defun medium-add-multicolor-2d-instanced-line-primitive
    (medium group model-matrix closed? line-thickness vertices &optional (object-id 0) (elevation 0))
  "Retained-mode function, returns a handle for a multicolored 2d polyline primitive.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number.  vertices should be of the form (list x0 y0 color0 x1 y1 color1 ... xn yn colorn) where the x's and the y's are vertex points of the polyline and must be real numbers, color values can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.   Dispatches actual work to render thread.  To delete the polyline, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq line-thickness (clampf line-thickness))
  (setq elevation (clampf elevation))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-multicolor-2d-instanced-line-primitive
     draw-data handle object-id group (when model-matrix (mcopy model-matrix)) closed? line-thickness elevation vertices)))

(defun medium-add-filled-3d-instanced-tube-primitive
    (medium group model-matrix closed? line-thickness color vertices &optional (object-id 0))
  "Retained-mode function, returns a handle for a multicolored 2d polyline primitive.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number.  vertices should be of the form (list x0 y0 color0 x1 y1 color1 ... xn yn colorn) where the x's and the y's are vertex points of the polyline and must be real numbers, color values can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.   Dispatches actual work to render thread.  To delete the polyline, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq line-thickness (clampf line-thickness))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-filled-3d-instanced-tube-primitive
     draw-data handle object-id group (when model-matrix (mcopy model-matrix)) closed? line-thickness color vertices)))

;; 2d-circular-arc
(defun medium-add-2d-circular-arc-primitive (medium group model-matrix closed? line-thickness color
                                            center-x center-y radius start-angle end-angle
                                            number-of-segments &optional (object-id 0) (elevation 0))
  "Retained-mode function, returns a handle for a 2d circular arc outline primitive.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity),  closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number.  color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer. center-x, and center-y must be real numbers, radius must be a positive real number, start-angle and end-angle are real numbers, measured in radians.  number-of-segments must be a positive integer, and defaults to 64.  Dispatches actual work to render thread.  To delete the arc, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type boolean closed?))
  (declare (type real center-x center-y radius start-angle end-angle line-thickness))
  (declare (type (integer 1 #.most-positive-fixnum) number-of-segments))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq center-x (coerce center-x 'double-float))
  (setq center-y (coerce center-y 'double-float))
  (setq radius (coerce radius 'double-float))
  (setq start-angle (coerce start-angle 'double-float))
  (setq end-angle (coerce end-angle 'double-float))
  (setq color (canonicalize-color color))
  (setq line-thickness (clampf line-thickness))
  (setq elevation (clampf elevation))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-2d-circular-arc-primitive draw-data handle object-id group
					      (when model-matrix (mcopy model-matrix)) closed? line-thickness color elevation
					      center-x center-y radius start-angle end-angle
					      number-of-segments)))

(defun medium-add-2d-circular-arc (medium group closed? line-thickness color
				  center-x center-y radius start-angle end-angle
				  number-of-segments &optional (object-id 0) (elevation 0))
  "Retained-mode function, adds a 2d circular arc outline to the draw lists, returns no values.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number.  color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer. center-x, and center-y must be real numbers, radius must be a positive real number, start-angle and end-angle are real numbers, measured in radians.  number-of-segments must be a positive integer, and defaults to 64.  Dispatches actual work to render thread.  To delete the arc, you must delete the entire group."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type boolean closed?))
  (declare (type real center-x center-y radius start-angle end-angle line-thickness))
  (declare (type (integer 1 #.most-positive-fixnum) number-of-segments))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq center-x (coerce center-x 'double-float))
  (setq center-y (coerce center-y 'double-float))
  (setq radius (coerce radius 'double-float))
  (setq start-angle (coerce start-angle 'double-float))
  (setq end-angle (coerce end-angle 'double-float))
  (setq color (canonicalize-color color))
  (setq line-thickness (clampf line-thickness))
  (setq elevation (clampf elevation))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%draw-data-add-2d-circular-arc draw-data object-id group closed? line-thickness color elevation
				    center-x center-y radius start-angle end-angle
				    number-of-segments)))

(defun medium-draw-2d-circular-arc (medium group closed? line-thickness color
				   center-x center-y radius start-angle end-angle
                                   number-of-segments &optional (object-id 0) (elevation 0))
  "Immediate-mode function, draws 2d circular arc outline, returns no values.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number.  color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer. center-x, and center-y must be real numbers, radius must be a positive real number, start-angle and end-angle are real numbers, measured in radians.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type boolean closed?))
  (declare (type real center-x center-y radius start-angle end-angle))
  (declare (type (integer 1 #.most-positive-fixnum) number-of-segments))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data medium)))
    (%draw-data-draw-2d-circular-arc draw-data object-id group closed? (clampf line-thickness) (canonicalize-color color) (clampf elevation)
				     (coerce center-x 'double-float) (coerce center-y 'double-float)
				     (coerce radius 'double-float)
				     (coerce start-angle 'double-float) (coerce end-angle 'double-float)
				     number-of-segments)))

;; 2d-circle
(defun medium-add-2d-circle-primitive (medium group model-matrix line-thickness color
                                      center-x center-y radius
                                      number-of-segments &optional (object-id 0) (elevation 0))
  "Retained-mode function, returns a handle for a 2d circle outline primitive.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity),  line-thickness should be a positive real number.  color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer. center-x, and center-y must be real numbers, radius must be a positive real number.  number-of-segments must be a positive integer, and defaults to 64. Dispatches actual work to render thread.  To delete the arc, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real center-x center-y radius line-thickness))
  (declare (type (integer 1 #.most-positive-fixnum) number-of-segments))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (setq center-x (coerce center-x 'double-float))
  (setq center-y (coerce center-y 'double-float))
  (setq radius (coerce radius 'double-float))
  (setq line-thickness (clampf line-thickness))
  (setq elevation (clampf elevation))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-2d-circle-primitive draw-data handle object-id group
					(when model-matrix (mcopy model-matrix)) line-thickness color elevation
					center-x center-y radius
					number-of-segments)))

(defun medium-add-2d-circle (medium group line-thickness color
			    center-x center-y radius
			    number-of-segments &optional (object-id 0) (elevation 0))
  "Retained-mode function, adds a 2d circle outline to the draw lists, returns no values.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, line-thickness should be a positive real number.  color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer. center-x, and center-y must be real numbers, radius must be a positive real number.  number-of-segments must be a positive integer, and defaults to 64.  Dispatches actual work to render thread.  To delete the arc, you must delete the entire group."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real center-x center-y radius line-thickness))
  (declare (type (integer 1 #.most-positive-fixnum) number-of-segments))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (setq center-x (coerce center-x 'double-float))
  (setq center-y (coerce center-y 'double-float))
  (setq radius (coerce radius 'double-float))
  (setq line-thickness (clampf line-thickness))
  (setq elevation (clampf elevation))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%draw-data-add-2d-circle draw-data object-id
			      group line-thickness color elevation
			      center-x center-y radius
			      number-of-segments)))

(defun medium-draw-2d-circle (medium group line-thickness color
			     center-x center-y radius
                             number-of-segments &optional (object-id 0) (elevation 0))
  "Immediate-mode function, draws a 2d circle outline, returns no values.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, line-thickness should be a positive real number.  color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer. center-x, and center-y must be real numbers, radius must be a positive real number.  number-of-segments must be a positive integer, and defaults to 64.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real center-x center-y radius))
  (declare (type (integer 1 #.most-positive-fixnum) number-of-segments))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data medium)))
    (%draw-data-draw-2d-circle draw-data object-id group (clampf line-thickness) (canonicalize-color color) (clampf elevation)
			       (coerce center-x 'double-float) (coerce center-y 'double-float)
			       (coerce radius 'double-float)
			       number-of-segments)))

;; 3d-polyline
(defun medium-add-3d-polyline-primitive (medium group model-matrix closed? line-thickness color vertices &optional (object-id 0))
  "Retained-mode function, returns a handle for a 3d polyline primitive.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  vertices should be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn) where the x, y and z's are the vertex points of the polyline and must be real numbers.   Dispatches actual work to render thread.  To delete the polyline, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real line-thickness))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-3d-polyline-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) closed? line-thickness color vertices)))

(defun medium-add-3d-polyline (medium group closed? line-thickness color vertices &optional (object-id 0))
  "Retained-mode function, adds a 3d polyline to the draw lists.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom,  closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  vertices should be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn) where the x, y and z's are the vertex points of the polyline and must be real numbers.   Dispatches actual work to render thread.  To delete the polyline, you must delete the entire group."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real line-thickness))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%draw-data-add-3d-polyline draw-data object-id group closed? line-thickness color vertices)))

(defun medium-draw-3d-polyline (medium group closed? line-thickness color vertices &optional (object-id 0))
  "Immediate-mode function, draws a 3d polyline.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom,  closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  vertices should be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn) where the x, y and z's are the vertex points of the polyline and must be real numbers.   Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data medium)))
    (%draw-data-draw-3d-polyline draw-data object-id group closed? (clampf line-thickness) (canonicalize-color color) vertices)))

;; multicolor-3d-polyline
(defun medium-add-multicolor-3d-polyline-primitive (medium group model-matrix closed? line-thickness vertices &optional (object-id 0))
  "Retained-mode function, returns a handle for a multicolored 3d polyline primitive.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number.  vertices should be of the form (list x0 y0 z0 color0 x1 y1 z1 color1 ... xn yn zn colorn) where the x, y and z's are vertex points of the polyline and must be real numbers, color values can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.   Dispatches actual work to render thread.  To delete the polyline, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-multicolor-3d-polyline-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) closed? line-thickness vertices)))

(defun medium-add-multicolor-3d-polyline (medium group closed? line-thickness vertices &optional (object-id 0))
  "Retained-mode function, adds a multicolored 3d polyline to the draw lists, returns no values.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom,  closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number.  vertices should be of the form (list x0 y0 z0 color0 x1 y1 z1 color1 ... xn yn zn colorn) where the x, y and z's are vertex points of the polyline and must be real numbers, color values can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.   Dispatches actual work to render thread.  To delete the polyline, you must delete the entire group."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%draw-data-add-multicolor-3d-polyline draw-data object-id group closed? line-thickness vertices)))

(defun medium-draw-multicolor-3d-polyline (medium group closed? line-thickness vertices &optional (object-id 0))
  "Immediate-mode function, draws a multicolored 3d polyline, returns no values.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom,  closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number.  vertices should be of the form (list x0 y0 z0 color0 x1 y1 z1 color1 ... xn yn zn colorn) where the x, y and z's are vertex points of the polyline and must be real numbers, color values can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data medium)))
    (%draw-data-draw-multicolor-3d-polyline draw-data object-id group closed? (clampf line-thickness) vertices)))

;; filled-2d-triangle-list
(defun medium-add-filled-2d-triangle-list-primitive (medium group model-matrix color vertices &optional (object-id 0) (elevation 0))
  "Retained-mode function, returns a handle for a filled 2d triangle list primitive.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity),   color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00  x10 y10 x20 y20 x01 y01 x11 y11 x21 y21 ... x0n y0n x1n y1n x2n y2n) where the x and y values represent vertices of a triangle in a series of triangles and must be real numbers,    Dispatches actual work to render thread.  To delete the triangle list, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (setq elevation (clampf elevation))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-filled-2d-triangle-list-primitive
     draw-data handle object-id group (when model-matrix (mcopy model-matrix)) color elevation vertices)))

(defun medium-add-filled-2d-triangle-list (medium group color vertices &optional (object-id 0) (elevation 0))
  "Retained-mode function, adds a filled 2d triangle list to the draw-lists.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00  x10 y10 x20 y20 x01 y01 x11 y11 x21 y21 ... x0n y0n x1n y1n x2n y2n) where the x and y values represent vertices of a triangle in a series of triangles and must be real numbers,    Dispatches actual work to render thread.  To delete the triangle list, you must delete the entire group."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (setq elevation (clampf elevation))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%draw-data-add-filled-2d-triangle-list draw-data object-id group color elevation vertices)))

(defun medium-draw-filled-2d-triangle-list (medium group color vertices &optional (object-id 0) (elevation 0))
  "Immediate-mode function, draws a filled 2d triangle list.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00  x10 y10 x20 y20 x01 y01 x11 y11 x21 y21 ... x0n y0n x1n y1n x2n y2n) where the x and y values represent vertices of a triangle in a series of triangles and must be real numbers,   Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data medium)))
    (%draw-data-draw-filled-2d-triangle-list draw-data object-id group (canonicalize-color color) (clampf elevation) vertices)))

;; filled-2d-triangle-strip
(defun medium-add-filled-2d-triangle-strip-primitive (medium group model-matrix color vertices &optional (object-id 0) (elevation 0))
  "Retained-mode function, returns a handle for a filled 2d triangle strip primitive.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity),   color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x0 y0  x1 y1 ... xn yn) where the x and y values represent successive vertices of a triangle strip and must be real numbers,    Dispatches actual work to render thread.  To delete the triangle strip, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (setq elevation (clampf elevation))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-filled-2d-triangle-strip-primitive
     draw-data handle object-id group (when model-matrix (mcopy model-matrix)) color elevation vertices)))

(defun medium-draw-filled-2d-triangle-strip (medium group color vertices &optional (object-id 0) (elevation 0))
  "Immediate-mode function, draws a filled 2d triangle strip.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom,    color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x0 y0  x1 y1 ... xn yn) where the x and y values represent successive vertices of a triangle strip and must be real numbers.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data medium)))
    (declare (type immediate-mode-draw-data draw-data))
    (let ((draw-list (draw-data-2d-triangle-strip-draw-list draw-data)))
      ;; we add the primitive/cmd without a handle:
      (%draw-list-add-filled-2d-triangle-strip/list draw-list object-id group nil (canonicalize-color color) (clampf elevation) vertices))))

;; filled-2d-rectangle-list
(defun medium-add-filled-2d-rectangle-list-primitive (medium group model-matrix color vertices &optional (object-id 0) (elevation 0))
  "Retained-mode function, returns a handle for a filled 2d rectangle list primitive.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00 x10 y10 x10 y10 x11 y11 ... x0n y0n x1n y1n) where  each pair of successive x and y's represent the top-left corner followed by the bottom-right corner of each rectangle and must be real numbers.  Dispatches actual work to render thread.  To delete the rectangle list, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (setq elevation (clampf elevation))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-filled-2d-rectangle-list-primitive
     draw-data handle object-id group (when model-matrix (mcopy model-matrix)) color elevation vertices)))

(defun medium-add-filled-2d-rectangle-list (medium group color vertices &optional (object-id 0) (elevation 0))
  "Retained-mode function, adds a  filled 2d rectangle list to the draw-lists.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00 x10 y10 x10 y10 x11 y11 ... x0n y0n x1n y1n) where  each pair of successive x and y's represent the top-left corner followed by the bottom-right corner of each rectangle and must be real numbers.  Dispatches actual work to render thread.  To delete the rectangle list, you must delete the entire group."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (setq elevation (clampf elevation))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%draw-data-add-filled-2d-rectangle-list draw-data object-id group color elevation vertices)))

(defun medium-draw-filled-2d-rectangle-list (medium group color vertices &optional (object-id 0) (elevation 0))
  "Immediate-mode function, draws a  filled 2d rectangle list.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00 x10 y10 x10 y10 x11 y11 ... x0n y0n x1n y1n) where  each pair of successive x and y's represent the top-left corner followed by the bottom-right corner of each rectangle and must be real numbers.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data medium)))
    (%draw-data-draw-filled-2d-rectangle-list draw-data object-id group (canonicalize-color color) (clampf elevation) vertices)))

;; textured-2d-rectangle-list
(defun medium-add-textured-2d-rectangle-list-primitive (medium group model-matrix texture color vertices &optional (object-id 0) (elevation 0))
  "Retained-mode function, returns a handle for a textured 2d rectangle list primitive.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), texture should be a texture such as return from make-vulkan-texture, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00 u00 v00 x10 y10 u10 v10 x01 y01 u01 v01 x11 y11 u11 v11 ... x0n y0n u0n v0n x1n y1n u1n v1n) where  each pair of successive x, y, u and v represent the top-left corner followed by the bottom-right corner of each rectangle with their normalized texture coordinates, and must be real numbers.  There must be at least one pair of the sequence x, y, u, v.  Dispatches actual work to render thread.  To delete the rectangle list, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (setq elevation (clampf elevation))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-textured-2d-rectangle-list-primitive
     draw-data handle object-id group (when model-matrix (mcopy model-matrix)) texture color elevation vertices)))

(defun medium-add-textured-2d-rectangle-list (medium group texture color vertices &optional (object-id 0) (elevation 0))
  "Retained-mode function, adds a textured 2d rectangle list to the draw-lists.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, texture should be a texture such as return from make-vulkan-texture, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00 u00 v00 x10 y10 u10 v10 x01 y01 u01 v01 x11 y11 u11 v11 ... x0n y0n u0n v0n x1n y1n u1n v1n) where  each pair of successive x, y, u and v represent the top-left corner followed by the bottom-right corner of each rectangle with their normalized texture coordinates, and must be real numbers.  There must be at least one pair of the sequence x, y, u, v.  Dispatches actual work to render thread.  To delete the rectangle list, you must delete the entire group."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (setq elevation (clampf elevation))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%draw-data-add-textured-2d-rectangle-list draw-data object-id group texture color elevation vertices)))

(defun medium-draw-textured-2d-rectangle-list (medium group texture color vertices &optional (object-id 0) (elevation 0))
  "Immediate-mode function, draws a textured 2d rectangle list.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, texture should be a texture such as return from make-vulkan-texture, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00 u00 v00 x10 y10 u10 v10 x01 y01 u01 v01 x11 y11 u11 v11 ... x0n y0n u0n v0n x1n y1n u1n v1n) where  each pair of successive x, y, u and v represent the top-left corner followed by the bottom-right corner of each rectangle with their normalized texture coordinates, and must be real numbers.  There must be at least one pair of the sequence x, y, u, v.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (%draw-data-draw-textured-2d-rectangle-list
   (im-draw-data medium) object-id group texture (canonicalize-color color) (clampf elevation) vertices))

;; filled-2d-convex-polygon
(defun medium-add-filled-2d-convex-polygon-primitive (medium group model-matrix color vertices &optional (object-id 0) (elevation 0))
  "Retained-mode function, returns a handle for a filled 2d convex polygon primitive.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x0 y0 x1 y1 ... xn yn)  where each successive x and y are the vertices of the polygon, and must be real numbers.  There must be at least three x, y pairs in vertices.  Dispatches actual work to render thread.  To delete the polygon, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (setq elevation (clampf elevation))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-filled-2d-convex-polygon-primitive
     draw-data handle object-id group (when model-matrix (mcopy model-matrix)) color elevation vertices)))

(defun medium-add-filled-2d-convex-polygon (medium group color vertices &optional (object-id 0) (elevation 0))
  "Retained-mode function, adds a filled 2d convex polygon to the draw-lists, returns no values.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x0 y0 x1 y1 ... xn yn)  where each successive x and y are the vertices of the polygon, and must be real numbers.  There must be at least three x, y pairs in vertices.  Dispatches actual work to render thread.  To delete the polygon, you must delete the entire group."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (setq elevation (clampf elevation))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%draw-data-add-filled-2d-convex-polygon draw-data object-id group color elevation vertices)))

(defun medium-draw-filled-2d-convex-polygon (medium group color vertices &optional (object-id 0) (elevation 0))
  "Immediate-mode function, draws a filled 2d convex polygon, returns no values.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x0 y0 x1 y1 ... xn yn)  where each successive x and y are the vertices of the polygon, and must be real numbers.  There must be at least three x, y pairs in vertices.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (%draw-data-draw-filled-2d-convex-polygon (im-draw-data medium) object-id group (canonicalize-color color) (clampf elevation) vertices))

;; filled-2d-circle
(declaim (inline compute-circle-vertices))
(defun compute-circle-vertices (fixnum-number-of-segments df-center-x df-center-y df-radius)
  (declare (optimize (speed 3) (safety 0) (debug 3) (compilation-speed 3) (space 0)))
  (declare (type fixnum fixnum-number-of-segments))
  (declare (type double-float df-center-x df-center-y df-radius))
  (let ((theta 0.0d0)
	(step (/ 2pi fixnum-number-of-segments))
	(verts ()))
    (declare (type double-float theta step))
    (declare (type list verts))
    (loop for i from 0 to fixnum-number-of-segments
	  do (let* ((coord-x (clampf (+ df-center-x (* df-radius (cos theta)))))
		    (coord-y (clampf (+ df-center-y (* df-radius (sin theta))))))
	       (push coord-x verts)
	       (push coord-y verts)	       
	       (incf theta step))
	  finally (return (nreverse verts)))))

(defun medium-add-filled-2d-circle-primitive (medium group model-matrix color
                                             center-x center-y radius
					     number-of-sectors &optional (object-id 0) (elevation 0))
  "Retained-mode function, returns a handle for a filled 2d circle primitive.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, center-x and center-y should be real numbers, radius should be a positive real number, number-of-sectors defaults to 64.  Dispatches actual work to render thread.  To delete the circle, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real center-x center-y radius))
  (declare (type (integer 1 #.most-positive-fixnum) number-of-sectors))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq center-x (coerce center-x 'double-float))
  (setq center-y (coerce center-y 'double-float))
  (setq radius (coerce radius 'double-float))
  (setq color (canonicalize-color color))
  (setq elevation (clampf elevation))
  (let ((vertices (compute-circle-vertices number-of-sectors center-x center-y radius)))
    (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
      (%draw-data-add-filled-2d-convex-polygon-primitive
       draw-data handle object-id group (when model-matrix (mcopy model-matrix)) color elevation vertices))))

(defun medium-add-filled-2d-circle (medium group color
				   center-x center-y radius
				   number-of-sectors &optional (object-id 0) (elevation 0))
  "Retained-mode function, adds a filled 2d circle to the draw lists, returns no values.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, center-x and center-y should be real numbers, radius should be a positive real number, number-of-sectors defaults to 64.  Dispatches actual work to render thread.  To delete the circle, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real center-x center-y radius))
  (declare (type (integer 1 #.most-positive-fixnum) number-of-sectors))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq center-x (coerce center-x 'double-float))
  (setq center-y (coerce center-y 'double-float))
  (setq radius (coerce radius 'double-float))
  (setq elevation (clampf elevation))
  (let ((vertices (compute-circle-vertices number-of-sectors center-x center-y radius)))
    (rm-dispatch-to-render-thread (medium draw-data)
      (%draw-data-add-filled-2d-convex-polygon draw-data object-id group color elevation vertices))))

(defun medium-draw-filled-2d-circle (medium color group
				    center-x center-y radius
				    number-of-segments &optional (object-id 0) (elevation 0))
  "Immediate-mode function, draws a filled 2d circle.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, center-x and center-y should be real numbers, radius should be a positive real number, number-of-sectors defaults to 64.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real center-x center-y radius))
  (declare (type (integer 1 #.most-positive-fixnum) number-of-segments))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq center-x (coerce center-x 'double-float))
  (setq center-y (coerce center-y 'double-float))
  (setq radius (coerce radius 'double-float))
  (let ((vertices (compute-circle-vertices number-of-segments center-x center-y radius)))
    (%draw-data-draw-filled-2d-convex-polygon (im-draw-data medium) object-id group (canonicalize-color color) (clampf elevation) vertices)))

;; filled-3d-triangle-list-flat
(defun medium-add-filled-3d-triangle-list-primitive-flat (medium group model-matrix color vertices &optional (object-id 0))
  "Retained-mode function, returns a handle for a filled 3d triangle list primitive.  Displays with flat shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00 z00 x10 y10 z10 x20 y20 z20 x01 y01 z01 x11 y11 z11 x21 y21 z21... x0n y0n z0n x1n y1n z1n x2n y2n z2n) where the x, y and z values represent vertices of a triangle in a series of triangles and must be real numbers.  There must be at least three sets of x, y and z, and additional vertices come as 3 sets each.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the triangle list, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-filled-3d-triangle-list-primitive
     draw-data handle object-id group (when model-matrix (mcopy model-matrix)) color vertices)))

(defun medium-add-filled-3d-triangle-list-flat (medium group color vertices &optional (object-id 0))
  "Retained-mode function, adds a filled 3d triangle list to the draw-lists, returns no values.  Displays with flat shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom,    color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00  z00 x10 y10 z10 x20 y20 z20 x01 y01 z01 x11 y11 z11 x21 y21 z21... x0n y0n z0n x1n y1n z1n x2n y2n z2n) where the x, y and z values represent vertices of a triangle in a series of triangles and must be real numbers.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the triangle list, you must delete the entire group."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%draw-data-add-filled-3d-triangle-list draw-data object-id group color vertices)))

(defun medium-draw-filled-3d-triangle-list-flat (medium group color vertices &optional (object-id 0))
  "Retained-mode function, adds a filled 3d triangle list to the draw-lists, returns no values.  Displays with flat shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom,    color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00  z00 x10 y10 z10 x20 y20 z20 x01 y01 z01 x11 y11 z11 x21 y21 z21... x0n y0n z0n x1n y1n z1n x2n y2n z2n) where the x, y and z values represent vertices of a triangle in a series of triangles and must be real numbers.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the triangle list, you must delete the entire group."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (%draw-data-draw-filled-3d-triangle-list (im-draw-data medium) object-id group (canonicalize-color color) vertices))

;; filled-3d-triangle-list-diffuse
(defun medium-add-filled-3d-triangle-list-primitive-diffuse (medium group model-matrix color vertices material &optional (object-id 0))
  "Retained-mode function, returns a handle for a filled 3d triangle list primitive.  Displays with diffuse shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00 z00 x10 y10 z10 x20 y20 z20 x01 y01 z01 x11 y11 z11 x21 y21 z21... x0n y0n z0n x1n y1n z1n x2n y2n z2n) where the x, y and z values represent vertices of a triangle in a series of triangles and must be real numbers.  There must be at least three sets of x, y and z, and additional vertices come as 3 sets each.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the triangle list, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (declare (type (or null material-mixin) material))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-filled-3d-triangle-list-with-normals-primitive
     draw-data handle object-id group (when model-matrix (mcopy model-matrix)) color vertices material)))

(defun medium-add-filled-3d-triangle-list-diffuse (medium group color vertices &optional (object-id 0))
  "Retained-mode function, adds a filled 3d triangle list to the draw-lists, returns no values.  Displays with diffuse shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00  z00 x10 y10 z10 x20 y20 z20 x01 y01 z01 x11 y11 z11 x21 y21 z21... x0n y0n z0n x1n y1n z1n x2n y2n z2n) where the x, y and z values represent vertices of a triangle in a series of triangles and must be real numbers.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the triangle list, you must delete the entire group."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%draw-data-add-filled-3d-triangle-list-with-normals draw-data object-id group color vertices)))

(defun medium-draw-filled-3d-triangle-list-diffuse (medium group color vertices &optional (object-id 0))
  "Immediate-mode function, draws a filled 3d triangle list, returns no values.  Displays with diffuse shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00  z00 x10 y10 z10 x20 y20 z20 x01 y01 z01 x11 y11 z11 x21 y21 z21... x0n y0n z0n x1n y1n z1n x2n y2n z2n) where the x, y and z values represent vertices of a triangle in a series of triangles and must be real numbers.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (%draw-data-draw-filled-3d-triangle-list-with-normals (im-draw-data medium) object-id group (canonicalize-color color) vertices))

;; filled-3d-triangle-strip-flat
(defun medium-add-filled-3d-triangle-strip-primitive-flat (medium group model-matrix color vertices &optional (object-id 0))
  "Retained-mode function, returns a handle for a filled 3d triangle strip primitive.  Displays with flat shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn) where the x, y and z values represent successive vertices of a triangle strip and must be real numbers.  There must be at least three vertices.  Dispatches actual work to render thread.  To delete the triangle strip, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-filled-3d-triangle-strip-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) color vertices)))

;; triangle strips do not have pseudo-cmds and therefore will
;; not have medium-add-filled-3d-triangle-strip-flat

(defun medium-draw-filled-3d-triangle-strip-flat (medium group color vertices &optional (object-id 0))
  "Immediate-mode function, draws a filled 3d triangle strip, returns no values.  Displays with flat shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn) where the x, y and z values represent successive vertices of a triangle strip and must be real numbers.  There must be at least 3 vertices.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data medium)))
    (declare (type immediate-mode-draw-data draw-data))
    (let ((draw-list (draw-data-3d-triangle-strip-draw-list draw-data)))
      ;; we add the primitive/cmd without a handle:
      (%draw-list-add-filled-3d-triangle-strip/list
       draw-list object-id group nil (canonicalize-color color) vertices))))

;; filled-3d-triangle-strip-diffuse
(defun medium-add-filled-3d-triangle-strip-primitive-diffuse (medium group model-matrix color vertices material &optional (object-id 0))
  "Retained-mode function, returns a handle for a filled 3d triangle strip primitive.  Displays with diffuse shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity),   color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x0 y0 z0  x1 y1 z1 ... xn yn zn) where the x, y and z values represent successive vertices of a triangle strip and must be real numbers.  There must be at least three vertices.  light-position must either be a 3d-vectors:vec4 or null.  Dispatches actual work to render thread.  To delete the triangle strip, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (declare (type (or null material-mixin) material))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-filled-3d-triangle-strip-with-normals-primitive
     draw-data handle object-id group (when model-matrix (mcopy model-matrix)) color vertices material)))

(defun medium-draw-filled-3d-triangle-strip-diffuse (medium group color vertices &optional (object-id 0))
  "Retained-mode function, returns a handle for a filled 3d triangle strip primitive.  Displays with diffuse shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x0 y0 z0  x1 y1 z1 ... xn yn zn) where the x, y and z values represent successive vertices of a triangle strip and must be real numbers.  There must be at least three vertices.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data medium)))
    (declare (type immediate-mode-draw-data draw-data))
    (let ((draw-list (draw-data-3d-triangle-strip-with-normals-draw-list draw-data)))
      ;; we add the primitive/cmd without a handle:
      (%draw-list-add-filled-3d-triangle-strip/list-with-normals
       draw-list object-id group nil (canonicalize-color color) vertices nil))))

;; filled-3d-convex-polygon-diffuse
(defun medium-add-filled-3d-convex-polygon-primitive-diffuse (medium group model-matrix color vertices material &optional (object-id 0))
  "Retained-mode function, returns a handle for a filled 3d convex polygon primitive.  Displays with diffuse shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer  vertices must be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn)  where each successive x, y and z are the vertices of the polygon, and must be real numbers.  There must be at least three x, y, z triplets in vertices.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the polygon, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (declare (type (or null material-mixin) material))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-filled-3d-convex-polygon-with-normals-primitive
     draw-data handle object-id group (when model-matrix (mcopy model-matrix)) color vertices material)))

(defun medium-add-filled-3d-convex-polygon-diffuse (medium group color vertices &optional (object-id 0))
  "Retained-mode function, adds a filled 3d convex polygon to the draw-lists, returns no values.  Displays with diffuse shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn)  where each successive x, y and z are the vertices of the polygon, and must be real numbers.  There must be at least three x, y, z triplets in vertices.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the polygon, you must delete the entire group."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%draw-data-add-filled-3d-convex-polygon-with-normals draw-data object-id group color vertices)))

(defun medium-draw-filled-3d-convex-polygon-diffuse (medium group color vertices &optional (object-id 0))
  "Immediate-mode function, draws a filled 3d convex polygon, returns no values.  Displays with diffuse shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn)  where each successive x, y and z are the vertices of the polygon, and must be real numbers.  There must be at least three x, y, z triplets in vertices.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data medium)))
    (%draw-data-draw-filled-3d-convex-polygon-with-normals draw-data object-id group (canonicalize-color color) vertices)))

;; filled-3d-convex-polygon-flat
(defun medium-add-filled-3d-convex-polygon-primitive-flat (medium group model-matrix color vertices &optional (object-id 0))
  "Retained-mode function, returns a handle for a filled 3d convex polygon primitive.  Displays with flat shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer,  vertices should be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn) where the x, y and z's represent a vertex of the polygon and must be real numbers.  There must be at least three x, y, z triplets in vertices.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the polygon, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-filled-3d-convex-polygon-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) color vertices)))

(defun medium-add-filled-3d-convex-polygon-flat (medium group color vertices &optional (object-id 0))
  "Retained-mode function, adds a filled 3d convex polygon to the draw-lists, returns no values.  Displays with flat shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices should be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn) where the x, y and z's represent a vertex of the polygon and must be real numbers.  There must be at least three x, y, z triplets in vertices.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the polygon, you must delete the entire group."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%draw-data-add-filled-3d-convex-polygon draw-data object-id group color vertices)))

(defun medium-draw-filled-3d-convex-polygon-flat (medium group color vertices &optional (object-id 0))
  "Immediate-mode function, draws a filled 3d convex polygon, returns no values.  Displays with flat shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices should be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn) where the x, y and z's represent a vertex of the polygon and must be real numbers.  There must be at least three x, y, z triplets in vertices.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data medium)))
    (%draw-data-draw-filled-3d-convex-polygon
     draw-data object-id group (canonicalize-color color) vertices)))

;; muticolor-3d-convex-polygon-diffuse
(defun medium-add-multicolor-3d-convex-polygon-primitive-diffuse (medium group model-matrix vertices material &optional (object-id 0))
  "Retained-mode function, returns a handle for a multicolored 3d convex polygon primitive.  Displays with diffuse shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), vertices must be of the form (list x0 y0 z0 nx0 ny0 nz0 color0 x1 y1 z1 nx1 ny1 nz1 color1 ... xn yn zn nxn nyn nzn colorn)  where each successive x, y z, nx, ny, nz and color are the vertices of the polygon and the normal at that vertex, and must be real numbers, color values can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  There must be at least three x, y, z, nx, ny, nz, color seven-tuples in vertices.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the polygon, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))  
  (declare (type atom group))
  (declare (type (or null material-mixin) material))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-multicolor-3d-convex-polygon-with-normals-primitive  
     draw-data handle object-id group (when model-matrix (mcopy model-matrix)) vertices material)))

(defun medium-add-multicolor-3d-convex-polygon-diffuse (medium group vertices &optional (object-id 0))
  "Retained-mode function, adds a multicolored 3d convex polygon to the draw lists, returns no values.  Displays with diffuse shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, vertices must be of the form (list x0 y0 z0 nx0 ny0 nz0 color0 x1 y1 z1 nx1 ny1 nz1 color1 ... xn yn zn nxn nyn nzn colorn)  where each successive x, y z, nx, ny, nz and color are the vertices of the polygon and the normal at that vertex, and must be real numbers, color values can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  There must be at least three x, y, z, nx, ny, nz, color seven-tuples in vertices.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the polygon, you must delete the entire group."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%draw-data-add-multicolor-3d-convex-polygon-with-normals draw-data object-id group vertices)))

(defun medium-draw-multicolor-3d-convex-polygon-diffuse (medium group vertices &optional (object-id 0))
  "Immediate-mode function, draws a multicolored 3d convex polygon, returns no values.  Displays with diffuse shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, vertices must be of the form (list x0 y0 z0 nx0 ny0 nz0 color0 x1 y1 z1 nx1 ny1 nz1 color1 ... xn yn zn nxn nyn nzn colorn)  where each successive x, y z, nx, ny, nz and color are the vertices of the polygon and the normal at that vertex, and must be real numbers, color values can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  There must be at least three x, y, z, nx, ny, nz, color seven-tuples in vertices.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data medium)))
    (%draw-data-draw-multicolor-3d-convex-polygon-with-normals draw-data object-id group vertices)))

;; multicolor-3d-convex-polygon-flat
(defun medium-add-multicolor-3d-convex-polygon-primitive-flat (medium group model-matrix vertices &optional (object-id 0))
  "Retained-mode function, returns a handle for a multicolored 3d convex polygon primitive.  Displays with flat shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), vertices should be of the form (list x0 y0 z0 color0 x1 y1 z1 color1 ... xn yn zn colorn) where the x, y and z's represent a vertex of the polygon and must be real numbers, color values can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  There must be at least three x, y, z, color quads in vertices.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the polygon, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-multicolor-3d-convex-polygon-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) vertices)))

(defun medium-add-multicolor-3d-convex-polygon-flat (medium group vertices &optional (object-id 0))
  "Retained-mode function, adds a multicolored 3d convex polygon to the draw lists, returns no values.  Displays with flat shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, vertices must be of the form (list x0 y0 z0 color0 x1 y1 z1 color1 ... xn yn zn colorn)  where each successive x, y z and color are the vertices of the polygon, and must be real numbers, color values can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  There must be at least three x, y, z, color quads in vertices.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the polygon, you must delete the entire group."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%draw-data-add-multicolor-3d-convex-polygon draw-data object-id group vertices)))

(defun medium-draw-multicolor-3d-convex-polygon-flat (medium group vertices &optional (object-id 0))
  "Immediate-mode function, draws a multicolored 3d convex polygon, returns no values.  Displays with flat shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, vertices should be of the form (list x0 y0 z0 color0 x1 y1 z1 color1 ... xn yn zn colorn) where the x, y and z's represent a vertex of the polygon and must be real numbers, color values can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  There must be at least three x, y, z, color quads in vertices.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data medium)))
    (%draw-data-draw-multicolor-3d-convex-polygon draw-data object-id group vertices)))

;; textured-3d-triangle-list-flat
(defun medium-add-textured-3d-triangle-list-primitive-flat (medium group model-matrix texture color vertices &optional (object-id 0))
  "Retained-mode function, returns a handle for a textured 3d triangle list primitive.  Displays with flat shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), texture should be a texture such as return from make-vulkan-texture, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices is a list must be composed of sub-sequences of x, y, z, u and v, where u and v are the normalized texture coordinates at that vertex.  There must be at least three sub-sequences of x, y, z, u and v to make a triangle and additional triangles come in 3 sub-sequences each.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the triangle list, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-textured-3d-triangle-list-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) texture color vertices)))

(defun medium-add-textured-3d-triangle-list-flat (medium group texture color vertices &optional (object-id 0))
  "Retained-mode function, adds a textured 3d convex polygon to the draw-lists, returns no values.  Displays with flat shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, texture should be a texture such as return from make-vulkan-texture, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices is a list and must be composed of sub-sequences of x, y, z, u and v, where u and v are the normalized texture coordinates at that vertex.  There must be at least three sub-sequences of x, y, z, u and v to make a triangle and additional triangles come in 3 sub-sequences each.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the polygon, you must delete the entire group."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%draw-data-add-textured-3d-triangle-list draw-data object-id group texture color vertices)))

(defun medium-draw-textured-3d-triangle-list-flat (medium group texture color vertices &optional (object-id 0))
  "Immediate-mode function, draws a textured 3d convex polygon, returns no values.  Displays with flat shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, texture should be a texture such as return from make-vulkan-texture, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices is a list and must be composed of sub-sequences of x, y, z, u and v, where u and v are the normalized texture coordinates at that vertex.  There must be at least three sub-sequences of x, y, z, u and v to make a triangle and additional triangles come in 3 sub-sequences each.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data medium)))
    (%draw-data-draw-textured-3d-triangle-list draw-data object-id group texture (canonicalize-color color) vertices)))

;; textured-3d-triangle-strip-flat
(defun medium-add-textured-3d-triangle-strip-primitive-flat (medium group model-matrix texture color vertices &optional (object-id 0))
  "Retained-mode function, returns a handle for a textured 3d triangle strip primitive.  Displays with flat shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity),  texture should be a texture such as return from make-vulkan-texture, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices should be of the form (list x0 y0 z0 u0 v0 x1 y1 z1 u1 v1 ... xn yn zn un vn) where the x, y and z's represent a vertex of the polygon and must be real numbers.  u and v are the normalized texture coordinates of that vertex, and must be real numbers between zero and one.  There must be at least three x, y, z, u and v quints in vertices.  Dispatches actual work to render thread.  To delete the triangle strip, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-textured-3d-triangle-strip-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) texture color vertices)))

(defun medium-draw-textured-3d-triangle-strip-flat (medium group texture color vertices &optional (object-id 0))
  "Immediate-mode function, draws a textured 3d triangle strip, returns no values.  Displays with flat shading.  Required arguments: medium must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices should be of the form (list x0 y0 z0 u0 v0 x1 y1 z1 u1 v1 ... xn yn zn un vn) where the x, y and z's represent a vertex of the polygon and must be real numbers.  u and v are the normalized texture coordinates of that vertex, and must be real numbers between zero and one.  There must be at least three x, y, z, u and v quints in vertices.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data medium)))
    (declare (type immediate-mode-draw-data draw-data))
    (let ((draw-list (draw-data-3d-triangle-strip-draw-list draw-data)))
      ;; we add the primitive/cmd without a handle:
      (%draw-list-add-textured-3d-triangle-strip/list
       draw-list object-id group nil texture (canonicalize-color color) vertices))))

(defun medium-add-filled-sphere-primitive-diffuse
    (medium group model-matrix color origin-x origin-y origin-z radius material
     resolution &optional (object-id 0))
  "Retained-mode function, returns a handle for a filled sphere primitive.  Displays with diffuse shading.  medium must be of the type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity),  color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, origin-z, origin-y and origin-z must be real numbers, radius must be a positive real number, light-position should either be nil or a 3d-vectors:vec3, resolution should be a positive integer and defaults to 64.  Dispatches actual work to render thread.  To delete the sphere, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real origin-x origin-y origin-z))
  (declare (type (integer 2 #.most-positive-fixnum) resolution))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (declare (type (or null material-mixin) material))
  (setq origin-x (coerce origin-x 'double-float))
  (setq origin-y (coerce origin-y 'double-float))
  (setq origin-z (coerce origin-z 'double-float))
  (setq radius (coerce radius 'double-float))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-filled-sphere-primitive
     draw-data handle object-id group (when model-matrix (mcopy model-matrix)) color origin-x origin-y origin-z radius resolution material)))

(defun medium-add-filled-ellipsoid-primitive-diffuse
    (medium group model-matrix color origin-x origin-y origin-z a b c material
     resolution &optional (object-id 0))
  "Retained-mode function, returns a handle for a filled sphere primitive.  Displays with diffuse shading.  medium must be of the type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity),  color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, origin-z, origin-y and origin-z must be real numbers, radius must be a positive real number, light-position should either be nil or a 3d-vectors:vec3, resolution should be a positive integer and defaults to 64.  Dispatches actual work to render thread.  To delete the sphere, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real origin-x origin-y origin-z))
  (declare (type (integer 2 #.most-positive-fixnum) resolution))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (declare (type (or null material-mixin) material))
  (setq origin-x (coerce origin-x 'double-float))
  (setq origin-y (coerce origin-y 'double-float))
  (setq origin-z (coerce origin-z 'double-float))
  (setq a (coerce a 'double-float))
  (setq b (coerce b 'double-float))
  (setq c (coerce c 'double-float))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
    (%draw-data-add-filled-ellipsoid-primitive
     draw-data handle object-id group (when model-matrix (mcopy model-matrix)) color origin-x origin-y origin-z a b c resolution material)))

(defun medium-add-filled-sphere-diffuse (medium group color origin-x origin-y origin-z radius resolution &optional (object-id 0))
  "Retained-mode function, adds a filled sphere to the draw-lists, returns no values.  Displays with diffuse shading.  medium must be of the type krma-essential-scene-mixin, group must be a non-null atom,  color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, origin-z, origin-y and origin-z must be real numbers, radius must be a positive real number,  resolution should be a positive integer and defaults to 64.  Dispatches actual work to render thread.  To delete the sphere, you must delete the entire group."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real origin-x origin-y origin-z))
  (declare (type (integer 2 #.most-positive-fixnum) resolution))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq origin-x (coerce origin-x 'double-float))
  (setq origin-y (coerce origin-y 'double-float))
  (setq origin-z (coerce origin-z 'double-float))
  (setq radius (coerce radius 'double-float))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%draw-data-add-filled-sphere
     draw-data object-id group color origin-x origin-y origin-z radius resolution)))

(defun medium-draw-filled-sphere-diffuse (medium group color origin-x origin-y origin-z radius resolution &optional (object-id 0))
  "Immediate-mode function, draws a filled sphere, returns no values.  Displays with diffuse shading.  medium must be of the type krma-essential-scene-mixin, group must be a non-null atom,  color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, origin-z, origin-y and origin-z must be real numbers, radius must be a positive real number,  resolution should be a positive integer and defaults to 64.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real origin-x origin-y origin-z))
  (declare (type (integer 2 #.most-positive-fixnum) resolution))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq origin-x (coerce origin-x 'double-float))
  (setq origin-y (coerce origin-y 'double-float))
  (setq origin-z (coerce origin-z 'double-float))
  (setq radius (coerce radius 'double-float))
  (setq color (canonicalize-color color))
  (%draw-data-draw-filled-sphere (im-draw-data medium) object-id group color origin-x origin-y origin-z radius resolution))

;; 2d-text
(defun compute-text-coordinates (pos-x pos-y string glyph-table scale-w scale-h)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type single-float pos-x pos-y scale-w scale-h))
  (declare (type string string))
  (declare (type hash-table glyph-table))
  (loop for i from 0 below (length string)
	with coords = nil
	with glyph = nil
	with dx = 0.0f0
	with x0
	with y0
	with x1
	with y1
	with u0
	with v0
	with u1
	with v1
	with width
	with height
	do (setf glyph (gethash (char string i) glyph-table))
	when (and glyph (not (eq (3b-bmfont-common:glyph-id glyph) 32))) ;; hack to deal with #\space artifact
	  do
	     (setq width (float (3b-bmfont:glyph-width glyph) 1.0f0))
	     (setq height (float (3b-bmfont:glyph-height glyph) 1.0f0))
	     (setq u0 (/ (float (3b-bmfont:glyph-x glyph) 1.0f0) scale-w))
	     (setq v0 (/ (float (3b-bmfont:glyph-y glyph) 1.0f0) scale-h))
	     (setq u1 (+ u0 (/ width scale-w)))
	     (setq v1 (+ v0 (/ height scale-h)))
	     (setq x0 (+ pos-x (3b-bmfont:glyph-xoffset glyph)))
	     (setq y0 (+ pos-y (3b-bmfont:glyph-yoffset glyph)))
	     (setq x1 (+ x0 width))
	     (setq y1 (+ y0 height))

	     (push (+ x0 (cl:the single-float dx)) coords)
	     (push y0 coords)
	     (push u0 coords)
	     (push v0 coords)
	     (push (+ x1 (cl:the single-float dx)) coords)
	     (push y1 coords)
	     (push u1 coords)
	     (push v1 coords)
	     
	when glyph
	  do (incf (cl:the single-float dx) (float (3b-bmfont:glyph-xadvance glyph) 1.0f0))
	finally (return (nreverse coords))))

(defun medium-add-text-primitive (medium group model-matrix font color pos-x pos-y string &optional (object-id 0) (elevation 0))
  "Retained-mode function, renders and returns a handle for a text primitive.  medium must be of the type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), font is a font object as returned by vulkan-make-font, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, pos-x and pos-y represent the top left corner of the text and must be real numbers, string is the string you wish to render.  Dispatches actual work to render thread.  To delete the text, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type real pos-x pos-y))
  (declare (type string string))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (unless (string= string "")
    (let ((data (font-data font)))
      (declare (type 3b-bmfont-common:bmfont data))
      (let* ((glyph-table (slot-value data '3b-bmfont-common::chars))
	     (scale-w (float (3b-bmfont-common:scale-w data) 1.0f0))
	     (scale-h (float (3b-bmfont-common:scale-h data) 1.0f0))
	     (pos-x (clampf pos-x))
	     (pos-y (clampf pos-y))
	     (color (canonicalize-color color))
	     (vertices (compute-text-coordinates pos-x pos-y string glyph-table scale-w scale-h))
	     (elevation (clampf elevation)))
	(when vertices
	  (rm-dispatch-to-render-thread-with-handle (medium draw-data handle)
	    (%draw-data-add-text-quad-list-primitive draw-data handle object-id group
						     (when model-matrix (mcopy model-matrix))
						     font color elevation
						     vertices)))))))



(defun medium-add-text (medium group font color pos-x pos-y string &optional (object-id 0) (elevation 0))
  "Retained-mode function, adds text to the draw lists, returns no values.  medium must be of the type krma-essential-scene-mixin, group must be a non-null atom, font is a font object as returned by vulkan-make-font, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, pos-x and pos-y represent the top left corner of the text and must be real numbers, string is the string you wish to render.  Dispatches actual work to render thread.  To delete the text, you must delete the entire group."
  (declare (type real pos-x pos-y))
  (declare (type string string))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (unless (string= string "")
    (let ((data (font-data font)))
      (declare (type 3b-bmfont-common:bmfont data))
      (let* ((glyph-table (slot-value data '3b-bmfont-common::chars))
	     (scale-w (float (3b-bmfont-common:scale-w data) 1.0f0))
	     (scale-h (float (3b-bmfont-common:scale-h data) 1.0f0))
	     (pos-x (clampf pos-x))
	     (pos-y (clampf pos-y))
	     (color (canonicalize-color color))
	     (vertices (compute-text-coordinates pos-x pos-y string glyph-table scale-w scale-h))
	     (elevation (clampf elevation)))
	(when vertices
	  (rm-dispatch-to-render-thread (medium draw-data)
	    (%draw-data-add-text-quad-list draw-data object-id group font color elevation vertices)))))))


  

(defun medium-draw-text (medium group font color pos-x pos-y string &optional (object-id 0) (elevation 0))
  "Retained-mode function, draws text, returns no values.  medium must be of the type krma-essential-scene-mixin, group must be a non-null atom, font is a font object as returned by vulkan-make-font, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, pos-x and pos-y represent the top left corner of the text and must be real numbers, string is the string you wish to render.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type real pos-x pos-y))
  (declare (type string string))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((data (font-data font)))
    (declare (type 3b-bmfont-common:bmfont data))
    (let* ((glyph-table (slot-value data '3b-bmfont-common::chars))
           (scale-w (float (3b-bmfont-common:scale-w data) 1.0f0))
           (scale-h (float (3b-bmfont-common:scale-h data) 1.0f0))
	   (pos-x (clampf pos-x))
	   (pos-y (clampf pos-y))
           (vertices (compute-text-coordinates pos-x pos-y string glyph-table scale-w scale-h)))
      (setq elevation (clampf elevation))
      (when vertices
	(%draw-data-draw-text-quad-list (im-draw-data medium) object-id group font (canonicalize-color color) elevation vertices)))))



;;(declaim (inline %reinstance-primitive-1))
#+update-me
(defun %reinstance-primitive-1 (ht new-handle handle
                                group model-mtx sf-line-thickness sf-point-size ub32-color-override light-position font)
  (let ((cmd (gethash handle ht)))
    (if (listp cmd)
        (warn "while in %reinstance-primitve-1 ...couldn't find primitive ~S to reinstance" handle)
        (let ((constructor #'make-standard-draw-indexed-cmd))
          (declare (type function constructor))
          (when font
            (setq constructor #'(lambda (&rest args)
                                  (apply #'make-text-draw-indexed-cmd font args))))
          (setf (gethash new-handle ht)
                )))))

#+update-me
(defun reinstance-primitive-1 (draw-data new-handle handle
                               &key 
				 (group nil)
				 (model-matrix nil)
                                 (point-size nil)
                                 (line-thickness nil)
                                 (color-override nil)
                                 (light-position nil)
                                 (font nil))
  "Retained-mode function.  Re-instances a primitive given a handle with the option to set a new group, model-matrix, point-size, line-thickness, color-override, light-position and/or font.  References the same vertices in the draw-lists. Performs work in current thread, which should be the render thread."
  (declare (type retained-mode-draw-data draw-data))
  (let ((ht (draw-data-handle-hash-table draw-data)))
    (%reinstance-primitive-1
     ht new-handle handle
     group model-matrix (clampf line-thickness) (clampf point-size) (canonicalize-color color-override) light-position font)))

#+update-me
(defun reinstance-primitive (medium handle
                             &key 
			       (group nil)
			       (model-matrix nil)
                               (point-size nil)
                               (line-thickness nil)
                               (color-override nil)
                               (light-position nil)
                               (font nil))
  "Retained-mode function.  Re-instances a primitive given a handle with the option to set a new group, model-matrix, point-size, line-thickness, color-override, light-position and/or font.  References the same vertices in the draw-lists.  To be run in a thread outside of the render thread.  Dispatches actual work to render thread."
  (declare (type krma-essential-scene-mixin medium))
  (when point-size
    (setq point-size (clampf point-size)))
  (when line-thickness
    (setq line-thickness (clampf line-thickness)))
  (when color-override
    (setq color-override (canonicalize-color color-override)))
  (let ((draw-data (rm-draw-data medium)))
    (let ((dd0 (svref draw-data 0))
          (dd1 (svref draw-data 1)))
      (declare (type retained-mode-draw-data dd0 dd1))
      (let ((ht0 (draw-data-handle-hash-table dd0))
            (ht1 (draw-data-handle-hash-table dd1))
            (wq0 (draw-data-work-queue dd0))
            (wq1 (draw-data-work-queue dd1))
            (new-handle (gen-rm-handle)))
	
        #+notyet(sb-concurrency:enqueue #'(lambda ()
					    (%reinstance-primitive-1 ht0 new-handle handle group
								     model-matrix line-thickness point-size color-override
								     light-position font))
					wq0)
	
	#+notyet(sb-concurrency:enqueue #'(lambda ()
					    (%reinstance-primitive-1 ht1 new-handle handle group
								     model-matrix line-thickness point-size color-override
								     light-position font))
					wq1)
        new-handle))))

(declaim (inline %primitive-set-color-1))
(defun %primitive-set-color-1 (ht handle ub32-color)
  (let ((cmd (gethash handle ht)))
    (if (listp cmd)
        (warn "while in %primitive-set-color-1 ...could not find primitive ~S to set color." handle)
        (setf (cmd-color-override cmd) ub32-color))
    (values)))

(defun primitive-set-color-1 (draw-data handle color)
  "Retained-mode function.  Returns no values.  Sets the color override of the primitive, normally renderer uses the vertex color.  Performs work in current thread, which should be the render thread."
  (declare (type retained-mode-draw-data draw-data))
  (let ((ht (draw-data-handle-hash-table draw-data)))
    (%primitive-set-color-1 ht handle (canonicalize-color color))))

;; need to be able to modify existing primitives
(defun primitive-set-color (medium handle color)
  "Retained-mode function. Returns no values.  To be run in a thread outside of the render thread.  Sets the color override of the primitive, normally renderer uses the vertex color.  To be run in a thread outside of the render thread.  Dispatches actual work to render thread."
  (declare (type krma-essential-scene-mixin medium))
  (let ((draw-data (rm-draw-data medium)))
    (setq color (canonicalize-color color))
    (let ((dd0 (svref draw-data 0))
          (dd1 (svref draw-data 1)))
      (declare (type retained-mode-draw-data dd0 dd1))
      (let ((ht0 (draw-data-handle-hash-table dd0))
            (ht1 (draw-data-handle-hash-table dd1))
            (wq0 (draw-data-work-queue dd0))
            (wq1 (draw-data-work-queue dd1)))
        (lparallel.queue:push-queue #'(lambda () (%primitive-set-color-1 ht0 handle color)) wq0)
        (lparallel.queue:push-queue #'(lambda () (%primitive-set-color-1 ht1 handle color)) wq1)
        (values)))))

(declaim (inline %primitive-set-light-position-1))
(defun %primitive-set-light-position-1 (ht handle pos)
  (let ((cmd (gethash handle ht)))
    (if (listp cmd)
        (warn "while in %primitive-set-light-position-1 ...could not find primitive ~S to set light position." handle)
        (setf (cmd-light-position cmd) pos))
    (values)))

(defun primitive-set-light-position-1 (draw-data handle pos)
  "Retained-mode function.  Returns no values.  Sets the light position of the primitive, normally renderer uses the medium light position, unless group light position is set.  Performs work in current thread, which should be the render thread."
  (declare (type retained-mode-draw-data draw-data))
  (let ((ht (draw-data-handle-hash-table draw-data)))
    (%primitive-set-light-position-1 ht handle pos)))

(defun primitive-set-light-position (medium handle pos)
  "Retained-mode function.  Returns no values.  Sets the light position of the primitive, normally renderer uses the medium light position, unless group light position is set.  To be run in a thread outside of the render thread.  Dispatches actual work to render thread."
  (declare (type krma-essential-scene-mixin medium))
  (let ((draw-data (rm-draw-data medium)))
    (let ((dd0 (svref draw-data 0))
          (dd1 (svref draw-data 1)))
      (declare (type retained-mode-draw-data dd0 dd1))
      (let ((ht0 (draw-data-handle-hash-table dd0))
            (ht1 (draw-data-handle-hash-table dd1))
            (wq0 (draw-data-work-queue dd0))
            (wq1 (draw-data-work-queue dd1)))
        (lparallel.queue:push-queue #'(lambda () (%primitive-set-light-position-1 ht0 handle pos)) wq0)
        (lparallel.queue:push-queue #'(lambda () (%primitive-set-light-position-1 ht1 handle pos)) wq1)
        (values)))))

(declaim (inline %primitive-set-transform-1))
(defun %primitive-set-transform-1 (ht handle matrix)
  (let ((cmd (gethash handle ht)))
    (if (listp cmd)
        (warn "while in %primitive-set-transform-1 ...could not find primitive to set transform ~S." handle)
        (setf (cmd-model-mtx cmd) (when matrix (mcopy matrix))))
    (values)))

(defun primitive-set-transform-1 (draw-data handle matrix)
  "Retained-mode function.  Returns no values.  Sets the model-matrix of the primitive, renderer composes the primitive model matrix with the group model matrix.  Performs work in current thread, which should be the render thread."
  (declare (type retained-mode-draw-data draw-data))
  (let ((ht (draw-data-handle-hash-table draw-data)))
    (%primitive-set-transform-1 ht handle matrix)))

;; replaces model-mtx in cmd by this matrix
(defun primitive-set-transform (medium handle matrix)
  "Retained-mode function.  Sets the model-matrix of the primitive, renderer composes the primitive model matrix with the group model matrix.  To be run in a thread outside of the render thread.  Dispatches actual work to render thread."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type (or 3dm.rat:mat4 3dm.d:mat4 3dm.f:mat4 null) matrix))
  (let ((draw-data (rm-draw-data medium)))
    (let ((dd0 (svref draw-data 0))
          (dd1 (svref draw-data 1)))
      (declare (type retained-mode-draw-data dd0 dd1))
      (let ((ht0 (draw-data-handle-hash-table dd0))
            (ht1 (draw-data-handle-hash-table dd1))
            (wq0 (draw-data-work-queue dd0))
            (wq1 (draw-data-work-queue dd1)))
        (lparallel.queue:push-queue #'(lambda () (%primitive-set-transform-1 ht0 handle matrix)) wq0)
        (lparallel.queue:push-queue #'(lambda () (%primitive-set-transform-1 ht1 handle matrix)) wq1)
        (values)))))

(declaim (inline %primitive-apply-transform-1))
(defun %primitive-apply-transform-1 (ht handle matrix)
  (let ((cmd (gethash handle ht)))
    (if (listp cmd)
        (warn "while in %primitive-apple-transform-1 ...could not find primitive to apply transform ~S." handle)
        (let ((existing (cmd-model-mtx cmd)))
          (if existing
              (setf (cmd-model-mtx cmd) (m* matrix existing))
              (setf (cmd-model-mtx cmd) (mcopy matrix)))))
    (values)))

(defun primitive-apply-transform-1 (draw-data handle matrix)
  "Retained-mode function.  Returns no values.  Applies the matrix to the existing model-matrix of the primitive, renderer composes the primitive model matrix with the group model matrix.  Performs work in current thread, which should be the render thread."
  (declare (type retained-mode-draw-data draw-data))
  (declare (type mat4 matrix))
  (let ((ht (draw-data-handle-hash-table draw-data)))
    (%primitive-apply-transform-1 ht handle matrix)))

;; multiplies new matrix against old matrix and replaces model-mtx in cmd
(defun primitive-apply-transform (medium handle matrix)
  "Retained-mode function.  Returns no values.  Applies the matrix to the existing model-matrix of the primitive, renderer composes the primitive model matrix with the group model matrix.  To be run in a thread outside of the render thread.  Dispatches actual work to render thread."
  (declare (type krma-essential-scene-mixin medium))
  (declare (type mat4 matrix))
  (let ((draw-data (rm-draw-data medium)))
    (let ((dd0 (svref draw-data 0))
          (dd1 (svref draw-data 1)))
      (declare (type retained-mode-draw-data dd0 dd1))
      (let ((ht0 (draw-data-handle-hash-table dd0))
            (ht1 (draw-data-handle-hash-table dd1))
            (wq0 (draw-data-work-queue dd0))
            (wq1 (draw-data-work-queue dd1)))
        (lparallel.queue:push-queue #'(lambda () (%primitive-apply-transform-1 ht0 handle matrix)) wq0)
        (lparallel.queue:push-queue #'(lambda () (%primitive-apply-transform-1 ht1 handle matrix)) wq1)
        (values)))))

(declaim (inline %primitive-set-line-thickness-1))
(defun %primitive-set-line-thickness-1 (ht handle sf-thickness)
  (declare (type single-float sf-thickness))
  (let ((cmd (gethash handle ht)))
    (if (listp cmd)
        (warn "while in %primitive-set-line-thickness-1 ...could not find primitive to update line thickness ~S."
              handle)
        (setf (cmd-line-thickness cmd) sf-thickness))
    (values)))

(defun primitive-set-line-thickness-1 (draw-data handle thickness)
  (declare (type retained-mode-draw-data draw-data))
  (let ((ht (draw-data-handle-hash-table draw-data)))
    (%primitive-set-line-thickness-1 ht handle (clampf thickness))))

(defun primitive-set-line-thickness (medium handle thickness)
  (declare (type krma-essential-scene-mixin medium))
  (declare (type real thickness))
  (setq thickness (clampf thickness))
  (let ((draw-data (rm-draw-data medium)))
    (let ((dd0 (svref draw-data 0))
          (dd1 (svref draw-data 1)))
      (declare (type retained-mode-draw-data dd0 dd1))
      (let ((ht0 (draw-data-handle-hash-table dd0))
            (ht1 (draw-data-handle-hash-table dd1))
            (wq0 (draw-data-work-queue dd0))
            (wq1 (draw-data-work-queue dd1)))
        #+notyet(sb-concurrency:enqueue #'(lambda () (%primitive-set-line-thickness-1 ht0 handle thickness)) wq0)
        #+notyet(sb-concurrency:enqueue #'(lambda () (%primitive-set-line-thickness-1 ht1 handle thickness)) wq1)
        (values)))))


#+NIL
(defun %delete-primitives-2 (ht handles)
  (handler-case
      (let ((cmds (mapcar #'(lambda (handle)
			      (cons handle (gethash handle ht))) handles)))
	(let* ((cmd-vector (draw-list-cmd-vector draw-list))
	       (holes 0))
	  (loop for entry across cmd-vector
	     with cmd = nil
	     for i from 0
	     unless entry
	     do (incf holes)
	     when (setq cmd (find entry cmds :key #'cdr))
	     do (remhash (car cmd) ht)
	       (setq cmds (delete entry cmds :key #'cdr))
	       (setf (aref cmd-vector i) nil)
	       (incf holes)
	     unless cmds
	     do (return))
	  ;; we do not count all the holes here, we'll do that elsewhere
	  ;; but if holes happens to exceed trigger already, then schedule compaction
	  (when (> holes (floor (* (fill-pointer cmd-vector) *compact-trigger*)))
	    (setf (draw-list-needs-compaction? draw-list) t))
	  (values)))
    (error (c)
      (warn (concatenate 'string "while in %delete-primitive2-2 ..." (princ-to-string c)))
      (values))))    

(defun %delete-primitive-1 (ht handle)
  (handler-case
      (let ((cmd (gethash handle ht)))
        (if (listp cmd)
            (warn "while in %delete-primitive-1 ...could not find primitive to delete ~S" handle)
            (let* ((draw-list (cmd-draw-list cmd))
                   (cmd-vector (draw-list-cmd-vector draw-list)))
              (loop for entry across cmd-vector
		 for i from 0
		 when (and entry (eq cmd entry))
		 do (setf (aref cmd-vector i) nil)
		   (remhash handle ht)
		   (return (values))))))
    (error (c)
      (warn (concatenate 'string "while in %delete-primitive-1 ..." (princ-to-string c)))
      (values))))

(defun delete-primitive-1 (draw-data handle)
  (let ((ht (draw-data-handle-hash-table draw-data)))
    (%delete-primitive-1 ht handle)))

(defun delete-primitives (medium list-of-handles)
  (declare (type krma-essential-scene-mixin medium))
  (let ((draw-data (rm-draw-data medium)))
    (let ((dd0 (svref draw-data 0))
          (dd1 (svref draw-data 1)))
      (declare (type retained-mode-draw-data dd0 dd1))
      (let ((wq0 (draw-data-work-queue dd0))
            (wq1 (draw-data-work-queue dd1))
            (ht0 (draw-data-handle-hash-table dd0))
            (ht1 (draw-data-handle-hash-table dd1)))
        (lparallel.queue:push-queue
	 #'(lambda ()
	     (loop for handle in list-of-handles
		  do (%delete-primitive-1 ht0 handle)))
         wq0)
        (lparallel.queue:push-queue
	 #'(lambda ()
	     (loop for handle in list-of-handles
		do (%delete-primitive-1 ht1 handle)))
         wq1)))))

(defun delete-primitive (medium handle)
  (declare (type krma-essential-scene-mixin medium))
  (delete-primitives medium (list handle)))

(defun delete-groups-1 (draw-data list-of-groups &optional (totally-expunge? t))
  (declare (type retained-mode-draw-data draw-data))
  (handler-case
      (progn
        (flet ((free-group-draw-lists (dpy ht)
                 (unless ht
                   (warn "ht is null"))
                 (let ((key-list ()))

		   #+NO ;; key is a list!
		   (loop for group in list-of-groups
			 do (let ((draw-list (gethash group ht)))
			      (unless draw-list
				(warn "could not find entry for ~S" group))
			      (when draw-list
				(let ((im (draw-list-index-memory draw-list))
				      (vm (draw-list-vertex-memory draw-list)))
				  (vk::release-index-memory dpy im)
				  (vk::release-vertex-memory dpy vm)))
			      (remhash group ht)))

		   (let ((new-ht (make-hash-table :test #'equal)))

		     (maphash #'(lambda (key draw-list)
				  (unless (find (car key) list-of-groups)
				    (setf (gethash key new-ht) draw-list))
				  (when (find (car key) list-of-groups)
				    (let ((im (draw-list-index-memory draw-list))
					  (vm (draw-list-vertex-memory draw-list)))
				      (when im
					(vk::release-index-memory dpy im))
				      (when vm
					(vk::release-vertex-memory dpy vm)))))
			      ht)

		     new-ht)

		   ;; this ought to be fast since we maphash these tables at render time also
                   ;; and haven't a performance problem yet (knock on wood)

		   #+NIL		   
                   (maphash #'(lambda (key draw-list)
                                (when (find (car key) list-of-groups)
                                  (push key key-list)
                                  (let ((im (draw-list-index-memory draw-list))
                                        (vm (draw-list-vertex-memory draw-list)))
                                    (vk::release-index-memory dpy im)
                                    (vk::release-vertex-memory dpy vm))))
                            ht)
		   #+NIL
                   (mapcar #'(lambda (key)
                               (remhash key ht))
                           key-list)))

               (delete-primitives-with-groups (draw-list)
                 (let ((cmd-vector (draw-list-cmd-vector draw-list)))
                   (loop for cmd across cmd-vector
                         for i from 0
                         when (and cmd (find (cmd-group cmd) list-of-groups))
			   do (setf (aref cmd-vector i) nil)))))

          (with-slots (2d-point-list-draw-list-table
                       2d-line-list-draw-list-table
                       2d-triangle-list-draw-list-table
                       2d-triangle-list-draw-list-for-text-table
                       3d-point-list-draw-list-table
                       3d-line-list-draw-list-table
                       3d-triangle-list-draw-list-table
                       3d-triangle-list-with-normals-draw-list-table

                       2d-triangle-strip-draw-list
                       3d-line-strip-draw-list
                       3d-triangle-strip-draw-list
                       3d-triangle-strip-with-normals-draw-list

                       2d-point-list-draw-list
                       2d-line-list-draw-list
                       2d-triangle-list-draw-list
                       2d-triangle-list-draw-list-for-text
                       3d-point-list-draw-list
                       3d-line-list-draw-list
                       3d-triangle-list-draw-list
                       3d-triangle-list-with-normals-draw-list

		       dpy)
              draw-data

            ;; I don't like these brute force searches, but it can't be any slower than a render!

	    ;; having a problem, perhaps specific to ccl, where remhashing keys from hash tables
	    ;; is producing inconsistent graphical results, workaround: copy the hash tables
	    ;; instead minus the groups

            (setf 2d-point-list-draw-list-table (free-group-draw-lists dpy 2d-point-list-draw-list-table))
            (setf 2d-line-list-draw-list-table (free-group-draw-lists dpy 2d-line-list-draw-list-table))
            (setf 2d-triangle-list-draw-list-table (free-group-draw-lists dpy 2d-triangle-list-draw-list-table))
            (setf 2d-triangle-list-draw-list-for-text-table (free-group-draw-lists dpy 2d-triangle-list-draw-list-for-text-table))
            (setf 3d-point-list-draw-list-table (free-group-draw-lists dpy 3d-point-list-draw-list-table))
            (setf 3d-line-list-draw-list-table (free-group-draw-lists dpy 3d-line-list-draw-list-table))
            (setf 3d-triangle-list-draw-list-table (free-group-draw-lists dpy 3d-triangle-list-draw-list-table))
            (setf 3d-triangle-list-with-normals-draw-list-table
		  (free-group-draw-lists dpy 3d-triangle-list-with-normals-draw-list-table))

	    ;;(setf 2d-triangle-list-draw-list-table (make-hash-table :test #'equalp))

            (delete-primitives-with-groups 2d-triangle-strip-draw-list)
            (delete-primitives-with-groups 3d-line-strip-draw-list)
            (delete-primitives-with-groups 3d-triangle-strip-draw-list)
            (delete-primitives-with-groups 3d-triangle-strip-with-normals-draw-list)
            (delete-primitives-with-groups 2d-point-list-draw-list)
            (delete-primitives-with-groups 2d-line-list-draw-list)
            (delete-primitives-with-groups 2d-triangle-list-draw-list)
            (delete-primitives-with-groups 2d-triangle-list-draw-list-for-text)
            (delete-primitives-with-groups 3d-point-list-draw-list)
            (delete-primitives-with-groups 3d-line-list-draw-list)
            (delete-primitives-with-groups 3d-triangle-list-draw-list)
            (delete-primitives-with-groups 3d-triangle-list-with-normals-draw-list))

	  (when totally-expunge?
	    (let ((groups (draw-data-group-hash-table draw-data)))
	      (unless groups
		(warn "groups is null"))
	      (mapcar #'(lambda (group)
			  (remhash group groups))
		      list-of-groups)))))
    (error (c)
      (warn (concatenate 'string "while in delete-groups-1 ..." (princ-to-string c))))))

(defun %purge-im-groups-1 (dpy draw-data)
  ;; call this function when finalizing a medium
  (declare (type immediate-mode-draw-data draw-data))
  (handler-case
      (progn
        (flet ((free-group-draw-lists (ht)
                 (unless ht
                   (warn "ht is null"))
                 (maphash #'(lambda (key draw-list)
                              (declare (ignore key))
                              ;; we're wanting to delete all groups from immediate mode draw lists!
                              (let ((ia (draw-list-index-array draw-list))
                                    (va (draw-list-vertex-array draw-list))
                                    (im (draw-list-index-memory draw-list))
                                    (vm (draw-list-vertex-memory draw-list)))
                                (declare (type foreign-adjustable-array ia va))
                                (foreign-free (foreign-array-ptr ia))
                                (foreign-free (foreign-array-ptr va))
				(when dpy
				  (when im
				    (vk::release-index-memory dpy im))
				  (when vm
				    (vk::release-vertex-memory dpy vm)))
                                nil))
                          ht)))

          (with-slots (2d-point-list-draw-list-table
                       2d-line-list-draw-list-table
                       2d-triangle-list-draw-list-table
                       2d-triangle-list-draw-list-for-text-table
                       3d-point-list-draw-list-table
                       3d-line-list-draw-list-table
                       3d-triangle-list-draw-list-table
                       3d-triangle-list-with-normals-draw-list-table
                       )
              draw-data

            (princ 'in-%purge-im-groups-1)
            (finish-output)

            ;; I don't like these brute force searches, but it can't be any slower than a render!

            (free-group-draw-lists 2d-point-list-draw-list-table)
            (free-group-draw-lists 2d-line-list-draw-list-table)
            (free-group-draw-lists 2d-triangle-list-draw-list-table)
            (free-group-draw-lists 2d-triangle-list-draw-list-for-text-table)
            (free-group-draw-lists 3d-point-list-draw-list-table)
            (free-group-draw-lists 3d-line-list-draw-list-table)
            (free-group-draw-lists 3d-triangle-list-draw-list-table)
            (free-group-draw-lists 3d-triangle-list-with-normals-draw-list-table))))

    (error (c)
      (warn (concatenate 'string "while in %purge-im-groups-1 ..." (princ-to-string c))))))

(defun delete-groups (medium list-of-groups &optional (totally-expunge? t))
  (declare (type krma-essential-scene-mixin medium))
  (rm-dispatch-to-render-thread (medium draw-data)
    (delete-groups-1 draw-data list-of-groups totally-expunge?)))

(defun delete-group (medium group &optional (totally-expunge? t))
  (declare (type (and atom t) group))
  (delete-groups medium (list group) totally-expunge?))

(declaim (inline %group-set-color-override-1))
(defun %group-set-color-override-1 (draw-data atom-group ub32-color)
  (let ((group (gethash atom-group (draw-data-group-hash-table draw-data))))
    (if group
        (setf (group-color-override group) ub32-color)
        (warn "while in %group-set-color-override-1 ...no group named ~S" atom-group))))

(defun group-set-color-override-1 (draw-data group color)
  (declare (type (and atom t) group))
  (%group-set-color-override-1 draw-data group (canonicalize-color color)))

(defun group-set-color-override (medium group color)
  (declare (type krma-essential-scene-mixin medium))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%group-set-color-override-1 draw-data group color)))

(declaim (inline %group-set-model-matrix-1))
(defun %group-set-model-matrix-1 (draw-data atom-group matrix)
  (let ((group (gethash atom-group (draw-data-group-hash-table draw-data))))
    (if group
        (setf (group-model-matrix group) matrix)
        (warn "while in %group-set-model-matrix-1 ...no group named ~S" atom-group))))

(defun group-set-model-matrix-1 (draw-data group matrix)
  (declare (type (or 3dm.f:mat4 3dm.d:mat4 3dm.rat:mat4 null) matrix))
  (declare (type (and atom t) group))
  (%group-set-model-matrix-1 draw-data group matrix))

(defun group-set-model-matrix (medium group matrix)
  (declare (type krma-essential-scene-mixin medium))
  (declare (type (or 3dm.f:mat4 3dm.d:mat4 3dm.rat:mat4 null) matrix))
  (declare (type (and atom t) group))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%group-set-model-matrix-1 draw-data group (mcopy matrix))))

(declaim (inline %group-apply-model-matrix-1))
(defun %group-apply-model-matrix-1 (draw-data atom-group matrix)
  (let ((group (gethash atom-group (draw-data-group-hash-table draw-data))))
    (if group
        (let ((existing (group-model-matrix group)))
          (if existing
              (setf (group-model-matrix group) (m* matrix existing))
              (setf (group-model-matrix group) (mcopy matrix))))
        (warn "while in %group-apply-model-matrix-1 ...no group named ~S" atom-group))))

(defun group-apply-model-matrix-1 (draw-data group matrix)
  (declare (type (or mat4 null) matrix))
  (declare (type (and atom t) group))
  (%group-apply-model-matrix-1 draw-data group matrix))

(defun group-apply-model-matrix (medium group matrix)
  (declare (type krma-essential-scene-mixin medium))
  (declare (type (or mat4 null) matrix))
  (declare (type (and atom t) group))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%group-apply-model-matrix-1 draw-data group matrix)))

#+NIL
(declaim (inline %group-set-light-position-1))
#+NIL
(defun %group-set-light-position-1 (draw-data atom-group light-position)
  (let ((group (gethash atom-group (draw-data-group-hash-table draw-data))))
    (if group
        (setf (group-light-position group) (when light-position (vcopy light-position)))
        (warn "while in %group-set-light-position-1 ...no group named ~S" atom-group))))

#+NIL
(defun group-set-light-position-1 (draw-data group position)
  (declare (type (or vec3 null) position))
  (declare (type (and atom t) group))
  (%group-set-light-position-1 draw-data group position))

#+NIL
(defun group-set-light-position (medium group position)
  (declare (type krma-essential-scene-mixin medium))
  (declare (type (or vec3 null) position))
  (declare (type (and atom t) group))
  (rm-dispatch-to-render-thread (medium draw-data)
    (%group-set-light-position-1 draw-data group position)))

(defun ensure-group-1 (draw-data group)
  (declare (type (and atom t) group))
  (let ((group-hash-table (draw-data-group-hash-table draw-data)))
    (or (gethash group group-hash-table)
        (setf (gethash group group-hash-table)
              (make-group group)))))

(defun medium-ensure-group (medium group)
  (rm-dispatch-to-render-thread (medium draw-data)
    (ensure-group-1 draw-data group)))
  

(defun sort-2d-draw-lists (draw-data)
  (with-slots (2d-point-list-draw-list
	       2d-line-list-draw-list
	       2d-triangle-list-draw-list
	       2d-triangle-list-draw-list-for-text)
      draw-data
    (flet ((do-sort (draw-list)
	     (let ((cmds (draw-list-cmd-vector draw-list)))
	       (sort cmds #'(lambda (one two)
			      (if (null one)
				  nil
				  (if (null two)
				      t
				      (let ((elevation1 (cmd-elevation one))
					    (elevation2 (cmd-elevation two)))
					(< elevation1 elevation2))))))))
	   (check-for-compaction (draw-list)
	     ;; should this be done in compaction thread?
	     ;; seems convenient here, since it's sorted
	     ;; actually, don't i already compact cmd vector in compaction thread??
	     (let* ((cmds (draw-list-cmd-vector draw-list))
		    (pos (position nil cmds)))
	       (when pos
		 (let ((holes (- (fill-pointer cmds) pos)))
		   (when (> holes (floor (* (fill-pointer cmds) *compact-trigger*)))
		     (setf (draw-list-needs-compaction? draw-list) t)))))))
      
      (do-sort 2d-point-list-draw-list)
      (check-for-compaction 2d-point-list-draw-list)
      (do-sort 2d-line-list-draw-list)
      (check-for-compaction 2d-line-list-draw-list)
      (do-sort 2d-triangle-list-draw-list)
      (check-for-compaction 2d-triangle-list-draw-list)
      (do-sort 2d-triangle-list-draw-list-for-text)
      (check-for-compaction 2d-triangle-list-draw-list-for-text))))
	       
(defun delete-all-from-medium (medium)
  (rm-dispatch-to-render-thread (medium draw-data)
    (let ((dl1 (draw-data-2d-triangle-list-draw-list draw-data))
	  (dl2 (draw-data-2d-line-list-draw-list draw-data)))
      (flet ((d (d)
	       (setf (draw-list-needs-compaction? d) nil)
	       (setf (fill-pointer (draw-list-cmd-vector d)) 0)
	       (setf (foreign-array-fill-pointer (draw-list-index-array d)) 0)
	       (setf (foreign-array-fill-pointer (draw-list-vertex-array d)) 0)))
	(d dl1)
	(d dl2)))))
