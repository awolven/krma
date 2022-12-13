(in-package :krma)

(eval-when (:compile-toplevel :load-toplevel)
  (when *muffle-compilation-notes*
    (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))))

(eval-when (:compile-toplevel :load-toplevel)
  (when krma::*debug*
    (declaim (optimize (safety 3) (debug 3))))
  (unless krma::*debug*
    (declaim (optimize (speed 3) (safety 0) (debug 0)))))

(defparameter +default-znear+ 0.001)
(defparameter +default-zfar+ 3000.0)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-concurrency))

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

(defcstruct vertex-uniform-buffer
  (view (:struct 3DMatrix))
  (proj (:struct 3DMatrix))
  (vproj (:struct 3DMatrix)))

(defun copy-matrix-to-foreign (lisp-matrix p-matrix)
  ;; glsl expects transpose of what is in marr of mat4
  (let ((array (marr lisp-matrix)))
    (loop for i from 0 below 4
	  do (loop for j from 0 below 4
		   do (setf (mem-aref p-matrix :float (+ j (* i 4)))
			    (clampf (aref array (+ i (* j 4)))))))
    (values)))

(defcstruct light
  (pos-x :float)
  (pos-y :float)
  (pos-z :float)
  (pos-w :float)
  (spot-direction-x :float)
  (spot-direction-y :float)
  (spot-direction-z :float)
  (spot-direction-w :float)
  (diffuse :unsigned-int)
  (specular :unsigned-int)
  (constant-attenuation :float)
  (linear-attenuation :float)
  (quadratic-attenuation :float)
  (spot-cutoff :float)
  (spot-exponent :float)
  (padding :float))

(defcstruct fragment-uniform-buffer
  (lights (:array (:struct light) 10))
  (num-lights :unsigned-int)
  (scene-ambient :unsigned-int)
  (padding1 :unsigned-int)
  (padding2 :unsigned-int))

(defclass light-mixin () ;; todo: add :type kwd to slot defs.
  ((position :initform *default-light-position* :initarg :position :accessor light-position :type (or vec3 vec4))
   (diffuse :initform *default-diffuse-color* :initarg :diffuse :accessor light-diffuse :type color)
   (specular :initform *default-specular-color* :initarg :specular :accessor light-specular :type color)
   (constant-attenuation :initform *default-constant-attenuation* :initarg :constant-attenuation :accessor light-constant-attenuation :type real)
   (linear-attenuation :initform *default-linear-attenuation* :initarg :linear-attenuation :accessor light-linear-attenuation :type real)
   (quadratic-attenuation :initform *default-quadratic-attenuation* :initarg :quadratic-attenuation :accessor light-quadratic-attenuation :type real)
   (spot-cutoff :initform *default-spot-cutoff* :initarg :spot-cutoff :accessor light-spot-cutoff :type real)
   (spot-exponent :initform *default-spot-exponent* :initarg :spot-exponent :accessor light-spot-exponent :type real)
   (spot-direction :initform *default-spot-direction* :initarg :spot-direction :accessor light-spot-direction :type (or vec3 vec4))))

(defclass directional-light (light-mixin) ())

(defclass point-light (light-mixin) ())
(defclass spot-light (light-mixin) ())

(defun update-fragment-uniform-buffer (pipeline scene)
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
										   (directional-light 0.0)
										   ((point-light spot-light) 1.0)
										   (t 0.0))
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
		       (copy-uniform-buffer-memory (default-logical-device pipeline)
						   p-stage
						   (allocated-memory (pipeline-fragment-uniform-buffer pipeline))
						   (load-time-value (foreign-type-size '(:struct fragment-uniform-buffer))))))))
  (values))

(defclass krma-essential-scene-mixin ()
  ((application :initarg :app)
   (im-draw-data :initform (make-immediate-mode-draw-data "IM Draw Data")
                 :reader im-draw-data)
   (rm-draw-data
    :reader rm-draw-data
    :initform (make-array 2 :initial-contents
                          (list
                           (make-retained-mode-draw-data "RM Draw Data 0")
                           (make-retained-mode-draw-data "RM Draw Data 1"))))
   (lights :initform (list (make-instance 'directional-light)) :accessor scene-lights)
   
   (3d-camera-projection-matrix)
   (3d-camera-view-matrix)
   (2d-camera-projection-matrix)
   (2d-camera-view-matrix)

   (ambient :initform *default-scene-ambient* :accessor scene-ambient)))

(defun finalize-scene (scene)
  (let ((draw-data (im-draw-data scene)))
    (sb-ext:finalize scene
                     #'(lambda ()
                         (%purge-im-groups-1 draw-data))
                     :dont-save t)))

(defmethod initialize-instance :after ((instance krma-essential-scene-mixin) &rest initargs)
  (declare (ignore initargs))
  (finalize-scene instance)
  (values))

(defclass standard-scene (krma-essential-scene-mixin) ())


(defmethod update-2d-camera (scene &optional
				     (proj (mortho-vulkan 0 (main-window-width *app*)
							  (main-window-height *app*) 0 0 1))
				     (view (meye 4)))
  (declare (type krma-essential-scene-mixin scene))
  (with-slots (2d-camera-projection-matrix
	       2d-camera-view-matrix)
      scene
    
    (setf 2d-camera-projection-matrix proj)
    (setf 2d-camera-view-matrix view)
    (values)))

(defmethod update-3d-camera (scene &optional
				     (proj (mperspective-vulkan 45 (/ (main-window-width *app*)
								      (main-window-height *app*))
								+default-znear+ +default-zfar+)
					   #+NIL(mortho-vulkan -1500 1500
							       (* -1500 (/ (main-window-height *app*)
									   (main-window-width *app*)))
							       (* 1500 (/ (main-window-height *app*)
									  (main-window-width *app*)))
							       +default-znear+ +default-zfar+))
				     (view (mlookat (vec3 0 0 1500) (vec3 0 0 0) (vec3 0 1 0))))
  (declare (type krma-essential-scene-mixin scene))
  (with-slots (3d-camera-projection-matrix
	       3d-camera-view-matrix)
      scene
    
    (setf 3d-camera-projection-matrix proj)
    (setf 3d-camera-view-matrix view)
    (values)))

(defmethod render-scene ((scene krma-essential-scene-mixin)
			 app command-buffer rm-draw-data im-draw-data)

  ;; todo: think about having separate clos objects for 3d-scene and 2d-scene
  
  (let ((device (default-logical-device app))
	(pipeline-store (application-pipeline-store app)))

    (with-slots (width height) app

      (with-slots (2d-camera-projection-matrix
		   2d-camera-view-matrix
		   3d-camera-projection-matrix
		   3d-camera-view-matrix)
	  scene

	(let ()

	  (loop for (p dl) on (3d-cmd-oriented-combinations pipeline-store rm-draw-data) by #'cddr
		do (render-draw-list-cmds p rm-draw-data dl app device command-buffer
					  scene
					  3d-camera-view-matrix 3d-camera-projection-matrix
					  width height))

	  (loop for (p dl) on (3d-draw-list-oriented-combinations pipeline-store rm-draw-data) by #'cddr
		do (render-draw-list p rm-draw-data dl app device command-buffer
				     scene
				     3d-camera-view-matrix 3d-camera-projection-matrix
				     width height))

	  (loop for (p dl) on (3d-cmd-oriented-combinations pipeline-store im-draw-data) by #'cddr
		do (render-draw-list-cmds p im-draw-data dl app device command-buffer
					  scene
					  3d-camera-view-matrix 3d-camera-projection-matrix
					  width height))
	  
	  (loop for (p dl) on (3d-draw-list-oriented-combinations pipeline-store im-draw-data) by #'cddr
		do (render-draw-list p im-draw-data dl app device command-buffer
				     scene
				     3d-camera-view-matrix 3d-camera-projection-matrix
				     width height))

	  (loop for (p dl) on (2d-cmd-oriented-combinations pipeline-store rm-draw-data) by #'cddr
		do (render-draw-list-cmds p rm-draw-data dl app device command-buffer
					  scene
					  2d-camera-view-matrix 2d-camera-projection-matrix
					  width height))

	  (loop for (p dl) on (2d-draw-list-oriented-combinations pipeline-store rm-draw-data) by #'cddr
		do (render-draw-list p rm-draw-data dl app device command-buffer
				     scene
				     2d-camera-view-matrix 2d-camera-projection-matrix
				     width height))

	  (loop for (p dl) on (2d-cmd-oriented-combinations pipeline-store im-draw-data) by #'cddr
		do (render-draw-list-cmds p im-draw-data dl app device command-buffer
					  scene
					  2d-camera-view-matrix 2d-camera-projection-matrix
					  width height))
	  
	  (loop for (p dl) on (2d-draw-list-oriented-combinations pipeline-store im-draw-data) by #'cddr
		do (render-draw-list p im-draw-data dl app device command-buffer
				     scene
				     2d-camera-view-matrix 2d-camera-projection-matrix
				     width height)))
	
	(values)))))

;; 2d-point
(defun scene-add-2d-point-primitive (scene group model-matrix point-size color x y &optional (object-id 0))
  "Retained-mode function, returns a handle for a 2d point primitive.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), point-size should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  and x and y must be real numbers.  Dispatches actual work to render thread.  To delete the point, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x y))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (or mat4 null) model-matrix))
  (declare (type atom group))
  ;; we try to run code which potentially errors outside of render-thread
  ;; the body of rm-dispatch-to-render-thread-with-handle becomes a closure
  (setq color (canonicalize-color color))
  (setq x (clampf x))
  (setq y (clampf y))
  (setq point-size (clampf point-size))
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-2d-point-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) point-size color x y)))

(defun scene-add-2d-point (scene group point-size color x y &optional (object-id 0))
  "Retained-mode function, returns no values.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, point-size should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  and x and y must be real numbers.  Dispatches actual work to render thread.  To delete the point, you must delete the entire group."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x y))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (setq x (clampf x))
  (setq y (clampf y))
  (setq point-size (clampf point-size))
  (rm-dispatch-to-render-thread (scene draw-data)
    (%draw-data-add-2d-point draw-data object-id group point-size color x y)))

(defun scene-draw-2d-point (scene group point-size color x y &optional (object-id 0))
  "Immediate-mode function, returns no values.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, point-size should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  and x and y must be real numbers.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x y point-size))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq point-size (clampf point-size))
  (let ((draw-data (im-draw-data scene)))
    (declare (type standard-draw-data draw-data))
    (%draw-data-draw-2d-point draw-data object-id group point-size (canonicalize-color color) (clampf x) (clampf y))))

;; 3d-point
(defun scene-add-3d-point-primitive (scene group model-matrix point-size color x y z &optional (object-id 0))
  "Retained-mode function, returns a handle for a 3d point primitive.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), point-size should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, and x, y and z must be real numbers. Dispatches actual work to render thread.  To delete the point, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x y z point-size))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (setq x (clampf x))
  (setq y (clampf y))
  (setq z (clampf z))
  (setq point-size (clampf point-size))
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-3d-point-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) point-size color x y z)))

(defun scene-add-3d-point (scene group point-size color x y z &optional (object-id 0))
  "Retained-mode function, adds a point to retained-mode draw-lists, returns no values.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, point-size should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer,  and x and y must be real numbers.  Dispatches actual work to render thread.  To delete the point, you must delete the entire group."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x y z point-size))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (setq x (clampf x))
  (setq y (clampf y))
  (setq z (clampf z))
  (setq point-size (clampf point-size))
  (rm-dispatch-to-render-thread (scene draw-data)
    (%draw-data-add-3d-point draw-data object-id group point-size color x y z)))

(defun scene-draw-3d-point (scene group point-size color x y z &optional (object-id 0))
  "Immediate-mode function, draws a 3d point, returns no values.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, point-size should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer,  and x, y and z must be real numbers.  Performs work in current thread, which should be the render thread."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x y z))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-3d-point draw-data object-id group (clampf point-size)
			      (canonicalize-color color) (clampf x) (clampf y) (clampf z))))

;; 2d-line
(defun scene-add-2d-line-primitive (scene group model-matrix line-thickness color x0 y0 x1 y1 &optional (object-id 0))
  "Retained-mode function, returns a handle for a 2d line primitive.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  x0, y0 and x1, y1  must be real numbers which represent the endpoints of the line.  Dispatches actual work to render thread.  To delete the line segment, you must delete the primitive using the handle."
  (declare (type krma-essential-scene-mixin scene))
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
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-2d-line-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) line-thickness color x0 y0 x1 y1)))

(defun scene-add-2d-line (scene group line-thickness color x0 y0 x1 y1 &optional (object-id 0))
  "Retained-mode function, adds a 2d line segment to draw lists.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom,  line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  x0, y0 and x1, y1  must be real numbers which represent the endpoints of the line.  Dispatches actual work to render thread.  To delete the line segment, you must delete the entire group."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x0 y0 x1 y1 line-thickness))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (setq x0 (clampf x0))
  (setq y0 (clampf y0))
  (setq x1 (clampf x1))
  (setq y1 (clampf y1))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data)
    (%draw-data-add-2d-line draw-data object-id group line-thickness color x0 y0 x1 y1)))

(defun scene-draw-2d-line (scene group line-thickness color x0 y0 x1 y1 &optional (object-id 0))
  "Immediate-mode function, draws a 2d line segment.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  x0, y0 and x1, y1  must be real numbers which represent the endpoints of the line.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x0 y0 x1 y1 line-thickness))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-2d-line draw-data object-id group (clampf line-thickness)
			     (canonicalize-color color) (clampf x0) (clampf y0) (clampf x1) (clampf y1))))

;; 3d-line
(defun scene-add-3d-line-primitive (scene group model-matrix line-thickness color x0 y0 z0 x1 y1 z1 &optional (object-id 0))
  "Retained-mode function, returns a handle for a 3d line primitive.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  x0, y0, z0 and x1, y1, z1  must be real numbers which represent the endpoints of the line.  Dispatches actual work to render thread.  To delete the line segment, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
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
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-3d-line-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) line-thickness color x0 y0 z0 x1 y1 z1)))

(defun scene-add-3d-line (scene group line-thickness color x0 y0 z0 x1 y1 z1 &optional (object-id 0))
  "Retained-mode function, adds a 3d line segment to the draw lists.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom,  line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  x0, y0, z0 and x1, y1, z1  must be real numbers which represent the endpoints of the line.  Dispatches actual work to render thread.  To delete the line you must delete the entire group."
  (declare (type krma-essential-scene-mixin scene))
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
  (rm-dispatch-to-render-thread (scene draw-data)
    (%draw-data-add-3d-line draw-data object-id group line-thickness color x0 y0 z0 x1 y1 z1)))

(defun scene-draw-3d-line (scene group line-thickness color x0 y0 z0 x1 y1 z1 &optional (object-id 0))
  "Retained-mode function, adds a 3d line segment to the draw lists.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom,  line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  x0, y0, z0 and x1, y1, z1  must be real numbers which represent the endpoints of the line.  Dispatches actual work to render thread.  To delete the line you must delete the entire group."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x0 y0 x0 x1 y1 z1 line-thickness))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-3d-line draw-data object-id group (clampf line-thickness) (canonicalize-color color)
			     (clampf x0) (clampf y0) (clampf z0)
			     (clampf x1) (clampf y1) (clampf z1))))

;; 2d-polyline
(defun scene-add-2d-polyline-primitive (scene group model-matrix closed? line-thickness color vertices &optional (object-id 0))
  "Retained-mode function, returns a handle for a 2d polyline primitive.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  vertices should be of the form (list x0 y0 x1 y1 ... xn yn) where the x's and the y's are vertex points of the polyline and must be real numbers.   Dispatches actual work to render thread.  To delete the polyline, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-2d-polyline-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) closed? line-thickness color vertices)))

(defun scene-add-2d-polyline (scene group closed? line-thickness color vertices &optional (object-id 0))
  "Retained-mode function, adds a 2d polyline to the draw lists.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom,  closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  vertices should be of the form (list x0 y0 x1 y1 ... xn yn) where the x's and the y's are vertex points of the polyline and must be real numbers.   Dispatches actual work to render thread.  To delete the polyline, you must delete the entire group."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data)
    (%draw-data-add-2d-polyline draw-data object-id group closed? line-thickness color vertices)))

(defun scene-draw-2d-polyline (scene group closed? line-thickness color vertices &optional (object-id 0))
  "Immediate-mode function, draws a 2d polyline.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom,  closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  vertices should be of the form (list x0 y0 x1 y1 ... xn yn) where the x's and the y's are vertex points of the polyline and must be real numbers.   Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-2d-polyline draw-data object-id group closed? (clampf line-thickness) (canonicalize-color color) vertices)))

;; 2d-triangle
(defun scene-add-2d-triangle-primitive (scene group model-matrix line-thickness color x0 y0 x1 y1 x2 y2 &optional (object-id 0))
  "Retained-mode function, returns a handle for a 2d triangle outline primitive.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity),  line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  x0, y0, x1, y1, x2 and y2 are the three vertex coordinates of the triangle and must be real numbers.   Dispatches actual work to render thread.  To delete the triangle, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
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
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-2d-polyline-primitive draw-data handle object-id group
					  (when model-matrix (mcopy model-matrix)) t line-thickness color
					  (list x0 y0 x1 y1 x2 y2))))

(defun scene-add-2d-triangle (scene group line-thickness color x0 y0 x1 y1 x2 y2 &optional (object-id 0))
  "Retained-mode function, adds a 2d triangle outline to the draw lists, returns no values.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an a non-null atom,  line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  x0, y0, x1, y1, x2 and y2 are the three vertex coordinates of the triangle and must be real numbers.   Dispatches actual work to render thread.  To delete the triangle, you must delete the entire group."
  (declare (type krma-essential-scene-mixin scene))
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
  (rm-dispatch-to-render-thread (scene draw-data)
    (%draw-data-add-2d-polyline draw-data object-id group t line-thickness color
				(list x0 y0 x1 y1 x2 y2))))

(defun scene-draw-2d-triangle (scene group line-thickness color x0 y0 x1 y1 x2 y2 &optional (object-id 0))
  "Immediate-mode function, draws a 2d triangle outline, returns no values.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an a non-null atom,  line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  x0, y0, x1, y1, x2 and y2 are the three vertex coordinates of the triangle and must be real numbers.   Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x0 y0 x1 y1 x2 y2 line-thickness))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-2d-polyline draw-data object-id group t (clampf line-thickness) (canonicalize-color color)
				 (list x0 y0 x1 y1 x2 y2))))

;; 2d-rectangle
(defun scene-add-2d-rectangle-primitive (scene group model-matrix line-thickness color x0 y0 x1 y1 &optional (object-id 0))
  "Retained-mode function, returns a handle for a 2d rectangle outline primitive.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity),  line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  x0, y0, x1, and y1 are the top-left and bottom-right corners of the rectangle and must be real numbers.   Dispatches actual work to render thread.  To delete the rectangle, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
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
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-2d-polyline-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) t line-thickness color
					  (list x0 y0 x0 y1 x1 y1 x1 y0))))


(defun scene-add-2d-rectangle (scene group line-thickness color x0 y0 x1 y1 &optional (object-id 0))
  "Retained-mode function, adds a 2d rectangle outline to the draw lists, returns no values.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an a non-null atom,  line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  x0, y0, x1, and y1 are the top-left and bottom-right corners of the rectangle and must be real numbers.   Dispatches actual work to render thread.  To delete the rectangle, you must delete the entire group."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x0 y0 x1 y1 line-thickness))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (setq x0 (clampf x0))
  (setq y0 (clampf y0))
  (setq x1 (clampf x1))
  (setq y1 (clampf y1))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data)
    (%draw-data-add-2d-polyline draw-data object-id group t line-thickness color
				(list x0 y0 x0 y1 x1 y1 x1 y0))))

(defun scene-draw-2d-rectangle (scene group line-thickness color x0 y0 x1 y1 &optional (object-id 0))
  "Immediate-mode function, draws a 2d rectangle outline, returns no values.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an a non-null atom,  line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  x0, y0, x1, and y1 are the top-left and bottom-right corners of the rectangle and must be real numbers.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x0 y0 x1 y1 line-thickness))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data scene)))
    (setq x0 (clampf x0))
    (setq y0 (clampf y0))
    (setq x1 (clampf x1))
    (setq y1 (clampf y1))
    (%draw-data-draw-2d-polyline draw-data object-id group t (clampf line-thickness) (canonicalize-color color)
				 (list x0 y0 x0 y1 x1 y1 x1 y0))))

;; multicolor-2d-polyline
(defun scene-add-multicolor-2d-polyline-primitive (scene group model-matrix closed? line-thickness vertices &optional (object-id 0))
  "Retained-mode function, returns a handle for a multicolored 2d polyline primitive.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number.  vertices should be of the form (list x0 y0 color0 x1 y1 color1 ... xn yn colorn) where the x's and the y's are vertex points of the polyline and must be real numbers, color values can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.   Dispatches actual work to render thread.  To delete the polyline, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-multicolor-2d-polyline-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) closed? line-thickness vertices)))

(defun scene-add-multicolor-2d-polyline (scene group closed? line-thickness vertices &optional (object-id 0))
  "Retained-mode function, adds a multicolored 2d polyline to the draw lists, returns no values.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom,  closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number.  vertices should be of the form (list x0 y0 color0 x1 y1 color1 ... xn yn colorn) where the x's and the y's are vertex points of the polyline and must be real numbers, color values can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.   Dispatches actual work to render thread.  To delete the polyline, you must delete the entire group."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (declare (type (and atom t) group))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data)
    (%draw-data-add-multicolor-2d-polyline draw-data object-id group closed? line-thickness vertices)))

(defun scene-draw-multicolor-2d-polyline (scene group closed? line-thickness vertices &optional (object-id 0))
  "Immediate-mode function, draws a multicolored 2d polyline, returns no values.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom,  closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number.  vertices should be of the form (list x0 y0 color0 x1 y1 color1 ... xn yn colorn) where the x's and the y's are vertex points of the polyline and must be real numbers, color values can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-multicolor-2d-polyline draw-data object-id group closed? (clampf line-thickness) vertices)))

;; 2d-circular-arc
(defun scene-add-2d-circular-arc-primitive (scene group model-matrix closed? line-thickness color
                                            center-x center-y radius start-angle end-angle
                                            number-of-segments &optional (object-id 0))
  "Retained-mode function, returns a handle for a 2d circular arc outline primitive.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity),  closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number.  color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer. center-x, and center-y must be real numbers, radius must be a positive real number, start-angle and end-angle are real numbers, measured in radians.  number-of-segments must be a positive integer, and defaults to 64.  Dispatches actual work to render thread.  To delete the arc, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
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
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-2d-circular-arc-primitive draw-data handle object-id group
					      (when model-matrix (mcopy model-matrix)) closed? line-thickness color
					      center-x center-y radius start-angle end-angle
					      number-of-segments)))

(defun scene-add-2d-circular-arc (scene group closed? line-thickness color
				  center-x center-y radius start-angle end-angle
				  number-of-segments &optional (object-id 0))
  "Retained-mode function, adds a 2d circular arc outline to the draw lists, returns no values.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number.  color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer. center-x, and center-y must be real numbers, radius must be a positive real number, start-angle and end-angle are real numbers, measured in radians.  number-of-segments must be a positive integer, and defaults to 64.  Dispatches actual work to render thread.  To delete the arc, you must delete the entire group."
  (declare (type krma-essential-scene-mixin scene))
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
  (rm-dispatch-to-render-thread (scene draw-data)
    (%draw-data-add-2d-circular-arc draw-data object-id group closed? line-thickness color
				    center-x center-y radius start-angle end-angle
				    number-of-segments)))

(defun scene-draw-2d-circular-arc (scene group closed? line-thickness color
				   center-x center-y radius start-angle end-angle
                                   number-of-segments &optional (object-id 0))
  "Immediate-mode function, draws 2d circular arc outline, returns no values.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number.  color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer. center-x, and center-y must be real numbers, radius must be a positive real number, start-angle and end-angle are real numbers, measured in radians.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type boolean closed?))
  (declare (type real center-x center-y radius start-angle end-angle))
  (declare (type (integer 1 #.most-positive-fixnum) number-of-segments))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-2d-circular-arc draw-data object-id group closed? (clampf line-thickness) (canonicalize-color color)
				     (coerce center-x 'double-float) (coerce center-y 'double-float)
				     (coerce radius 'double-float)
				     (coerce start-angle 'double-float) (coerce end-angle 'double-float)
				     number-of-segments)))

;; 2d-circle
(defun scene-add-2d-circle-primitive (scene group model-matrix line-thickness color
                                      center-x center-y radius
                                      number-of-segments &optional (object-id 0))
  "Retained-mode function, returns a handle for a 2d circle outline primitive.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity),  line-thickness should be a positive real number.  color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer. center-x, and center-y must be real numbers, radius must be a positive real number.  number-of-segments must be a positive integer, and defaults to 64. Dispatches actual work to render thread.  To delete the arc, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
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
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-2d-circle-primitive draw-data handle object-id group
					(when model-matrix (mcopy model-matrix)) line-thickness color
					center-x center-y radius
					number-of-segments)))

(defun scene-add-2d-circle (scene group line-thickness color
			    center-x center-y radius
			    number-of-segments &optional (object-id 0))
  "Retained-mode function, adds a 2d circle outline to the draw lists, returns no values.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, line-thickness should be a positive real number.  color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer. center-x, and center-y must be real numbers, radius must be a positive real number.  number-of-segments must be a positive integer, and defaults to 64.  Dispatches actual work to render thread.  To delete the arc, you must delete the entire group."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real center-x center-y radius line-thickness))
  (declare (type (integer 1 #.most-positive-fixnum) number-of-segments))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (setq center-x (coerce center-x 'double-float))
  (setq center-y (coerce center-y 'double-float))
  (setq radius (coerce radius 'double-float))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data)
    (%draw-data-add-2d-circle draw-data object-id
			      group line-thickness color
			      center-x center-y radius
			      number-of-segments)))

(defun scene-draw-2d-circle (scene group line-thickness color
			     center-x center-y radius
                             number-of-segments &optional (object-id 0))
  "Immediate-mode function, draws a 2d circle outline, returns no values.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, line-thickness should be a positive real number.  color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer. center-x, and center-y must be real numbers, radius must be a positive real number.  number-of-segments must be a positive integer, and defaults to 64.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real center-x center-y radius))
  (declare (type (integer 1 #.most-positive-fixnum) number-of-segments))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-2d-circle draw-data object-id group (clampf line-thickness) (canonicalize-color color)
			       (coerce center-x 'double-float) (coerce center-y 'double-float)
			       (coerce radius 'double-float)
			       number-of-segments)))

;; 3d-polyline
(defun scene-add-3d-polyline-primitive (scene group model-matrix closed? line-thickness color vertices &optional (object-id 0))
  "Retained-mode function, returns a handle for a 3d polyline primitive.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  vertices should be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn) where the x, y and z's are the vertex points of the polyline and must be real numbers.   Dispatches actual work to render thread.  To delete the polyline, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real line-thickness))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-3d-polyline-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) closed? line-thickness color vertices)))

(defun scene-add-3d-polyline (scene group closed? line-thickness color vertices &optional (object-id 0))
  "Retained-mode function, adds a 3d polyline to the draw lists.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom,  closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  vertices should be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn) where the x, y and z's are the vertex points of the polyline and must be real numbers.   Dispatches actual work to render thread.  To delete the polyline, you must delete the entire group."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real line-thickness))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data)
    (%draw-data-add-3d-polyline draw-data object-id group closed? line-thickness color vertices)))

(defun scene-draw-3d-polyline (scene group closed? line-thickness color vertices &optional (object-id 0))
  "Immediate-mode function, draws a 3d polyline.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom,  closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  vertices should be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn) where the x, y and z's are the vertex points of the polyline and must be real numbers.   Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-3d-polyline draw-data object-id group closed? (clampf line-thickness) (canonicalize-color color) vertices)))

;; multicolor-3d-polyline
(defun scene-add-multicolor-3d-polyline-primitive (scene group model-matrix closed? line-thickness vertices &optional (object-id 0))
  "Retained-mode function, returns a handle for a multicolored 3d polyline primitive.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number.  vertices should be of the form (list x0 y0 z0 color0 x1 y1 z1 color1 ... xn yn zn colorn) where the x, y and z's are vertex points of the polyline and must be real numbers, color values can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.   Dispatches actual work to render thread.  To delete the polyline, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-multicolor-3d-polyline-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) closed? line-thickness vertices)))

(defun scene-add-multicolor-3d-polyline (scene group closed? line-thickness vertices &optional (object-id 0))
  "Retained-mode function, adds a multicolored 3d polyline to the draw lists, returns no values.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom,  closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number.  vertices should be of the form (list x0 y0 z0 color0 x1 y1 z1 color1 ... xn yn zn colorn) where the x, y and z's are vertex points of the polyline and must be real numbers, color values can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.   Dispatches actual work to render thread.  To delete the polyline, you must delete the entire group."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data)
    (%draw-data-add-multicolor-3d-polyline draw-data object-id group closed? line-thickness vertices)))

(defun scene-draw-multicolor-3d-polyline (scene group closed? line-thickness vertices &optional (object-id 0))
  "Immediate-mode function, draws a multicolored 3d polyline, returns no values.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom,  closed? should be a boolean, which specifies whether to draw a segment between the last vertex and the first vertex, line-thickness should be a positive real number.  vertices should be of the form (list x0 y0 z0 color0 x1 y1 z1 color1 ... xn yn zn colorn) where the x, y and z's are vertex points of the polyline and must be real numbers, color values can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-multicolor-3d-polyline draw-data object-id group closed? (clampf line-thickness) vertices)))

;; filled-2d-triangle-list
(defun scene-add-filled-2d-triangle-list-primitive (scene group model-matrix color vertices &optional (object-id 0))
  "Retained-mode function, returns a handle for a filled 2d triangle list primitive.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity),   color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00  x10 y10 x20 y20 x01 y01 x11 y11 x21 y21 ... x0n y0n x1n y1n x2n y2n) where the x and y values represent vertices of a triangle in a series of triangles and must be real numbers,    Dispatches actual work to render thread.  To delete the triangle list, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-filled-2d-triangle-list-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) color vertices)))

(defun scene-add-filled-2d-triangle-list (scene group color vertices &optional (object-id 0))
  "Retained-mode function, adds a filled 2d triangle list to the draw-lists.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00  x10 y10 x20 y20 x01 y01 x11 y11 x21 y21 ... x0n y0n x1n y1n x2n y2n) where the x and y values represent vertices of a triangle in a series of triangles and must be real numbers,    Dispatches actual work to render thread.  To delete the triangle list, you must delete the entire group."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data)
    (%draw-data-add-filled-2d-triangle-list draw-data object-id group color vertices)))

(defun scene-draw-filled-2d-triangle-list (scene group color vertices &optional (object-id 0))
  "Immediate-mode function, draws a filled 2d triangle list.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00  x10 y10 x20 y20 x01 y01 x11 y11 x21 y21 ... x0n y0n x1n y1n x2n y2n) where the x and y values represent vertices of a triangle in a series of triangles and must be real numbers,   Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-filled-2d-triangle-list draw-data object-id group color vertices)))

;; filled-2d-triangle-strip
(defun scene-add-filled-2d-triangle-strip-primitive (scene group model-matrix color vertices &optional (object-id 0))
  "Retained-mode function, returns a handle for a filled 2d triangle strip primitive.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity),   color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x0 y0  x1 y1 ... xn yn) where the x and y values represent successive vertices of a triangle strip and must be real numbers,    Dispatches actual work to render thread.  To delete the triangle strip, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-filled-2d-triangle-strip-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) color vertices)))

(defun scene-draw-filled-2d-triangle-strip (scene group color vertices &optional (object-id 0))
  "Immediate-mode function, draws a filled 2d triangle strip.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom,    color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x0 y0  x1 y1 ... xn yn) where the x and y values represent successive vertices of a triangle strip and must be real numbers.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data scene)))
    (declare (type immediate-mode-draw-data draw-data))
    (let ((draw-list (draw-data-2d-triangle-strip-draw-list draw-data)))
      ;; we add the primitive/cmd without a handle:
      (%draw-list-add-filled-2d-triangle-strip/list draw-list object-id group nil (canonicalize-color color) vertices))))

;; filled-2d-rectangle-list
(defun scene-add-filled-2d-rectangle-list-primitive (scene group model-matrix color vertices &optional (object-id 0))
  "Retained-mode function, returns a handle for a filled 2d rectangle list primitive.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00 x10 y10 x10 y10 x11 y11 ... x0n y0n x1n y1n) where  each pair of successive x and y's represent the top-left corner followed by the bottom-right corner of each rectangle and must be real numbers.  Dispatches actual work to render thread.  To delete the rectangle list, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-filled-2d-rectangle-list-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) color vertices)))

(defun scene-add-filled-2d-rectangle-list (scene group color vertices &optional (object-id 0))
  "Retained-mode function, adds a  filled 2d rectangle list to the draw-lists.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00 x10 y10 x10 y10 x11 y11 ... x0n y0n x1n y1n) where  each pair of successive x and y's represent the top-left corner followed by the bottom-right corner of each rectangle and must be real numbers.  Dispatches actual work to render thread.  To delete the rectangle list, you must delete the entire group."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data)
    (%draw-data-add-filled-2d-rectangle-list draw-data object-id group color vertices)))

(defun scene-draw-filled-2d-rectangle-list (scene group color vertices &optional (object-id 0))
  "Immediate-mode function, draws a  filled 2d rectangle list.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00 x10 y10 x10 y10 x11 y11 ... x0n y0n x1n y1n) where  each pair of successive x and y's represent the top-left corner followed by the bottom-right corner of each rectangle and must be real numbers.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-filled-2d-rectangle-list draw-data object-id group (canonicalize-color color) vertices)))

;; textured-2d-rectangle-list
(defun scene-add-textured-2d-rectangle-list-primitive (scene group model-matrix texture color vertices &optional (object-id 0))
  "Retained-mode function, returns a handle for a textured 2d rectangle list primitive.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), texture should be a texture such as return from make-vulkan-texture, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00 u00 v00 x10 y10 u10 v10 x01 y01 u01 v01 x11 y11 u11 v11 ... x0n y0n u0n v0n x1n y1n u1n v1n) where  each pair of successive x, y, u and v represent the top-left corner followed by the bottom-right corner of each rectangle with their normalized texture coordinates, and must be real numbers.  There must be at least one pair of the sequence x, y, u, v.  Dispatches actual work to render thread.  To delete the rectangle list, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-textured-2d-rectangle-list-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) texture color vertices)))

(defun scene-add-textured-2d-rectangle-list (scene group texture color vertices &optional (object-id 0))
  "Retained-mode function, adds a textured 2d rectangle list to the draw-lists.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, texture should be a texture such as return from make-vulkan-texture, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00 u00 v00 x10 y10 u10 v10 x01 y01 u01 v01 x11 y11 u11 v11 ... x0n y0n u0n v0n x1n y1n u1n v1n) where  each pair of successive x, y, u and v represent the top-left corner followed by the bottom-right corner of each rectangle with their normalized texture coordinates, and must be real numbers.  There must be at least one pair of the sequence x, y, u, v.  Dispatches actual work to render thread.  To delete the rectangle list, you must delete the entire group."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data)
    (%draw-data-add-textured-2d-rectangle-list draw-data object-id group texture color vertices)))

(defun scene-draw-textured-2d-rectangle-list (scene group texture color vertices &optional (object-id 0))
  "Immediate-mode function, draws a textured 2d rectangle list.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, texture should be a texture such as return from make-vulkan-texture, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00 u00 v00 x10 y10 u10 v10 x01 y01 u01 v01 x11 y11 u11 v11 ... x0n y0n u0n v0n x1n y1n u1n v1n) where  each pair of successive x, y, u and v represent the top-left corner followed by the bottom-right corner of each rectangle with their normalized texture coordinates, and must be real numbers.  There must be at least one pair of the sequence x, y, u, v.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (%draw-data-draw-textured-2d-rectangle-list (im-draw-data scene) object-id group texture (canonicalize-color color) vertices))

;; filled-2d-convex-polygon
(defun scene-add-filled-2d-convex-polygon-primitive (scene group model-matrix color vertices &optional (object-id 0))
  "Retained-mode function, returns a handle for a filled 2d convex polygon primitive.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x0 y0 x1 y1 ... xn yn)  where each successive x and y are the vertices of the polygon, and must be real numbers.  There must be at least three x, y pairs in vertices.  Dispatches actual work to render thread.  To delete the polygon, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-filled-2d-convex-polygon-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) color vertices)))

(defun scene-add-filled-2d-convex-polygon (scene group color vertices &optional (object-id 0))
  "Retained-mode function, adds a filled 2d convex polygon to the draw-lists, returns no values.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x0 y0 x1 y1 ... xn yn)  where each successive x and y are the vertices of the polygon, and must be real numbers.  There must be at least three x, y pairs in vertices.  Dispatches actual work to render thread.  To delete the polygon, you must delete the entire group."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data)
    (%draw-data-add-filled-2d-convex-polygon draw-data object-id group color vertices)))

(defun scene-draw-filled-2d-convex-polygon (scene group color vertices &optional (object-id 0))
  "Immediate-mode function, draws a filled 2d convex polygon, returns no values.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x0 y0 x1 y1 ... xn yn)  where each successive x and y are the vertices of the polygon, and must be real numbers.  There must be at least three x, y pairs in vertices.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (%draw-data-draw-filled-2d-convex-polygon (im-draw-data scene) object-id group (canonicalize-color color) vertices))

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

(defun scene-add-filled-2d-circle-primitive (scene group model-matrix color
                                             center-x center-y radius
					     number-of-sectors &optional (object-id 0))
  "Retained-mode function, returns a handle for a filled 2d circle primitive.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, center-x and center-y should be real numbers, radius should be a positive real number, number-of-sectors defaults to 64.  Dispatches actual work to render thread.  To delete the circle, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real center-x center-y radius))
  (declare (type (integer 1 #.most-positive-fixnum) number-of-sectors))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq center-x (coerce center-x 'double-float))
  (setq center-y (coerce center-y 'double-float))
  (setq radius (coerce radius 'double-float))
  (let ((vertices (compute-circle-vertices number-of-sectors center-x center-y radius)))
    (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
      (%draw-data-add-filled-2d-convex-polygon-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) color vertices))))

(defun scene-add-filled-2d-circle (scene group color
				   center-x center-y radius
				   number-of-sectors &optional (object-id 0))
  "Retained-mode function, adds a filled 2d circle to the draw lists, returns no values.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, center-x and center-y should be real numbers, radius should be a positive real number, number-of-sectors defaults to 64.  Dispatches actual work to render thread.  To delete the circle, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real center-x center-y radius))
  (declare (type (integer 1 #.most-positive-fixnum) number-of-sectors))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq center-x (coerce center-x 'double-float))
  (setq center-y (coerce center-y 'double-float))
  (setq radius (coerce radius 'double-float))
  (let ((vertices (compute-circle-vertices number-of-sectors center-x center-y radius)))
    (rm-dispatch-to-render-thread (scene draw-data)
      (%draw-data-add-filled-2d-convex-polygon draw-data object-id group color vertices))))

(defun scene-draw-filled-2d-circle (scene color group
				    center-x center-y radius
				    number-of-segments &optional (object-id 0))
  "Immediate-mode function, draws a filled 2d circle.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, center-x and center-y should be real numbers, radius should be a positive real number, number-of-sectors defaults to 64.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real center-x center-y radius))
  (declare (type (integer 1 #.most-positive-fixnum) number-of-segments))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq center-x (coerce center-x 'double-float))
  (setq center-y (coerce center-y 'double-float))
  (setq radius (coerce radius 'double-float))
  (let ((vertices (compute-circle-vertices number-of-segments center-x center-y radius)))
    (%draw-data-draw-filled-2d-convex-polygon (im-draw-data scene) object-id group (canonicalize-color color) vertices)))

;; filled-3d-triangle-list-flat
(defun scene-add-filled-3d-triangle-list-primitive-flat (scene group model-matrix color vertices &optional (object-id 0))
  "Retained-mode function, returns a handle for a filled 3d triangle list primitive.  Displays with flat shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00 z00 x10 y10 z10 x20 y20 z20 x01 y01 z01 x11 y11 z11 x21 y21 z21... x0n y0n z0n x1n y1n z1n x2n y2n z2n) where the x, y and z values represent vertices of a triangle in a series of triangles and must be real numbers.  There must be at least three sets of x, y and z, and additional vertices come as 3 sets each.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the triangle list, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-filled-3d-triangle-list-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) color vertices)))

(defun scene-add-filled-3d-triangle-list-flat (scene group color vertices &optional (object-id 0))
  "Retained-mode function, adds a filled 3d triangle list to the draw-lists, returns no values.  Displays with flat shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom,    color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00  z00 x10 y10 z10 x20 y20 z20 x01 y01 z01 x11 y11 z11 x21 y21 z21... x0n y0n z0n x1n y1n z1n x2n y2n z2n) where the x, y and z values represent vertices of a triangle in a series of triangles and must be real numbers.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the triangle list, you must delete the entire group."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data)
    (%draw-data-add-filled-3d-triangle-list draw-data object-id group color vertices)))

(defun scene-draw-filled-3d-triangle-list-flat (scene group color vertices &optional (object-id 0))
  "Retained-mode function, adds a filled 3d triangle list to the draw-lists, returns no values.  Displays with flat shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom,    color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00  z00 x10 y10 z10 x20 y20 z20 x01 y01 z01 x11 y11 z11 x21 y21 z21... x0n y0n z0n x1n y1n z1n x2n y2n z2n) where the x, y and z values represent vertices of a triangle in a series of triangles and must be real numbers.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the triangle list, you must delete the entire group."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (%draw-data-draw-filled-3d-triangle-list (im-draw-data scene) object-id group (canonicalize-color color) vertices))

;; filled-3d-triangle-list-diffuse
(defun scene-add-filled-3d-triangle-list-primitive-diffuse (scene group model-matrix color vertices light-position &optional (object-id 0))
  "Retained-mode function, returns a handle for a filled 3d triangle list primitive.  Displays with diffuse shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00 z00 x10 y10 z10 x20 y20 z20 x01 y01 z01 x11 y11 z11 x21 y21 z21... x0n y0n z0n x1n y1n z1n x2n y2n z2n) where the x, y and z values represent vertices of a triangle in a series of triangles and must be real numbers.  There must be at least three sets of x, y and z, and additional vertices come as 3 sets each.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the triangle list, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (or vec3 null) light-position))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-filled-3d-triangle-list-with-normals-primitive
     draw-data handle object-id group (when model-matrix (mcopy model-matrix)) color vertices (when light-position (vcopy light-position)))))

(defun scene-add-filled-3d-triangle-list-diffuse (scene group color vertices &optional (object-id 0))
  "Retained-mode function, adds a filled 3d triangle list to the draw-lists, returns no values.  Displays with diffuse shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00  z00 x10 y10 z10 x20 y20 z20 x01 y01 z01 x11 y11 z11 x21 y21 z21... x0n y0n z0n x1n y1n z1n x2n y2n z2n) where the x, y and z values represent vertices of a triangle in a series of triangles and must be real numbers.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the triangle list, you must delete the entire group."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data)
    (%draw-data-add-filled-3d-triangle-list-with-normals draw-data object-id group color vertices)))

(defun scene-draw-filled-3d-triangle-list-diffuse (scene group color vertices &optional (object-id 0))
  "Immediate-mode function, draws a filled 2d triangle list, returns no values.  Displays with diffuse shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x00 y00  z00 x10 y10 z10 x20 y20 z20 x01 y01 z01 x11 y11 z11 x21 y21 z21... x0n y0n z0n x1n y1n z1n x2n y2n z2n) where the x, y and z values represent vertices of a triangle in a series of triangles and must be real numbers.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (%draw-data-draw-filled-3d-triangle-list-with-normals (im-draw-data scene) object-id group (canonicalize-color color) vertices))

;; filled-3d-triangle-strip-flat
(defun scene-add-filled-3d-triangle-strip-primitive-flat (scene group model-matrix color vertices &optional (object-id 0))
  "Retained-mode function, returns a handle for a filled 3d triangle strip primitive.  Displays with flat shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn) where the x, y and z values represent successive vertices of a triangle strip and must be real numbers.  There must be at least three vertices.  Dispatches actual work to render thread.  To delete the triangle strip, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-filled-3d-triangle-strip-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) color vertices)))

;; triangle strips do not have pseudo-cmds and therefore will
;; not have scene-add-filled-3d-triangle-strip-flat

(defun scene-draw-filled-3d-triangle-strip-flat (scene group color vertices &optional (object-id 0))
  "Immediate-mode function, draws a filled 3d triangle strip, returns no values.  Displays with flat shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn) where the x, y and z values represent successive vertices of a triangle strip and must be real numbers.  There must be at least 3 vertices.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data scene)))
    (declare (type immediate-mode-draw-data draw-data))
    (let ((draw-list (draw-data-3d-triangle-strip-draw-list draw-data)))
      ;; we add the primitive/cmd without a handle:
      (%draw-list-add-filled-3d-triangle-strip/list
       draw-list object-id group nil (canonicalize-color color) vertices))))

;; filled-3d-triangle-strip-diffuse
(defun scene-add-filled-3d-triangle-strip-primitive-diffuse (scene group model-matrix color vertices light-position &optional (object-id 0))
  "Retained-mode function, returns a handle for a filled 3d triangle strip primitive.  Displays with diffuse shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity),   color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x0 y0 z0  x1 y1 z1 ... xn yn zn) where the x, y and z values represent successive vertices of a triangle strip and must be real numbers.  There must be at least three vertices.  light-position must either be a 3d-vectors:vec4 or null.  Dispatches actual work to render thread.  To delete the triangle strip, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (or vec3 null) light-position))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-filled-3d-triangle-strip-with-normals-primitive
     draw-data handle object-id group (when model-matrix (mcopy model-matrix)) color vertices (when light-position (vcopy light-position)))))

(defun scene-draw-filled-3d-triangle-strip-diffuse (scene group color vertices &optional (object-id 0))
  "Retained-mode function, returns a handle for a filled 3d triangle strip primitive.  Displays with diffuse shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x0 y0 z0  x1 y1 z1 ... xn yn zn) where the x, y and z values represent successive vertices of a triangle strip and must be real numbers.  There must be at least three vertices.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data scene)))
    (declare (type immediate-mode-draw-data draw-data))
    (let ((draw-list (draw-data-3d-triangle-strip-with-normals-draw-list draw-data)))
      ;; we add the primitive/cmd without a handle:
      (%draw-list-add-filled-3d-triangle-strip/list-with-normals
       draw-list object-id group nil (canonicalize-color color) vertices nil))))

;; filled-3d-convex-polygon-diffuse
(defun scene-add-filled-3d-convex-polygon-primitive-diffuse (scene group model-matrix color vertices light-position &optional (object-id 0))
  "Retained-mode function, returns a handle for a filled 3d convex polygon primitive.  Displays with diffuse shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer  vertices must be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn)  where each successive x, y and z are the vertices of the polygon, and must be real numbers.  There must be at least three x, y, z triplets in vertices.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the polygon, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (or vec3 null) light-position))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-filled-3d-convex-polygon-with-normals-primitive
     draw-data handle object-id group (when model-matrix (mcopy model-matrix)) color vertices (when light-position (vcopy light-position)))))

(defun scene-add-filled-3d-convex-polygon-diffuse (scene group color vertices &optional (object-id 0))
  "Retained-mode function, adds a filled 3d convex polygon to the draw-lists, returns no values.  Displays with diffuse shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn)  where each successive x, y and z are the vertices of the polygon, and must be real numbers.  There must be at least three x, y, z triplets in vertices.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the polygon, you must delete the entire group."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data)
    (%draw-data-add-filled-3d-convex-polygon-with-normals draw-data object-id group color vertices)))

(defun scene-draw-filled-3d-convex-polygon-diffuse (scene group color vertices &optional (object-id 0))
  "Immediate-mode function, draws a filled 3d convex polygon, returns no values.  Displays with diffuse shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices must be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn)  where each successive x, y and z are the vertices of the polygon, and must be real numbers.  There must be at least three x, y, z triplets in vertices.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-filled-3d-convex-polygon-with-normals draw-data object-id group (canonicalize-color color) vertices)))

;; filled-3d-convex-polygon-flat
(defun scene-add-filled-3d-convex-polygon-primitive-flat (scene group model-matrix color vertices &optional (object-id 0))
  "Retained-mode function, returns a handle for a filled 3d convex polygon primitive.  Displays with flat shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer,  vertices should be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn) where the x, y and z's represent a vertex of the polygon and must be real numbers.  There must be at least three x, y, z triplets in vertices.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the polygon, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-filled-3d-convex-polygon-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) color vertices)))

(defun scene-add-filled-3d-convex-polygon-flat (scene group color vertices &optional (object-id 0))
  "Retained-mode function, adds a filled 3d convex polygon to the draw-lists, returns no values.  Displays with flat shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices should be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn) where the x, y and z's represent a vertex of the polygon and must be real numbers.  There must be at least three x, y, z triplets in vertices.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the polygon, you must delete the entire group."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data)
    (%draw-data-add-filled-3d-convex-polygon draw-data object-id group color vertices)))

(defun scene-draw-filled-3d-convex-polygon-flat (scene group color vertices &optional (object-id 0))
  "Immediate-mode function, draws a filled 3d convex polygon, returns no values.  Displays with flat shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices should be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn) where the x, y and z's represent a vertex of the polygon and must be real numbers.  There must be at least three x, y, z triplets in vertices.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-filled-3d-convex-polygon
     draw-data object-id group (canonicalize-color color) vertices)))

;; muticolor-3d-convex-polygon-diffuse
(defun scene-add-multicolor-3d-convex-polygon-primitive-diffuse (scene group model-matrix vertices light-position &optional (object-id 0))
  "Retained-mode function, returns a handle for a multicolored 3d convex polygon primitive.  Displays with diffuse shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), vertices must be of the form (list x0 y0 z0 nx0 ny0 nz0 color0 x1 y1 z1 nx1 ny1 nz1 color1 ... xn yn zn nxn nyn nzn colorn)  where each successive x, y z, nx, ny, nz and color are the vertices of the polygon and the normal at that vertex, and must be real numbers, color values can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  There must be at least three x, y, z, nx, ny, nz, color seven-tuples in vertices.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the polygon, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (or vec3 null) light-position))
  (declare (type (unsigned-byte 32) object-id))  
  (declare (type atom group))
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-multicolor-3d-convex-polygon-with-normals-primitive  
     draw-data handle object-id group (when model-matrix (mcopy model-matrix)) vertices (when light-position (vcopy light-position)))))

(defun scene-add-multicolor-3d-convex-polygon-diffuse (scene group vertices &optional (object-id 0))
  "Retained-mode function, adds a multicolored 3d convex polygon to the draw lists, returns no values.  Displays with diffuse shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, vertices must be of the form (list x0 y0 z0 nx0 ny0 nz0 color0 x1 y1 z1 nx1 ny1 nz1 color1 ... xn yn zn nxn nyn nzn colorn)  where each successive x, y z, nx, ny, nz and color are the vertices of the polygon and the normal at that vertex, and must be real numbers, color values can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  There must be at least three x, y, z, nx, ny, nz, color seven-tuples in vertices.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the polygon, you must delete the entire group."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (rm-dispatch-to-render-thread (scene draw-data)
    (%draw-data-add-multicolor-3d-convex-polygon-with-normals draw-data object-id group vertices)))

(defun scene-draw-multicolor-3d-convex-polygon-diffuse (scene group vertices &optional (object-id 0))
  "Immediate-mode function, draws a multicolored 3d convex polygon, returns no values.  Displays with diffuse shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, vertices must be of the form (list x0 y0 z0 nx0 ny0 nz0 color0 x1 y1 z1 nx1 ny1 nz1 color1 ... xn yn zn nxn nyn nzn colorn)  where each successive x, y z, nx, ny, nz and color are the vertices of the polygon and the normal at that vertex, and must be real numbers, color values can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  There must be at least three x, y, z, nx, ny, nz, color seven-tuples in vertices.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-multicolor-3d-convex-polygon-with-normals draw-data object-id group vertices)))

;; multicolor-3d-convex-polygon-flat
(defun scene-add-multicolor-3d-convex-polygon-primitive-flat (scene group model-matrix vertices &optional (object-id 0))
  "Retained-mode function, returns a handle for a multicolored 3d convex polygon primitive.  Displays with flat shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), vertices should be of the form (list x0 y0 z0 color0 x1 y1 z1 color1 ... xn yn zn colorn) where the x, y and z's represent a vertex of the polygon and must be real numbers, color values can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  There must be at least three x, y, z, color quads in vertices.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the polygon, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-multicolor-3d-convex-polygon-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) vertices)))

(defun scene-add-multicolor-3d-convex-polygon-flat (scene group vertices &optional (object-id 0))
  "Retained-mode function, adds a multicolored 3d convex polygon to the draw lists, returns no values.  Displays with flat shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, vertices must be of the form (list x0 y0 z0 color0 x1 y1 z1 color1 ... xn yn zn colorn)  where each successive x, y z and color are the vertices of the polygon, and must be real numbers, color values can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  There must be at least three x, y, z, color quads in vertices.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the polygon, you must delete the entire group."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (rm-dispatch-to-render-thread (scene draw-data)
    (%draw-data-add-multicolor-3d-convex-polygon draw-data object-id group vertices)))

(defun scene-draw-multicolor-3d-convex-polygon-flat (scene group vertices &optional (object-id 0))
  "Immediate-mode function, draws a multicolored 3d convex polygon, returns no values.  Displays with flat shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, vertices should be of the form (list x0 y0 z0 color0 x1 y1 z1 color1 ... xn yn zn colorn) where the x, y and z's represent a vertex of the polygon and must be real numbers, color values can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer.  There must be at least three x, y, z, color quads in vertices.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-multicolor-3d-convex-polygon draw-data object-id group vertices)))

;; textured-3d-triangle-list-flat
(defun scene-add-textured-3d-triangle-list-primitive-flat (scene group model-matrix texture color vertices &optional (object-id 0))
  "Retained-mode function, returns a handle for a textured 3d triangle list primitive.  Displays with flat shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), texture should be a texture such as return from make-vulkan-texture, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices is a list must be composed of sub-sequences of x, y, z, u and v, where u and v are the normalized texture coordinates at that vertex.  There must be at least three sub-sequences of x, y, z, u and v to make a triangle and additional triangles come in 3 sub-sequences each.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the triangle list, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-textured-3d-triangle-list-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) texture color vertices)))

(defun scene-add-textured-3d-triangle-list-flat (scene group texture color vertices &optional (object-id 0))
  "Retained-mode function, adds a textured 3d convex polygon to the draw-lists, returns no values.  Displays with flat shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, texture should be a texture such as return from make-vulkan-texture, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices is a list and must be composed of sub-sequences of x, y, z, u and v, where u and v are the normalized texture coordinates at that vertex.  There must be at least three sub-sequences of x, y, z, u and v to make a triangle and additional triangles come in 3 sub-sequences each.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Dispatches actual work to render thread.  To delete the polygon, you must delete the entire group."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data)
    (%draw-data-add-textured-3d-triangle-list draw-data object-id group texture color vertices)))

(defun scene-draw-textured-3d-triangle-list-flat (scene group texture color vertices &optional (object-id 0))
  "Immediate-mode function, draws a textured 3d convex polygon, returns no values.  Displays with flat shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, texture should be a texture such as return from make-vulkan-texture, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices is a list and must be composed of sub-sequences of x, y, z, u and v, where u and v are the normalized texture coordinates at that vertex.  There must be at least three sub-sequences of x, y, z, u and v to make a triangle and additional triangles come in 3 sub-sequences each.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is out/up.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-textured-3d-triangle-list draw-data object-id group texture (canonicalize-color color) vertices)))

;; textured-3d-triangle-strip-flat
(defun scene-add-textured-3d-triangle-strip-primitive-flat (scene group model-matrix texture color vertices &optional (object-id 0))
  "Retained-mode function, returns a handle for a textured 3d triangle strip primitive.  Displays with flat shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity),  texture should be a texture such as return from make-vulkan-texture, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices should be of the form (list x0 y0 z0 u0 v0 x1 y1 z1 u1 v1 ... xn yn zn un vn) where the x, y and z's represent a vertex of the polygon and must be real numbers.  u and v are the normalized texture coordinates of that vertex, and must be real numbers between zero and one.  There must be at least three x, y, z, u and v quints in vertices.  Dispatches actual work to render thread.  To delete the triangle strip, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-textured-3d-triangle-strip-primitive draw-data handle object-id group (when model-matrix (mcopy model-matrix)) texture color vertices)))

(defun scene-draw-textured-3d-triangle-strip-flat (scene group texture color vertices &optional (object-id 0))
  "Immediate-mode function, draws a textured 3d triangle strip, returns no values.  Displays with flat shading.  Required arguments: scene must be of type krma-essential-scene-mixin, group must be a non-null atom, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, vertices should be of the form (list x0 y0 z0 u0 v0 x1 y1 z1 u1 v1 ... xn yn zn un vn) where the x, y and z's represent a vertex of the polygon and must be real numbers.  u and v are the normalized texture coordinates of that vertex, and must be real numbers between zero and one.  There must be at least three x, y, z, u and v quints in vertices.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (let ((draw-data (im-draw-data scene)))
    (declare (type immediate-mode-draw-data draw-data))
    (let ((draw-list (draw-data-3d-triangle-strip-draw-list draw-data)))
      ;; we add the primitive/cmd without a handle:
      (%draw-list-add-textured-3d-triangle-strip/list
       draw-list object-id group nil texture (canonicalize-color color) vertices))))

(defun scene-add-filled-sphere-primitive-diffuse
    (scene group model-matrix color origin-x origin-y origin-z radius light-position
     resolution &optional (object-id 0))
  "Retained-mode function, returns a handle for a filled sphere primitive.  Displays with diffuse shading.  scene must be of the type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity),  color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, origin-z, origin-y and origin-z must be real numbers, radius must be a positive real number, light-position should either be nil or a 3d-vectors:vec3, resolution should be a positive integer and defaults to 64.  Dispatches actual work to render thread.  To delete the sphere, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real origin-x origin-y origin-z))
  (declare (type (integer 2 #.most-positive-fixnum) resolution))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (or vec3 null) light-position))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (setq origin-x (coerce origin-x 'double-float))
  (setq origin-y (coerce origin-y 'double-float))
  (setq origin-z (coerce origin-z 'double-float))
  (setq radius (coerce radius 'double-float))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
    (%draw-data-add-filled-sphere-primitive
     draw-data handle object-id group (when model-matrix (mcopy model-matrix)) color origin-x origin-y origin-z radius resolution (when light-position (vcopy light-position)))))

(defun scene-add-filled-sphere-diffuse (scene group color origin-x origin-y origin-z radius resolution &optional (object-id 0))
  "Retained-mode function, adds a filled sphere to the draw-lists, returns no values.  Displays with diffuse shading.  scene must be of the type krma-essential-scene-mixin, group must be a non-null atom,  color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, origin-z, origin-y and origin-z must be real numbers, radius must be a positive real number,  resolution should be a positive integer and defaults to 64.  Dispatches actual work to render thread.  To delete the sphere, you must delete the entire group."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real origin-x origin-y origin-z))
  (declare (type (integer 2 #.most-positive-fixnum) resolution))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq origin-x (coerce origin-x 'double-float))
  (setq origin-y (coerce origin-y 'double-float))
  (setq origin-z (coerce origin-z 'double-float))
  (setq radius (coerce radius 'double-float))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data)
    (%draw-data-add-filled-sphere
     draw-data object-id group color origin-x origin-y origin-z radius resolution)))

(defun scene-draw-filled-sphere-diffuse (scene color group origin-x origin-y origin-z radius resolution &optional (object-id 0))
  "Immediate-mode function, draws a filled sphere, returns no values.  Displays with diffuse shading.  scene must be of the type krma-essential-scene-mixin, group must be a non-null atom,  color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, origin-z, origin-y and origin-z must be real numbers, radius must be a positive real number,  resolution should be a positive integer and defaults to 64.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real origin-x origin-y origin-z))
  (declare (type (integer 2 #.most-positive-fixnum) resolution))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type (and atom t) group))
  (setq origin-x (coerce origin-x 'double-float))
  (setq origin-y (coerce origin-y 'double-float))
  (setq origin-z (coerce origin-z 'double-float))
  (setq radius (coerce radius 'double-float))
  (setq color (canonicalize-color color))
  (%draw-data-draw-filled-sphere (im-draw-data scene) object-id group color origin-x origin-y origin-z radius resolution))

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

(defun scene-add-text-primitive (scene group model-matrix font color pos-x pos-y string &optional (object-id 0))
  "Retained-mode function, renders and returns a handle for a text primitive.  scene must be of the type krma-essential-scene-mixin, group must be an atom, possibly nil (meaning not associated with a group), model-matrix must either be a 3d-matrices:mat4 or nil (nil effectively means identity), font is a font object as returned by vulkan-make-font, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, pos-x and pos-y represent the top left corner of the text and must be real numbers, string is the string you wish to render.  Dispatches actual work to render thread.  To delete the text, you must delete the primitive using the handle or delete the entire group, if any."
  (declare (type real pos-x pos-y))
  (declare (type string string))
  (declare (type (or mat4 null) model-matrix))
  (declare (type (unsigned-byte 32) object-id))
  (declare (type atom group))
  (let ((data (font-data font)))
    (declare (type 3b-bmfont-common:bmfont data))
    (let* ((glyph-table (slot-value data '3b-bmfont-common::chars))
           (scale-w (float (3b-bmfont-common:scale-w data) 1.0f0))
           (scale-h (float (3b-bmfont-common:scale-h data) 1.0f0))
	   (pos-x (clampf pos-x))
	   (pos-y (clampf pos-y))
	   (color (canonicalize-color color))
           (vertices (compute-text-coordinates pos-x pos-y string glyph-table scale-w scale-h)))
      (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
	(%draw-data-add-text-quad-list-primitive draw-data handle object-id group
						 (when model-matrix (mcopy model-matrix))
						 font color
						 vertices)))))

(defun scene-add-text (scene group font color pos-x pos-y string &optional (object-id 0))
  "Retained-mode function, adds text to the draw lists, returns no values.  scene must be of the type krma-essential-scene-mixin, group must be a non-null atom, font is a font object as returned by vulkan-make-font, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, pos-x and pos-y represent the top left corner of the text and must be real numbers, string is the string you wish to render.  Dispatches actual work to render thread.  To delete the text, you must delete the entire group."
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
	   (color (canonicalize-color color))
           (vertices (compute-text-coordinates pos-x pos-y string glyph-table scale-w scale-h)))
      (rm-dispatch-to-render-thread (scene draw-data)
	(%draw-data-add-text-quad-list draw-data object-id group
				       font color vertices)))))

(defun scene-draw-text (scene group font color pos-x pos-y string &optional (object-id 0))
  "Retained-mode function, draws text, returns no values.  scene must be of the type krma-essential-scene-mixin, group must be a non-null atom, font is a font object as returned by vulkan-make-font, color can either be a 4 component vector who's elements are real numbers between zero and one, or a 32 bit unsigned integer, pos-x and pos-y represent the top left corner of the text and must be real numbers, string is the string you wish to render.  Performs work in current thread, which should be the render thread.  Effects of this function only last for the current frame."
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
      (%draw-data-draw-text-quad-list (im-draw-data scene) object-id group font (canonicalize-color color) vertices))))

(declaim (inline %resinstance-cmd))
(defun %reinstance-cmd (cmd constructor group model-mtx sf-line-thickness sf-point-size ub32-color-override light-position)
  (declare (type standard-draw-indexed-cmd cmd))
  (declare (type function constructor))
  (declare (type (or single-float null) sf-line-thickness sf-point-size))
  (declare (type (or (unsigned-byte 32) null) ub32-color-override))
  (declare (type (or mat4 null) model-mtx))
  (declare (type (or vec3 null) light-position))
  (let ((first-idx (cmd-first-idx cmd))
        (elem-count (cmd-elem-count cmd))
        (vtx-offset (cmd-vtx-offset cmd))
        (draw-list (cmd-draw-list cmd))
        (texture (cmd-texture cmd)))
    (setq group (or group (cmd-group cmd)))
    (setq model-mtx (or model-mtx (cmd-model-mtx cmd)))
    (setq sf-line-thickness (or sf-line-thickness (cmd-line-thickness cmd)))
    (setq ub32-color-override (or ub32-color-override (cmd-color-override cmd)))
    (setq sf-point-size (or sf-point-size (cmd-point-size cmd)))
    (setq light-position (or light-position (cmd-light-position cmd)))
    (let ((cmd (funcall constructor
                        draw-list
                        first-idx elem-count vtx-offset
                        group (when model-mtx (mcopy model-mtx))
                        ub32-color-override texture sf-line-thickness sf-point-size
                        (when light-position (vcopy light-position)))))
      (vector-push-extend cmd (draw-list-cmd-vector draw-list))
      cmd)))

(declaim (inline %reinstance-primitive-1))
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
                (%reinstance-cmd cmd constructor
                                 group model-mtx sf-line-thickness ub32-color-override sf-point-size light-position))))))

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


(defun reinstance-primitive (scene handle
                             &key 
			       (group nil)
			       (model-matrix nil)
                               (point-size nil)
                               (line-thickness nil)
                               (color-override nil)
                               (light-position nil)
                               (font nil))
  "Retained-mode function.  Re-instances a primitive given a handle with the option to set a new group, model-matrix, point-size, line-thickness, color-override, light-position and/or font.  References the same vertices in the draw-lists.  To be run in a thread outside of the render thread.  Dispatches actual work to render thread."
  (declare (type krma-essential-scene-mixin scene))
  (when point-size
    (setq point-size (clampf point-size)))
  (when line-thickness
    (setq line-thickness (clampf line-thickness)))
  (when color-override
    (setq color-override (canonicalize-color color-override)))
  (let ((draw-data (rm-draw-data scene)))
    (let ((dd0 (svref draw-data 0))
          (dd1 (svref draw-data 1)))
      (declare (type retained-mode-draw-data dd0 dd1))
      (let ((ht0 (draw-data-handle-hash-table dd0))
            (ht1 (draw-data-handle-hash-table dd1))
            (wq0 (draw-data-work-queue dd0))
            (wq1 (draw-data-work-queue dd1))
            (new-handle (gen-rm-handle)))
        (sb-concurrency:enqueue #'(lambda ()
                                    (%reinstance-primitive-1 ht0 new-handle handle group
                                                             model-matrix line-thickness point-size color-override
                                                             light-position font))
                                wq0)
        (sb-concurrency:enqueue #'(lambda ()
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
(defun primitive-set-color (scene handle color)
  "Retained-mode function. Returns no values.  To be run in a thread outside of the render thread.  Sets the color override of the primitive, normally renderer uses the vertex color.  To be run in a thread outside of the render thread.  Dispatches actual work to render thread."
  (declare (type krma-essential-scene-mixin scene))
  (let ((draw-data (rm-draw-data scene)))
    (setq color (canonicalize-color color))
    (let ((dd0 (svref draw-data 0))
          (dd1 (svref draw-data 1)))
      (declare (type retained-mode-draw-data dd0 dd1))
      (let ((ht0 (draw-data-handle-hash-table dd0))
            (ht1 (draw-data-handle-hash-table dd1))
            (wq0 (draw-data-work-queue dd0))
            (wq1 (draw-data-work-queue dd1)))
        (sb-concurrency:enqueue #'(lambda () (%primitive-set-color-1 ht0 handle color)) wq0)
        (sb-concurrency:enqueue #'(lambda () (%primitive-set-color-1 ht1 handle color)) wq1)
        (values)))))

(declaim (inline %primitive-set-light-position-1))
(defun %primitive-set-light-position-1 (ht handle pos)
  (let ((cmd (gethash handle ht)))
    (if (listp cmd)
        (warn "while in %primitive-set-light-position-1 ...could not find primitive ~S to set light position." handle)
        (setf (cmd-light-position cmd) pos))
    (values)))

(defun primitive-set-light-position-1 (draw-data handle pos)
  "Retained-mode function.  Returns no values.  Sets the light position of the primitive, normally renderer uses the scene light position, unless group light position is set.  Performs work in current thread, which should be the render thread."
  (declare (type retained-mode-draw-data draw-data))
  (let ((ht (draw-data-handle-hash-table draw-data)))
    (%primitive-set-light-position-1 ht handle pos)))

(defun primitive-set-light-position (scene handle pos)
  "Retained-mode function.  Returns no values.  Sets the light position of the primitive, normally renderer uses the scene light position, unless group light position is set.  To be run in a thread outside of the render thread.  Dispatches actual work to render thread."
  (declare (type krma-essential-scene-mixin scene))
  (let ((draw-data (rm-draw-data scene)))
    (let ((dd0 (svref draw-data 0))
          (dd1 (svref draw-data 1)))
      (declare (type retained-mode-draw-data dd0 dd1))
      (let ((ht0 (draw-data-handle-hash-table dd0))
            (ht1 (draw-data-handle-hash-table dd1))
            (wq0 (draw-data-work-queue dd0))
            (wq1 (draw-data-work-queue dd1)))
        (sb-concurrency:enqueue #'(lambda () (%primitive-set-light-position-1 ht0 handle pos)) wq0)
        (sb-concurrency:enqueue #'(lambda () (%primitive-set-light-position-1 ht1 handle pos)) wq1)
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
(defun primitive-set-transform (scene handle matrix)
  "Retained-mode function.  Sets the model-matrix of the primitive, renderer composes the primitive model matrix with the group model matrix.  To be run in a thread outside of the render thread.  Dispatches actual work to render thread."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type (or mat4 null) matrix))
  (let ((draw-data (rm-draw-data scene)))
    (let ((dd0 (svref draw-data 0))
          (dd1 (svref draw-data 1)))
      (declare (type retained-mode-draw-data dd0 dd1))
      (let ((ht0 (draw-data-handle-hash-table dd0))
            (ht1 (draw-data-handle-hash-table dd1))
            (wq0 (draw-data-work-queue dd0))
            (wq1 (draw-data-work-queue dd1)))
        (sb-concurrency:enqueue #'(lambda () (%primitive-set-transform-1 ht0 handle matrix)) wq0)
        (sb-concurrency:enqueue #'(lambda () (%primitive-set-transform-1 ht1 handle matrix)) wq1)
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
(defun primitive-apply-transform (scene handle matrix)
  "Retained-mode function.  Returns no values.  Applies the matrix to the existing model-matrix of the primitive, renderer composes the primitive model matrix with the group model matrix.  To be run in a thread outside of the render thread.  Dispatches actual work to render thread."
  (declare (type krma-essential-scene-mixin scene))
  (declare (type mat4 matrix))
  (let ((draw-data (rm-draw-data scene)))
    (let ((dd0 (svref draw-data 0))
          (dd1 (svref draw-data 1)))
      (declare (type retained-mode-draw-data dd0 dd1))
      (let ((ht0 (draw-data-handle-hash-table dd0))
            (ht1 (draw-data-handle-hash-table dd1))
            (wq0 (draw-data-work-queue dd0))
            (wq1 (draw-data-work-queue dd1)))
        (sb-concurrency:enqueue #'(lambda () (%primitive-apply-transform-1 ht0 handle matrix)) wq0)
        (sb-concurrency:enqueue #'(lambda () (%primitive-apply-transform-1 ht1 handle matrix)) wq1)
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

(defun primitive-set-line-thickness (scene handle thickness)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real thickness))
  (setq thickness (clampf thickness))
  (let ((draw-data (rm-draw-data scene)))
    (let ((dd0 (svref draw-data 0))
          (dd1 (svref draw-data 1)))
      (declare (type retained-mode-draw-data dd0 dd1))
      (let ((ht0 (draw-data-handle-hash-table dd0))
            (ht1 (draw-data-handle-hash-table dd1))
            (wq0 (draw-data-work-queue dd0))
            (wq1 (draw-data-work-queue dd1)))
        (sb-concurrency:enqueue #'(lambda () (%primitive-set-line-thickness-1 ht0 handle thickness)) wq0)
        (sb-concurrency:enqueue #'(lambda () (%primitive-set-line-thickness-1 ht1 handle thickness)) wq1)
        (values)))))

(defun %delete-primitive-1 (ht handle)
  (handler-case
      (let ((cmd (gethash handle ht)))
        (if (listp cmd)
            (warn "while in %delete-primitive-1 ...could not find primitive to delete ~S" handle)
            (let* ((draw-list (cmd-draw-list cmd))
                   (cmd-vector (draw-list-cmd-vector draw-list)))
              (loop for entry across cmd-vector
                    for i from 0
                    when (eq cmd entry)
		      do (setf (aref cmd-vector i) nil)
			 (setf (draw-list-needs-compaction? draw-list) t)
			 (remhash handle ht)
			 (return (values))))))
    (error (c)
      (warn (concatenate 'string "while in %delete-primitive-1 ..." (princ-to-string c)))
      (values))))

(defun delete-primitive-1 (draw-data handle)
  (let ((ht (draw-data-handle-hash-table draw-data)))
    (%delete-primitive-1 ht handle)))

(defun delete-primitives (scene list-of-handles)
  (declare (type krma-essential-scene-mixin scene))
  (let ((draw-data (rm-draw-data scene)))
    (let ((dd0 (svref draw-data 0))
          (dd1 (svref draw-data 1)))
      (declare (type retained-mode-draw-data dd0 dd1))
      (let ((wq0 (draw-data-work-queue dd0))
            (wq1 (draw-data-work-queue dd1))
            (ht0 (draw-data-handle-hash-table dd0))
            (ht1 (draw-data-handle-hash-table dd1)))
        (sb-concurrency:enqueue #'(lambda ()
                                    (loop for handle in list-of-handles
                                          do (%delete-primitive-1 ht0 handle)))
                                wq0)
        (sb-concurrency:enqueue #'(lambda ()
                                    (loop for handle in list-of-handles
                                          do (%delete-primitive-1 ht1 handle)))
                                wq1)))))

(defun delete-primitive (scene handle)
  (declare (type krma-essential-scene-mixin scene))
  (delete-primitives scene (list handle)))

(defun delete-groups-1 (draw-data list-of-groups)
  (declare (type retained-mode-draw-data draw-data))
  (handler-case
      (progn
        (flet ((free-group-draw-lists (ht)
                 (unless ht
                   (warn "ht is null"))
                 (let ((key-list ()))
                   ;; this ought to be fast since we maphash these tables at render time also
                   ;; and haven't a performance problem yet (knock on wood)
                   (maphash #'(lambda (key draw-list)
                                (when (find (car key) list-of-groups)
                                  (push key key-list)
                                  (let ((ia (draw-list-index-array draw-list))
                                        (va (draw-list-vertex-array draw-list))
                                        (im (draw-list-index-memory draw-list))
                                        (vm (draw-list-vertex-memory draw-list)))
                                    (declare (type foreign-adjustable-array ia va))
                                    (foreign-free (foreign-array-ptr ia))
                                    (foreign-free (foreign-array-ptr va))
                                    (vk::release-index-memory *app* im)
                                    (vk::release-vertex-memory *app* vm)
                                    nil)))
                            ht)
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
                       )
              draw-data

            ;; I don't like these brute force searches, but it can't be any slower than a render!

            (free-group-draw-lists 2d-point-list-draw-list-table)
            (free-group-draw-lists 2d-line-list-draw-list-table)
            (free-group-draw-lists 2d-triangle-list-draw-list-table)
            (free-group-draw-lists 2d-triangle-list-draw-list-for-text-table)
            (free-group-draw-lists 3d-point-list-draw-list-table)
            (free-group-draw-lists 3d-line-list-draw-list-table)
            (free-group-draw-lists 3d-triangle-list-draw-list-table)
            (free-group-draw-lists 3d-triangle-list-with-normals-draw-list-table)

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


          (let ((groups (draw-data-group-hash-table draw-data)))
            (unless groups
              (warn "groups is null"))
            (mapcar #'(lambda (group)
                        (remhash group groups))
                    list-of-groups))))
    (error (c)
      (warn (concatenate 'string "while in delete-groups-1 ..." (princ-to-string c))))))

(defun %purge-im-groups-1 (draw-data)
  ;; call this function when finalizing a scene
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
				(when *app*
				  (vk::release-index-memory *app* im)
				  (vk::release-vertex-memory *app* vm))
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

(defun delete-groups (scene list-of-groups)
  (declare (type krma-essential-scene-mixin scene))
  (rm-dispatch-to-render-thread (scene draw-data)
    (delete-groups-1 draw-data list-of-groups)))

(defun delete-group (scene group)
  (declare (type (and atom t) group))
  (delete-groups scene (list group)))

(declaim (inline %group-set-color-override-1))
(defun %group-set-color-override-1 (draw-data atom-group ub32-color)
  (let ((group (gethash atom-group (draw-data-group-hash-table draw-data))))
    (if group
        (setf (group-color-override group) ub32-color)
        (warn "while in %group-set-color-override-1 ...no group named ~S" atom-group))))

(defun group-set-color-override-1 (draw-data group color)
  (declare (type (and atom t) group))
  (%group-set-color-override-1 draw-data group (canonicalize-color color)))

(defun group-set-color-override (scene group color)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type (and atom t) group))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data)
    (%group-set-color-override-1 draw-data group color)))

(declaim (inline %group-set-model-matrix-1))
(defun %group-set-model-matrix-1 (draw-data atom-group matrix)
  (let ((group (gethash atom-group (draw-data-group-hash-table draw-data))))
    (if group
        (setf (group-model-matrix group) matrix)
        (warn "while in %group-set-model-matrix-1 ...no group named ~S" atom-group))))

(defun group-set-model-matrix-1 (draw-data group matrix)
  (declare (type (or mat4 null) matrix))
  (declare (type (and atom t) group))
  (%group-set-model-matrix-1 draw-data group matrix))

(defun group-set-model-matrix (scene group matrix)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type (or mat4 null) matrix))
  (declare (type (and atom t) group))
  (rm-dispatch-to-render-thread (scene draw-data)
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

(defun group-apply-model-matrix (scene group matrix)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type (or mat4 null) matrix))
  (declare (type (and atom t) group))
  (rm-dispatch-to-render-thread (scene draw-data)
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
(defun group-set-light-position (scene group position)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type (or vec3 null) position))
  (declare (type (and atom t) group))
  (rm-dispatch-to-render-thread (scene draw-data)
    (%group-set-light-position-1 draw-data group position)))

(defun ensure-group-1 (draw-data group)
  (declare (type (and atom t) group))
  (let ((group-hash-table (draw-data-group-hash-table draw-data)))
    (or (gethash group group-hash-table)
        (setf (gethash group group-hash-table)
              (make-group group)))))
