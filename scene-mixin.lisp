(in-package :krma)

#+NOTYET(declaim (optimize (speed 1) (safety 3) (debug 3)))

(defparameter +default-znear+ 0.001)
(defparameter +default-zfar+ 3000.0)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-concurrency))

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
   (light-position
    :initform nil
    :accessor scene-light-position)

   (3d-camera-projection-matrix)
   (3d-camera-view-matrix)
   (2d-camera-projection-matrix)
   (2d-camera-view-matrix)))

(defclass standard-scene (krma-essential-scene-mixin) ())


(defun update-2d-camera (scene &optional
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

(defun update-3d-camera (scene &optional
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
  
  (let ((device (default-logical-device app))
	(pipeline-store (application-pipeline-store app)))

    (with-slots (width height) app

      (with-slots (3d-camera-projection-matrix
		   3d-camera-view-matrix
		   2d-camera-projection-matrix
		   2d-camera-view-matrix)
	  scene

	(let ((2d-vproj (m* 2d-camera-projection-matrix 2d-camera-view-matrix))
	      (3d-vproj (m* 3d-camera-projection-matrix 3d-camera-view-matrix)))

	  (loop for (p dd) on (3d-cmd-oriented-combinations pipeline-store rm-draw-data) by #'cddr
		do (render-draw-list-cmds
		    p dd scene device command-buffer *identity-matrix* 3d-vproj width height))

	  (loop for (p dd) on (3d-draw-list-oriented-combinations pipeline-store rm-draw-data) by #'cddr
		do (render-draw-list
		    p dd scene device command-buffer *identity-matrix* 3d-vproj width height))

	  (loop for (p dd) on (3d-cmd-oriented-combinations pipeline-store im-draw-data) by #'cddr
		do (render-draw-list-cmds
		    p dd scene device command-buffer *identity-matrix* 3d-vproj width height))
	
	  (loop for (p dd) on (3d-draw-list-oriented-combinations pipeline-store im-draw-data) by #'cddr
		do (render-draw-list
		    p dd scene device command-buffer *identity-matrix* 3d-vproj width height))
	
	  (loop for (p dd) on (2d-cmd-oriented-combinations pipeline-store rm-draw-data) by #'cddr
		do (render-draw-list-cmds
		    p dd scene device command-buffer *identity-matrix* 2d-vproj width height))

	  (loop for (p dd) on (2d-draw-list-oriented-combinations pipeline-store rm-draw-data) by #'cddr
		do (render-draw-list
		    p dd scene device command-buffer *identity-matrix* 2d-vproj width height))

	  (loop for (p dd) on (2d-cmd-oriented-combinations pipeline-store im-draw-data) by #'cddr
		do (render-draw-list-cmds
		    p dd scene device command-buffer *identity-matrix* 2d-vproj width height))
	
	  (loop for (p dd) on (2d-draw-list-oriented-combinations pipeline-store im-draw-data) by #'cddr
		do (render-draw-list
		    p dd scene device command-buffer *identity-matrix* 2d-vproj width height)))
	(values)))))

;; 2d-point
(defun scene-add-2d-point (scene model-matrix point-size color x y)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x y))
  ;; we try to run code which potentially errors outside of render-thread
  ;; the body of rm-dispatch-to-render-thread becomes a closure
  (setq color (canonicalize-color color))
  (setq x (clampf x))
  (setq y (clampf y))
  (setq point-size (clampf point-size))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-2d-point-primitive draw-data handle model-matrix point-size color x y)))

(defun scene-add-2d-point-to-group (scene group point-size color x y)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x y))
  (assert (typep group 'atom))
  (setq color (canonicalize-color color))
  (setq x (clampf x))
  (setq y (clampf y))
  (setq point-size (clampf point-size))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-2d-point-pseudo draw-data handle group point-size color x y)))

(defun scene-draw-2d-point (scene point-size color x y)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x y point-size))
  (setq point-size (clampf point-size))
  (let ((draw-data (im-draw-data scene)))
    (declare (type standard-draw-data draw-data))
    (%draw-data-draw-2d-point draw-data point-size (canonicalize-color color) (clampf x) (clampf y))))

;; 3d-point
(defun scene-add-3d-point (scene model-matrix point-size color x y z)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x y z point-size))
  (setq color (canonicalize-color color))
  (setq x (clampf x))
  (setq y (clampf y))
  (setq z (clampf z))
  (setq point-size (clampf point-size))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-3d-point-primitive draw-data handle model-matrix point-size color x y z)))

(defun scene-add-3d-point-to-group (scene group point-size color x y z)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x y z point-size))
  (assert (typep group 'atom))
  (setq color (canonicalize-color color))
  (setq x (clampf x))
  (setq y (clampf y))
  (setq z (clampf z))
  (setq point-size (clampf point-size))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-3d-point-pseudo draw-data handle group point-size color x y z)))

(defun scene-draw-3d-point (scene point-size color x y z)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x y z))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-3d-point draw-data (clampf point-size)
			      (canonicalize-color color) (clampf x) (clampf y) (clampf z))))

;; 2d-line
(defun scene-add-2d-line (scene model-matrix line-thickness color x0 y0 x1 y1)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x0 y0 x1 y1 line-thickness))
  (setq color (canonicalize-color color))
  (setq x0 (clampf x0))
  (setq y0 (clampf y0))
  (setq x1 (clampf x1))
  (setq y1 (clampf y1))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-2d-line-primitive draw-data handle model-matrix line-thickness color x0 y0 x1 y1)))

(defun scene-add-2d-line-to-group (scene group line-thickness color x0 y0 x1 y1)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x0 y0 x1 y1 line-thickness))
  (assert (typep group 'atom))
  (setq color (canonicalize-color color))
  (setq x0 (clampf x0))
  (setq y0 (clampf y0))
  (setq x1 (clampf x1))
  (setq y1 (clampf y1))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-2d-line-pseudo draw-data handle group line-thickness color x0 y0 x1 y1)))

(defun scene-draw-2d-line (scene line-thickness color x0 y0 x1 y1)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x0 y0 x1 y1 line-thickness))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-2d-line draw-data (clampf line-thickness)
			     (canonicalize-color color) (clampf x0) (clampf y0) (clampf x1) (clampf y1))))

;; 3d-line
(defun scene-add-3d-line (scene model-matrix line-thickness color x0 y0 z0 x1 y1 z1)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x0 y0 z0 x1 y1 z1 line-thickness))
  (setq color (canonicalize-color color))
  (setq x0 (clampf x0))
  (setq y0 (clampf y0))
  (setq z0 (clampf z0))
  (setq x1 (clampf x1))
  (setq y1 (clampf y1))
  (setq z1 (clampf z1))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-3d-line-primitive draw-data handle model-matrix line-thickness color x0 y0 z0 x1 y1 z1)))

(defun scene-add-3d-line-to-group (scene group line-thickness color x0 y0 z0 x1 y1 z1)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x0 y0 z0 x1 y1 z1 line-thickness))
  (assert (typep group 'atom))
  (setq color (canonicalize-color color))
  (setq x0 (clampf x0))
  (setq y0 (clampf y0))
  (setq z0 (clampf z0))
  (setq x1 (clampf x1))
  (setq y1 (clampf y1))
  (setq z1 (clampf z1))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-3d-line-pseudo draw-data handle group line-thickness color x0 y0 z0 x1 y1 z1)))

(defun scene-draw-3d-line (scene line-thickness color x0 y0 z0 x1 y1 z1)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x0 y0 x0 x1 y1 z1 line-thickness))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-3d-line draw-data (clampf line-thickness) (canonicalize-color color)
			     (clampf x0) (clampf y0) (clampf z0)
			     (clampf x1) (clampf y1) (clampf z1))))

;; 2d-polyline
(defun scene-add-2d-polyline (scene model-mtx closed? line-thickness color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (setq color (canonicalize-color color))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-2d-polyline-primitive draw-data handle model-mtx closed? line-thickness color vertices)))

(defun scene-add-2d-polyline-to-group (scene group closed? line-thickness color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (assert (typep group 'atom))
  (setq color (canonicalize-color color))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-2d-polyline-pseudo draw-data handle group closed? line-thickness color vertices)))

(defun scene-draw-2d-polyline (scene closed? line-thickness color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-2d-polyline draw-data closed? (clampf line-thickness) (canonicalize-color color) vertices)))

;; 2d-triangle
(defun scene-add-2d-triangle (scene model-mtx line-thickness color x0 y0 x1 y1 x2 y2)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x0 y0 x1 y1 x2 y2 line-thickness))
  (setq color (canonicalize-color color))
  (setq x0 (clampf x0))
  (setq y0 (clampf y0))
  (setq x1 (clampf x1))
  (setq y1 (clampf y1))
  (setq x2 (clampf x2))
  (setq y2 (clampf y2))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-2d-polyline-primitive draw-data handle model-mtx t line-thickness color
					  (list x0 y0 x1 y1 x2 y2))))

(defun scene-add-2d-triangle-to-group (scene group line-thickness color x0 y0 x1 y1 x2 y2)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x0 y0 x1 y1 x2 y2 line-thickness))
  (assert (typep group 'atom))
  (setq color (canonicalize-color color))
  (setq x0 (clampf x0))
  (setq y0 (clampf y0))
  (setq x1 (clampf x1))
  (setq y1 (clampf y1))
  (setq x2 (clampf x2))
  (setq y2 (clampf y2))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-2d-polyline-pseudo draw-data handle group t line-thickness color
				       (list x0 y0 x1 y1 x2 y2))))

(defun scene-draw-2d-triangle (scene line-thickness color x0 y0 x1 y1 x2 y2)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x0 y0 x1 y1 x2 y2 line-thickness))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-2d-polyline draw-data t (clampf line-thickness) (canonicalize-color color)
				 (list x0 y0 x1 y1 x2 y2))))

;; 2d-rectangle
(defun scene-add-2d-rectangle (scene model-mtx line-thickness color x0 y0 x1 y1)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x0 y0 x1 y1 line-thickness))
  (setq color (canonicalize-color color))
  (setq x0 (clampf x0))
  (setq y0 (clampf y0))
  (setq x1 (clampf x1))
  (setq y1 (clampf y1))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-2d-polyline-primitive draw-data handle model-mtx t line-thickness color
					  (list x0 y0 x0 y1 x1 y1 x1 y0))))
					  

(defun scene-add-2d-rectangle-to-group (scene group line-thickness color x0 y0 x1 y1)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x0 y0 x1 y1 line-thickness))
  (assert (typep group 'atom))
  (setq color (canonicalize-color color))
  (setq x0 (clampf x0))
  (setq y0 (clampf y0))
  (setq x1 (clampf x1))
  (setq y1 (clampf y1))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-2d-polyline-pseudo draw-data handle group t line-thickness color
				       (list x0 y0 x0 y1 x1 y1 x1 y0))))

(defun scene-draw-2d-rectangle (scene line-thickness color x0 y0 x1 y1)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real x0 y0 x1 y1 line-thickness))
  (let ((draw-data (im-draw-data scene)))
    (setq x0 (clampf x0))
    (setq y0 (clampf y0))
    (setq x1 (clampf x1))
    (setq y1 (clampf y1))
    (%draw-data-draw-2d-polyline draw-data t (clampf line-thickness) (canonicalize-color color)
				 (list x0 y0 x0 y1 x1 y1 x1 y0))))

;; multicolor-2d-polyline
(defun scene-add-multicolor-2d-polyline (scene model-mtx closed? line-thickness vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-multicolor-2d-polyline-primitive draw-data handle model-mtx closed? line-thickness vertices)))

(defun scene-add-multicolor-2d-polyline-to-group (scene group closed? line-thickness vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (assert (typep group 'atom))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-multicolor-2d-polyline-pseudo draw-data handle group closed? line-thickness vertices)))

(defun scene-draw-multicolor-2d-polyline (scene closed? line-thickness vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-multicolor-2d-polyline draw-data closed? (clampf line-thickness) vertices)))

;; 2d-circular-arc
(defun scene-add-2d-circular-arc (scene model-mtx closed? line-thickness color
                                  center-x center-y radius start-angle end-angle
                                  &optional (number-of-segments 64))
  (declare (type krma-essential-scene-mixin scene))
  (declare (type boolean closed?))
  (declare (type real center-x center-y radius start-angle end-angle line-thickness))
  (declare (type (integer 1 #.most-positive-fixnum) number-of-segments))
  (setq center-x (coerce center-x 'double-float))
  (setq center-y (coerce center-y 'double-float))
  (setq radius (coerce radius 'double-float))
  (setq start-angle (coerce start-angle 'double-float))
  (setq end-angle (coerce end-angle 'double-float))
  (setq color (canonicalize-color color))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-2d-circular-arc-primitive draw-data handle model-mtx closed? line-thickness color
					      center-x center-y radius start-angle end-angle
					      number-of-segments)))

(defun scene-add-2d-circular-arc-to-group (scene group closed? line-thickness color
					   center-x center-y radius start-angle end-angle
					   &optional (number-of-segments 64))
  (declare (type krma-essential-scene-mixin scene))
  (declare (type boolean closed?))
  (declare (type real center-x center-y radius start-angle end-angle line-thickness))
  (declare (type (integer 1 #.most-positive-fixnum) number-of-segments))
  (assert (typep group 'atom))
  (setq center-x (coerce center-x 'double-float))
  (setq center-y (coerce center-y 'double-float))
  (setq radius (coerce radius 'double-float))
  (setq start-angle (coerce start-angle 'double-float))
  (setq end-angle (coerce end-angle 'double-float))
  (setq color (canonicalize-color color))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-2d-circular-arc-pseudo draw-data handle group closed? line-thickness color
					   center-x center-y radius start-angle end-angle
					   number-of-segments)))

(defun scene-draw-2d-circular-arc (scene closed? line-thickness color
				   center-x center-y radius start-angle end-angle
                                   &optional (number-of-segments 64))
  (declare (type krma-essential-scene-mixin scene))
  (declare (type boolean closed?))
  (declare (type real center-x center-y radius start-angle end-angle))
  (declare (type (integer 1 #.most-positive-fixnum) number-of-segments))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-2d-circular-arc draw-data closed? (clampf line-thickness) (canonicalize-color color)
				     (coerce center-x 'double-float) (coerce center-y 'double-float)
				     (coerce radius 'double-float)
				     (coerce start-angle 'double-float) (coerce end-angle 'double-float)
				     number-of-segments)))

;; 2d-circle
(defun scene-add-2d-circle (scene model-mtx line-thickness color
                            center-x center-y radius
                            &optional (number-of-segments 64))
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real center-x center-y radius line-thickness))
  (declare (type (integer 1 #.most-positive-fixnum) number-of-segments))
  (setq color (canonicalize-color color))
  (setq center-x (coerce center-x 'double-float))
  (setq center-y (coerce center-y 'double-float))
  (setq radius (coerce radius 'double-float))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-2d-circle-primitive draw-data handle
					model-mtx line-thickness color
					center-x center-y radius
					number-of-segments)))

(defun scene-add-2d-circle-to-group (scene group line-thickness color
				     center-x center-y radius
				     &optional (number-of-segments 64))
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real center-x center-y radius line-thickness))
  (declare (type (integer 1 #.most-positive-fixnum) number-of-segments))
  (assert (typep group 'atom))
  (setq color (canonicalize-color color))
  (setq center-x (coerce center-x 'double-float))
  (setq center-y (coerce center-y 'double-float))
  (setq radius (coerce radius 'double-float))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-2d-circle-pseudo draw-data handle
				     group line-thickness color
				     center-x center-y radius
				     number-of-segments)))

(defun scene-draw-2d-circle (scene line-thickness color
			     center-x center-y radius
                             &optional (number-of-segments 64))
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real center-x center-y radius))
  (declare (type (integer 1 #.most-positive-fixnum) number-of-segments))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-2d-circle draw-data (clampf line-thickness) (canonicalize-color color)
				(coerce center-x 'double-float) (coerce center-y 'double-float)
				(coerce radius 'double-float)
				number-of-segments)))

;; 3d-polyline
(defun scene-add-3d-polyline (scene model-mtx closed? line-thickness color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real line-thickness))
  (declare (type sequence vertices))
  (setq color (canonicalize-color color))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-3d-polyline-primitive draw-data handle model-mtx closed? line-thickness color vertices)))

(defun scene-add-3d-polyline-to-group (scene group closed? line-thickness color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real line-thickness))
  (declare (type sequence vertices))
  (assert (typep group 'atom))
  (setq color (canonicalize-color color))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-3d-polyline-pseudo draw-data handle group closed? line-thickness color vertices)))

(defun scene-draw-3d-polyline (scene closed? line-thickness color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-3d-polyline draw-data closed? (clampf line-thickness) (canonicalize-color color) vertices)))

;; multicolor-3d-polyline
(defun scene-add-multicolor-3d-polyline (scene model-mtx closed? line-thickness vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-multicolor-3d-polyline-primitive draw-data handle model-mtx closed? line-thickness vertices)))

(defun scene-add-multicolor-3d-polyline-to-group (scene group closed? line-thickness vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (assert (typep group 'atom))
  (setq line-thickness (clampf line-thickness))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-multicolor-3d-polyline-pseudo draw-data handle group closed? line-thickness vertices)))

(defun scene-draw-multicolor-3d-polyline (scene closed? line-thickness vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real line-thickness))
  (declare (type boolean closed?))
  (declare (type sequence vertices))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-multicolor-3d-polyline draw-data closed? (clampf line-thickness) vertices)))

;; filled-2d-triangle-list
(defun scene-add-filled-2d-triangle-list (scene model-mtx color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-2d-triangle-list-primitive draw-data handle model-mtx color vertices)))

(defun scene-add-filled-2d-triangle-list-to-group (scene group color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (assert (typep group 'atom))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-2d-triangle-list-pseudo draw-data handle group color vertices)))

(defun scene-draw-filled-2d-triangle-list (scene color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-filled-2d-triangle-list draw-data color vertices)))

;; filled-2d-triangle-strip
(defun scene-add-filled-2d-triangle-strip (scene model-mtx color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-2d-triangle-strip-primitive draw-data handle model-mtx color vertices)))

(defun scene-draw-filled-2d-triangle-strip (scene color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (let ((draw-data (im-draw-data scene)))
    (declare (type immediate-mode-draw-data draw-data))
    (let ((draw-list (draw-data-2d-triangle-strip-draw-list draw-data)))
      ;; we add the primitive/cmd without a handle:
      (%draw-list-add-filled-2d-triangle-strip/list-cmd draw-list nil (canonicalize-color color) vertices))))

;; filled-2d-rectangle-list
(defun scene-add-filled-2d-rectangle-list (scene model-mtx color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-2d-rectangle-list-primitive draw-data handle model-mtx color vertices)))

(defun scene-add-filled-2d-rectangle-list-to-group (scene group color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (assert (typep group 'atom))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-2d-rectangle-list-pseudo draw-data handle group color vertices)))

(defun scene-draw-filled-2d-rectangle-list (scene color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-filled-2d-rectangle-list draw-data (canonicalize-color color) vertices)))

;; textured-2d-rectangle-list
(defun scene-add-textured-2d-rectangle-list (scene model-mtx texture color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-textured-2d-rectangle-list-primitive draw-data handle model-mtx texture color vertices)))

(defun scene-add-textured-2d-rectangle-list-to-group (scene group texture color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (assert (typep group 'atom))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-textured-2d-rectangle-list-pseudo draw-data handle group texture color vertices)))

(defun scene-draw-textured-2d-rectangle-list (scene texture color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (%draw-data-draw-textured-2d-rectangle-list (im-draw-data scene) texture (canonicalize-color color) vertices))

;; filled-2d-convex-polygon
(defun scene-add-filled-2d-convex-polygon (scene model-mtx color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-2d-convex-polygon-primitive draw-data handle model-mtx color vertices)))

(defun scene-add-filled-2d-convex-polygon-to-group (scene group color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (assert (typep group 'atom))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-2d-convex-polygon-pseudo draw-data handle group color vertices)))

(defun scene-draw-filled-2d-convex-polygon (scene color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (%draw-data-draw-filled-2d-convex-polygon (im-draw-data scene) (canonicalize-color color) vertices))

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

(defun scene-add-filled-2d-circle (scene model-mtx color
                                   center-x center-y radius
                                   &optional (number-of-segments 64))
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real center-x center-y radius))
  (declare (type (integer 1 #.most-positive-fixnum) number-of-segments))
  (setq center-x (coerce center-x 'double-float))
  (setq center-y (coerce center-y 'double-float))
  (setq radius (coerce radius 'double-float))
  (let ((vertices (compute-circle-vertices number-of-segments center-x center-y radius)))
    (rm-dispatch-to-render-thread (scene draw-data handle)
      (%draw-data-add-filled-2d-convex-polygon-primitive draw-data handle model-mtx color vertices))))

(defun scene-add-filled-2d-circle-to-group (scene group color
					    center-x center-y radius
					    &optional (number-of-segments 64))
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real center-x center-y radius))
  (declare (type (integer 1 #.most-positive-fixnum) number-of-segments))
  (assert (typep group 'atom))
  (setq center-x (coerce center-x 'double-float))
  (setq center-y (coerce center-y 'double-float))
  (setq radius (coerce radius 'double-float))
  (let ((vertices (compute-circle-vertices number-of-segments center-x center-y radius)))
    (rm-dispatch-to-render-thread (scene draw-data handle)
      (%draw-data-add-filled-2d-convex-polygon-pseudo draw-data handle group color vertices))))

(defun scene-draw-filled-2d-circle (scene color
				    center-x center-y radius
				    &optional (number-of-segments 64))
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real center-x center-y radius))
  (declare (type (integer 1 #.most-positive-fixnum) number-of-segments))
  (setq center-x (coerce center-x 'double-float))
  (setq center-y (coerce center-y 'double-float))
  (setq radius (coerce radius 'double-float))
  (let ((vertices (compute-circle-vertices number-of-segments center-x center-y radius)))
    (%draw-data-draw-filled-2d-convex-polygon (im-draw-data scene) (canonicalize-color color) vertices)))

;; filled-3d-triangle-strip
(defun scene-add-filled-3d-triangle-strip (scene model-mtx color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-3d-triangle-strip-primitive draw-data handle model-mtx color vertices)))

;; there are no groups for 3d-triangle-strip because strips always need to use cmds
;; maybe we can put group in cmd and in the renderer, if group is present use group model-matrix etc
;; instead of cmd-model-mtx

(defun scene-draw-filled-3d-triangle-strip (scene color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (let ((draw-data (im-draw-data scene)))
    (declare (type immediate-mode-draw-data draw-data))
    (let ((draw-list (draw-data-3d-triangle-strip-draw-list draw-data)))
      ;; we add the primitive/cmd without a handle:
      (%draw-list-add-filled-3d-triangle-strip/list-cmd draw-list nil (canonicalize-color color) vertices))))

;; filled-3d-triangle-list-flat
(defun scene-add-filled-3d-triangle-list-flat (scene model-mtx color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-3d-triangle-list-primitive draw-data handle model-mtx color vertices)))

(defun scene-add-filled-3d-triangle-list-flat-to-group (scene group color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (assert (typep group 'atom))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-3d-triangle-list-pseudo draw-data handle group color vertices)))

(defun scene-draw-filled-3d-triangle-list-flat (scene color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (%draw-data-draw-filled-3d-triangle-list (im-draw-data scene) (canonicalize-color color) vertices))

;; filled-3d-triangle-list-diffuse
(defun scene-add-filled-3d-triangle-list-diffuse (scene model-mtx color vertices light-position)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-3d-triangle-list-with-normals-primitive
     draw-data handle model-mtx color vertices light-position)))

(defun scene-add-filled-3d-triangle-list-diffuse-to-group (scene group color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (assert (typep group 'atom))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-3d-triangle-list-with-normals-pseudo draw-data handle group color vertices)))

(defun scene-draw-filled-3d-triangle-list-diffuse (scene color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (%draw-data-draw-filled-3d-triangle-list-with-normals (im-draw-data scene) (canonicalize-color color) vertices))

;; filled-3d-triangle-strip-flat
(defun scene-add-filled-3d-triangle-strip-flat (scene model-mtx color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-3d-triangle-strip-primitive draw-data handle model-mtx color vertices)))

;; triangle strips do not have pseudo-cmds and therefore will
;; not have scene-add-filled-3d-triangle-strip-flat-to-group

(defun scene-draw-filled-3d-triangle-strip-flat (scene color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (let ((draw-data (im-draw-data scene)))
    (declare (type immediate-mode-draw-data draw-data))
    (let ((draw-list (draw-data-3d-triangle-strip-draw-list draw-data)))
      ;; we add the primitive/cmd without a handle:
      (%draw-list-add-filled-3d-triangle-strip/list-cmd
       draw-list nil (canonicalize-color color) vertices))))

;; filled-3d-triangle-strip-diffuse
(defun scene-add-filled-3d-triangle-strip-diffuse (scene model-mtx color vertices light-position)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-3d-triangle-strip-with-normals-primitive
     draw-data handle model-mtx color vertices light-position)))

;; triangle strips do not have pseudo-cmds and therefore will
;; not have scene-add-filled-3d-triangle-strip-diffuse-to-group

(defun scene-draw-filled-3d-triangle-strip-diffuse (scene color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (let ((draw-data (im-draw-data scene)))
    (declare (type immediate-mode-draw-data draw-data))
    (let ((draw-list (draw-data-3d-triangle-strip-with-normals-draw-list draw-data)))
      ;; we add the primitive/cmd without a handle:
      (%draw-list-add-filled-3d-triangle-strip/list-with-normals-cmd
       draw-list nil (canonicalize-color color) vertices nil))))

;; filled-3d-convex-polygon-diffuse
(defun scene-add-filled-3d-convex-polygon-diffuse (scene model-mtx color vertices light-position)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-3d-convex-polygon-with-normals-primitive
     draw-data handle model-mtx color vertices light-position)))

(defun scene-add-filled-3d-convex-polygon-diffuse-to-group (scene group color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (assert (typep group 'atom))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-3d-convex-polygon-with-normals-pseudo draw-data group color vertices)))

(defun scene-draw-filled-3d-convex-polygon-diffuse (scene color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-filled-3d-convex-polygon-with-normals draw-data (canonicalize-color color) vertices)))

;; filled-3d-convex-polygon-flat
(defun scene-add-filled-3d-convex-polygon-flat (scene model-mtx color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-3d-convex-polygon-primitive draw-data handle model-mtx color vertices)))

(defun scene-add-filled-3d-convex-polygon-flat-to-group (scene group color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (assert (typep group 'atom))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-3d-convex-polygon-pseudo draw-data handle group color vertices)))

(defun scene-draw-filled-3d-convex-polygon-flat (scene color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-filled-3d-convex-polygon
     draw-data (canonicalize-color color) vertices)))

;; muticolor-3d-convex-polygon-diffuse
(defun scene-add-multicolor-3d-convex-polygon-diffuse (scene model-mtx vertices light-position)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-multicolor-3d-convex-polygon-with-normals-primitive  
     draw-data handle model-mtx vertices light-position)))

(defun scene-add-multicolor-3d-convex-polygon-diffuse-to-group (scene group vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (assert (typep group 'atom))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-multicolor-3d-convex-polygon-with-normals-pseudo draw-data group vertices)))

(defun scene-draw-multicolor-3d-convex-polygon-diffuse (scene vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-multicolor-3d-convex-polygon-with-normals  
     (draw-data-3d-triangle-list-with-normals-draw-list draw-data) vertices)))

;; multicolor-3d-convex-polygon-flat
(defun scene-add-multicolor-3d-convex-polygon-flat (scene model-mtx vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-multicolor-3d-convex-polygon-primitive draw-data handle model-mtx vertices)))

(defun scene-add-multicolor-3d-convex-polygon-flat-to-group (scene group vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (assert (typep group 'atom))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-multicolor-3d-convex-polygon-pseudo draw-data handle group vertices)))

(defun scene-draw-multicolor-3d-convex-polygon-flat (scene vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-multicolor-3d-convex-polygon draw-data vertices)))

;; textured-3d-triangle-list-flat
(defun scene-add-textured-3d-triangle-list-flat (scene model-mtx texture color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-textured-3d-triangle-list-primitive draw-data handle model-mtx texture color vertices)))

(defun scene-add-textured-3d-triangle-list-flat-to-group (scene group texture color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (assert (typep group 'atom))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-textured-3d-triangle-list-pseudo draw-data handle group texture color vertices)))

(defun scene-draw-textured-3d-triangle-list-flat (scene texture color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (let ((draw-data (im-draw-data scene)))
    (%draw-data-draw-textured-3d-triangle-list draw-data texture (canonicalize-color color) vertices)))

;; textured-3d-triangle-strip-flat
(defun scene-add-textured-3d-triangle-strip-flat (scene model-mtx texture color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-textured-3d-triangle-strip-primitive draw-data handle model-mtx texture color vertices)))

(defun scene-draw-textured-3d-triangle-strip-flat (scene texture color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type sequence vertices))
  (let ((draw-data (im-draw-data scene)))
    (declare (type immediate-mode-draw-data draw-data))
    (let ((draw-list (draw-data-3d-triangle-strip-draw-list draw-data)))
      ;; we add the primitive/cmd without a handle:
      (%draw-list-add-textured-3d-triangle-strip/list-cmd
       draw-list nil texture (canonicalize-color color) vertices))))

(defun scene-add-filled-sphere-diffuse
    (scene model-mtx color origin-x origin-y origin-z radius resolution
     &optional (light-position (vec3 10000 10000 10000)))
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real origin-x origin-y origin-z))
  (declare (type (integer 2 #.most-positive-fixnum) resolution))
  (setq origin-x (coerce origin-x 'double-float))
  (setq origin-y (coerce origin-y 'double-float))
  (setq origin-z (coerce origin-z 'double-float))
  (setq radius (coerce radius 'double-float))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-sphere-primitive
     draw-data handle model-mtx color origin-x origin-y origin-z radius resolution light-position)))

(defun scene-add-filled-sphere-diffuse-to-group (scene group color origin-x origin-y origin-z radius resolution)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real origin-x origin-y origin-z))
  (declare (type (integer 2 #.most-positive-fixnum) resolution))
  (assert (typep group 'atom))
  (setq origin-x (coerce origin-x 'double-float))
  (setq origin-y (coerce origin-y 'double-float))
  (setq origin-z (coerce origin-z 'double-float))
  (setq radius (coerce radius 'double-float))
  (setq color (canonicalize-color color))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-sphere-pseudo
     draw-data handle group color origin-x origin-y origin-z radius resolution)))

(defun scene-draw-filled-sphere-diffuse (scene color origin-x origin-y origin-z radius resolution)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real origin-x origin-y origin-z))
  (declare (type (integer 2 #.most-positive-fixnum) resolution))
  (setq origin-x (coerce origin-x 'double-float))
  (setq origin-y (coerce origin-y 'double-float))
  (setq origin-z (coerce origin-z 'double-float))
  (setq radius (coerce radius 'double-float))
  (setq color (canonicalize-color color))
  (%draw-data-draw-filled-sphere (im-draw-data scene) color origin-x origin-y origin-z radius resolution))

;; 2d-text
(defun compute-text-coordinates (pos-x pos-y string glyph-table scale-w scale-h)
  (loop for char across string
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
	do (setf glyph (gethash char glyph-table))
	when (and glyph (not (eq (getf glyph :id) 32))) ;; hack to deal with #\space artifact
	  do
	     (setq width (clampf (getf glyph :width)))
	     (setq height (clampf (getf glyph :height)))
	     (setq u0 (clampf (/ (getf glyph :x) scale-w)))
	     (setq v0 (clampf (/ (getf glyph :y) scale-h)))
	     (setq u1 (+ u0 (clampf (/ width scale-w))))
	     (setq v1 (+ v0 (clampf (/ height scale-h))))
	     (setq x0 (+ pos-x (clampf (getf glyph :xoffset))))
	     (setq y0 (+ pos-y (clampf (getf glyph :yoffset))))
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
          do (incf dx (clampf (getf glyph :xadvance)))
	finally (return (nreverse coords))))

(defun scene-add-text (scene model-matrix font color pos-x pos-y string)
  (declare (type real pos-x pos-y))
  (declare (type string string))
  (let ((data (font-data font)))
    (declare (type 3b-bmfont-common:bmfont data))
    (let* ((glyph-table (slot-value data '3b-bmfont-common::chars))
           (scale-w (3b-bmfont-common:scale-w data))
           (scale-h (3b-bmfont-common:scale-h data))
	   (pos-x (clampf pos-x))
	   (pos-y (clampf pos-y))
	   (color (canonicalize-color color))
           (vertices (compute-text-coordinates pos-x pos-y string glyph-table scale-w scale-h)))
    (rm-dispatch-to-render-thread (scene draw-data handle)
      (%draw-data-add-text-quad-list-primitive draw-data handle
					       model-matrix
					       font color
					       vertices)))))

(defun scene-add-text-to-group (scene group font color pos-x pos-y string)
  (declare (type real pos-x pos-y))
  (declare (type string string))
  (assert (typep group 'atom))
  (let ((data (font-data font)))
    (declare (type 3b-bmfont-common:bmfont data))
    (let* ((glyph-table (slot-value data '3b-bmfont-common::chars))
           (scale-w (3b-bmfont-common:scale-w data))
           (scale-h (3b-bmfont-common:scale-h data))
	   (pos-x (clampf pos-x))
	   (pos-y (clampf pos-y))
	   (color (canonicalize-color color))
           (vertices (compute-text-coordinates pos-x pos-y string glyph-table scale-w scale-h)))
      (rm-dispatch-to-render-thread (scene draw-data handle)
	(%draw-data-add-text-quad-list-pseudo draw-data handle
					      group
                                              font color
                                              vertices)))))

(defun scene-draw-text (scene font color pos-x pos-y string)
  (declare (type real pos-x pos-y))
  (declare (type string string))
  (let ((data (font-data font)))
    (declare (type 3b-bmfont-common:bmfont data))
    (let* ((glyph-table (slot-value data '3b-bmfont-common::chars))
           (scale-w (3b-bmfont-common:scale-w data))
           (scale-h (3b-bmfont-common:scale-h data))
	   (pos-x (clampf pos-x))
	   (pos-y (clampf pos-y))
           (vertices (compute-text-coordinates pos-x pos-y string glyph-table scale-w scale-h)))
      (%draw-data-draw-text-quad-list (im-draw-data scene) font (canonicalize-color color) vertices))))

(defun %reinstance-cmd (cmd model-mtx line-thickness color-override point-size light-position)
  (declare (type standard-draw-indexed-cmd cmd))
  (declare (type (or real null) line-thickness))
  (let ((first-idx (cmd-first-idx cmd))
        (elem-count (cmd-elem-count cmd))
        (vtx-offset (cmd-vtx-offset cmd))
        (draw-list (cmd-draw-list cmd))
        (texture (cmd-texture cmd))
        (model-mtx (if model-mtx model-mtx (cmd-model-mtx cmd)))
        (line-thickness (if line-thickness (clampf line-thickness) (cmd-line-thickness cmd)))
        (color-override (if color-override (canonicalize-color color-override) (cmd-color-override cmd)))
	(point-size (if point-size (clampf point-size) (cmd-point-size cmd)))
	(light-position (if light-position light-position (cmd-light-position cmd))))
    (let ((cmd (make-standard-draw-indexed-cmd 
                draw-list
                first-idx elem-count vtx-offset
                model-mtx color-override texture line-thickness point-size light-position)))
      (vector-push-extend cmd (draw-list-cmd-vector draw-list))
      cmd)))


;; the following "primitive" modifiers only work on retained mode primitives using handle
(defun reinstance-primitive (scene handle
                             &key (model-matrix nil)
			       (point-size nil)
                               (line-thickness nil)
                               (color-override nil)
			       (light-position nil))
  (declare (type krma-essential-scene-mixin scene))
  (let ((draw-data (rm-draw-data scene)))
    (let ((dd0 (svref draw-data 0))
          (dd1 (svref draw-data 1)))
      (declare (type retained-mode-draw-data dd0 dd1))
      (let ((ht0 (draw-data-handle-hash-table dd0))
            (ht1 (draw-data-handle-hash-table dd1))
            (wq0 (draw-data-work-queue dd0))
            (wq1 (draw-data-work-queue dd1))
            (new-handle (gen-rm-handle)))
        (flet ((reinstance! (ht)
                 (let ((cmd (gethash handle ht)))
                   (if cmd
                       (if (listp cmd)
			   ;; pseudo-primitives cannot be reinstanced
                           (warn "failed to reinstance ~S" handle)
                           (setf (gethash new-handle ht)
                                 (%reinstance-cmd
				  cmd model-matrix line-thickness color-override point-size light-position)))
                       (warn "failed to reinstance ~S" handle)))))

          (sb-concurrency:enqueue #'(lambda ()
                                      (reinstance! ht0))
                                  wq0)
          (sb-concurrency:enqueue #'(lambda ()
                                      (reinstance! ht1))
                                  wq1)
          new-handle)))))

;; need to be able to modify existing primitives
(defun primitive-set-color (scene handle color)
  (declare (type krma-essential-scene-mixin scene))
  (let ((draw-data (rm-draw-data scene))
	(color (canonicalize-color color)))
    (let ((dd0 (svref draw-data 0))
          (dd1 (svref draw-data 1)))
      (declare (type retained-mode-draw-data dd0 dd1))
      (let ((ht0 (draw-data-handle-hash-table dd0))
            (ht1 (draw-data-handle-hash-table dd1))
            (wq0 (draw-data-work-queue dd0))
            (wq1 (draw-data-work-queue dd1)))
        (flet ((update-color! (ht)
                 (let ((cmd (gethash handle ht)))
                   (if (null cmd)
                       (warn "could not find primitive ~S to set color." handle)
                       (if (listp cmd)
                           (loop for index in (cdr cmd)
                                 do (let* ((index-array (draw-list-index-array (car cmd)))
                                           (vertex-array (draw-list-vertex-array (car cmd)))
                                           (vtx-type (foreign-array-foreign-type vertex-array))
                                           (vtx-type-size (foreign-array-foreign-type-size vertex-array))
                                           (vtx-offset (mem-aref index-array
                                                                 (foreign-array-foreign-type index-array)
                                                                 index))
					   (pointer (inc-pointer (foreign-array-ptr vertex-array)
								 (* vtx-offset vtx-type-size))))

                                      (setf (foreign-slot-value pointer vtx-type 'col)
                                            color)))
                           (setf (cmd-color-override cmd) color))))))
          (sb-concurrency:enqueue #'(lambda () (update-color! ht0)) wq0)
          (sb-concurrency:enqueue #'(lambda () (update-color! ht1)) wq1)
          (values))))))

(defun primitive-set-light-position (scene handle pos)
  (declare (type krma-essential-scene-mixin scene))
  (let ((draw-data (rm-draw-data scene)))
    (let ((dd0 (svref draw-data 0))
          (dd1 (svref draw-data 1)))
      (declare (type retained-mode-draw-data dd0 dd1))
      (let ((ht0 (draw-data-handle-hash-table dd0))
            (ht1 (draw-data-handle-hash-table dd1))
            (wq0 (draw-data-work-queue dd0))
            (wq1 (draw-data-work-queue dd1)))
        (flet ((update-light-pos! (ht)
                 (let ((cmd (gethash handle ht)))
                   (if (null cmd)
                       (warn "could not find primitive ~S to set light position." handle)
                       (if (listp cmd)
			   (warn "cannot set light position of pseudo-primitive ~s." handle)
			   (setf (cmd-light-position cmd) pos))))))
          (sb-concurrency:enqueue #'(lambda () (update-light-pos! ht0)) wq0)
          (sb-concurrency:enqueue #'(lambda () (update-light-pos! ht1)) wq1)
          (values))))))

;; replaces model-mtx in cmd by this matrix
(defun primitive-set-transform (scene handle matrix)
  (declare (type krma-essential-scene-mixin scene))
  (let ((draw-data (rm-draw-data scene)))
    (let ((dd0 (svref draw-data 0))
          (dd1 (svref draw-data 1)))
      (declare (type retained-mode-draw-data dd0 dd1))
      (let ((ht0 (draw-data-handle-hash-table dd0))
            (ht1 (draw-data-handle-hash-table dd1))
            (wq0 (draw-data-work-queue dd0))
            (wq1 (draw-data-work-queue dd1)))
        (flet ((update-matrix! (ht)
                 (let ((cmd (gethash handle ht)))
                   (if (listp cmd)
                       (warn "could not set transform for pseudo-primitive ~S." handle)
                       (setf (cmd-model-mtx cmd) matrix)))))
          (sb-concurrency:enqueue #'(lambda () (update-matrix! ht0)) wq0)
          (sb-concurrency:enqueue #'(lambda () (update-matrix! ht1)) wq1)
          (values))))))

;; multiplies new matrix against old matrix and replaces model-mtx in cmd
(defun primitive-apply-transform (scene handle matrix)
  (declare (type krma-essential-scene-mixin scene))
  (let ((draw-data (rm-draw-data scene)))
    (let ((dd0 (svref draw-data 0))
          (dd1 (svref draw-data 1)))
      (declare (type retained-mode-draw-data dd0 dd1))
      (let ((ht0 (draw-data-handle-hash-table dd0))
            (ht1 (draw-data-handle-hash-table dd1))
            (wq0 (draw-data-work-queue dd0))
            (wq1 (draw-data-work-queue dd1)))
        (flet ((multiply-matrix! (ht)
                 (let ((cmd (gethash handle ht)))
                   (if (listp cmd)
                       (warn "could not apply transform for pseudo-primitive ~S." handle)
                       (setf (cmd-model-mtx cmd) (funcall *multiply-matrix-function* matrix (cmd-model-mtx cmd)))))))
          (sb-concurrency:enqueue #'(lambda () (multiply-matrix! ht0)) wq0)
          (sb-concurrency:enqueue #'(lambda () (multiply-matrix! ht1)) wq1)
          (values))))))

(defun primitive-set-line-thickness (scene handle thickness)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real thickness))
  (let ((draw-data (rm-draw-data scene)))
    (let ((dd0 (svref draw-data 0))
          (dd1 (svref draw-data 1)))
      (declare (type retained-mode-draw-data dd0 dd1))
      (let ((ht0 (draw-data-handle-hash-table dd0))
            (ht1 (draw-data-handle-hash-table dd1))
            (wq0 (draw-data-work-queue dd0))
            (wq1 (draw-data-work-queue dd1)))
        (flet ((update-thickness! (ht)
                 (let ((cmd (gethash handle ht)))
                   (if (listp cmd)
                       (warn "could not update line thickness for pseudo-primitive ~S." handle)
                       (setf (cmd-line-thickness cmd) thickness)))))
          (sb-concurrency:enqueue #'(lambda () (update-thickness! ht0)) wq0)
          (sb-concurrency:enqueue #'(lambda () (update-thickness! ht1)) wq1)
          (values))))))

(defun delete-primitive (scene handle)
  (declare (type krma-essential-scene-mixin scene))
  (let ((draw-data (rm-draw-data scene)))
    (let ((dd0 (svref draw-data 0))
          (dd1 (svref draw-data 1)))
      (declare (type retained-mode-draw-data dd0 dd1))
      (let ((ht0 (draw-data-handle-hash-table dd0))
            (ht1 (draw-data-handle-hash-table dd1))
            (wq0 (draw-data-work-queue dd0))
            (wq1 (draw-data-work-queue dd1)))
        (flet ((delete-handle-from! (ht)
                 (let ((cmd (gethash handle ht)))
                   (if (not cmd)
                       (warn "could not find primitive ~S" handle)
                       (if (listp cmd)
                           ;; point lists and line lists: rebuild index-array and remove handle
                           ;; todo: use memcpy if these index-arrays are big
			   (let* ((old-index-array (draw-list-index-array (car cmd)))
                                  (ptr (foreign-array-ptr old-index-array))
                                  (type (foreign-array-foreign-type old-index-array))
                                  (new-index-array
                                    (make-index-array
                                     (foreign-array-allocated-count old-index-array)
                                     (foreign-array-foreign-type old-index-array)
                                     0
                                     (foreign-alloc
                                      (foreign-array-foreign-type old-index-array)
                                      :count (foreign-array-allocated-count old-index-array))
                                     (foreign-type-size (foreign-array-foreign-type old-index-array)))))
                             ;; copy indexes
                             (loop for i from 0 below (foreign-array-fill-pointer old-index-array)
                                   unless (member i (cdr cmd))
                                     do (index-array-push-extend new-index-array (mem-aref ptr type i)))
                             (setf (draw-list-index-array (car cmd)) new-index-array)
                             (setf (draw-list-needs-compaction? (car cmd)) t)
                             (foreign-free (foreign-array-ptr old-index-array))
                             (remhash handle ht))

                           ;; other primitives: delete cmd and remove handle
                           (let* ((draw-list (cmd-draw-list cmd))
                                  (cmd-vector (draw-list-cmd-vector draw-list)))
                             (loop for entry across cmd-vector
                                   for i from 0
                                   when (eq cmd entry)
                                     do (setf (aref cmd-vector i) nil)
                                        (setf (draw-list-needs-compaction? draw-list) t)
                                        (remhash handle ht)
                                        (return))))))))
          (sb-concurrency:enqueue #'(lambda ()
                                      (delete-handle-from! ht0))
                                  wq0)
          (sb-concurrency:enqueue #'(lambda ()
                                      (delete-handle-from! ht1))
                                  wq1)
          (values))))))
