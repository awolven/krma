(in-package :krma)

(defclass camera-mixin ()
  ((projection-matrix :initform nil :accessor camera-proj-matrix)
   (view-matrix :initform nil :accessor camera-view-matrix)
   (position :initform (vec3 0 0 1000)   :accessor camera-position)
   (rotation :initform (quat 0 0 0 1) :accessor camera-rotation)
   (width :initform 640 :initarg :width :accessor camera-width)
   (height :initform 480 :initarg :height :accessor camera-height)
   (near :initform *default-znear* :initarg :near :accessor camera-near)
   (far :initform *default-zfar* :initarg :far :accessor camera-far)))

(defclass perspective-camera (camera-mixin)
  ((fov :initform 45 :initarg :fov :accessor camera-fov)))

(defclass orthographic-camera (camera-mixin)
  ())

(defmethod set-camera-width-height ((self camera-mixin) new-width new-height camera-type)
  (declare (ignore camera-type))
  (setf (slot-value self 'width) new-width)
  (setf (slot-value self 'height) new-height)
  (%recompute-projection-matrix self)
  (values))

(defmethod un-project ((self camera-mixin) (point vec2) &optional (depth 0))
  (let* ((x (- (/ (* 2.0d0 (vx point)) (slot-value self 'width)) 1.0d0))
	 (y (- (/ (* 2.0d0 (vy point)) (slot-value self 'height)) 1.0d0))
	 (view-matrix (camera-view-matrix self))
	 (proj-matrix (camera-proj-matrix self))
	 (view-proj-m (m* proj-matrix view-matrix))
	 (inv (minv view-proj-m)))
    (safe-euclid (m* inv (vec4 x y depth 1)))))

(defmethod %recompute-projection-matrix ((camera orthographic-camera))
  (setf (camera-proj-matrix camera)
	(mortho-vulkan 0 (slot-value camera 'width)
		       (slot-value camera 'height) 0
		       0 (- (slot-value camera 'far) (slot-value camera 'near)))))

(defmethod %recompute-projection-matrix ((camera perspective-camera))
  (setf (camera-proj-matrix camera)
	(mperspective-vulkan (camera-fov camera)
			     (/ (camera-width camera) (camera-height camera))
			     (camera-near camera) (camera-far camera))))

(defun %recompute-view-matrix (camera)
  (let* ((position (camera-position camera))
	 (rotation (camera-rotation camera))
	 (translation-matrix (mat4 1 0 0 (vx position)
				   0 1 0 (vy position)
				   0 0 1 (vz position)
				   0 0 0 1))
	 (rotation-matrix (matrix-from-quaternion rotation))
	 (camera-matrix (m* translation-matrix rotation-matrix)))
    (setf (camera-view-matrix camera) (minv camera-matrix))))

(defun arcball-camera-about (camera delta-x-ndc delta-y-ndc target)
  (let* ((delta-azimuth (* 2.0d0 pi delta-x-ndc))
	 (delta-elevation (* pi delta-y-ndc))
	 (initial-rotation (camera-rotation camera))
	 (azimuth-rotation (qrotation (vec3 0 0 1) delta-azimuth))
	 (right-vector (rotate-vector (vec3 1 0 0) (q* azimuth-rotation initial-rotation)))
	 (elevation-rotation (qrotation right-vector delta-elevation))
	 (initial-relative-position (v- (camera-position camera) target))
	 (new-relative-rotation (q* elevation-rotation azimuth-rotation))
	 (new-position (v+ target
			 (rotate-vector initial-relative-position new-relative-rotation)))
	 (new-rotation (q* new-relative-rotation initial-rotation)))

    (setf (camera-position camera) new-position
	  (camera-rotation camera) new-rotation)

    (%recompute-projection-matrix camera)
    (%recompute-view-matrix camera)
    (values)))

(defstruct ray
  (origin)
  (direction))

(defun compute-picking-ray (window viewport)
  (multiple-value-bind (mouse-x mouse-y) (window-cursor-position window)
    (let ((camera (krma::viewport-3d-camera viewport)))
      (let ((mouse-world (un-project camera (vec2 mouse-x mouse-y)))
	    (p (camera-position camera)))
	(typecase camera
	  (perspective-camera (let ((dir (vunit (v- mouse-world p))))
				(make-ray :origin p :direction dir)))
	  (orthographic-camera (let ((aim (vunit (rotate-vector p (camera-rotation camera)))))
				 ;; this branch of this function is untested.
				 (make-ray :origin mouse-world :direction aim))))))))

(defun intersect-ray-with-plane (ray plane-equation)
  (let* ((ray-direction (ray-direction ray))
	 (ray-origin (ray-origin ray))
	 (denom (+ (* (vx ray-direction) (vx plane-equation))
		   (* (vy ray-direction) (vy plane-equation))
		   (* (vz ray-direction) (vz plane-equation)))))
    (when (zerop denom)
      (return-from intersect-ray-with-plane nil))
    (let* ((param (- (/ (+ (* (vx ray-origin) (vx plane-equation))
			   (* (vy ray-origin) (vy plane-equation))
			   (* (vz ray-origin) (vz plane-equation))
			   (vw plane-equation))
			denom)))
	   (vec3 (v+ ray-origin (v* param ray-direction))))
      (values param vec3))))

(defmethod initialize-instance :before ((window krma-window-mixin) &rest initargs)
  (declare (ignore initargs))
  (multiple-value-bind (width height) (if #+(or (and linux x11) win32)(slot-boundp window 'clui::handle)
					  #+cocoa(slot-boundp window 'clui::id)
					  (window-framebuffer-size window)
					  (values 640 480))
    (setf (krma::window-viewports window)
	  (list (krma::make-viewport
		 :x 0 :y 0
		 :width width
		 :height height
		 :2d-camera (make-camera
			     :proj-matrix (mortho-vulkan 0 width height 0 0 +select-box-2d-depth+)
			     :view-matrix (mlookat (vec3 0 0 +select-box-2d-depth+) (vec3 0 0 0) (vec3 0 1 0)))
		 :3d-camera (let ((camera (make-instance 'perspective-camera
							 :width width
							 :height height)))
			      (%recompute-projection-matrix camera)
			      (%recompute-view-matrix camera)
			      camera))))
    (values)))

(defstruct mouse-click-record
  (zero-origin)
  (last-ndc (vec2 0 0))
  (zero-origin-world))

(defmethod clim:handle-event :after ((window krma-window-mixin) (event clim:pointer-button-press-event))
  (let ((button (clui::input-event-code event)))
    (cond ((eq button clui::+pointer-middle-button+)
	   (if (middle-click-mode-record window)
	       (setf (middle-click-mode-record window) nil)
	       (setf (middle-click-mode-record window)
		     (make-mouse-click-record
		      :zero-origin (vec2 (clim:pointer-event-x event) (clim:pointer-event-y event))
		      :zero-origin-world (let* ((viewport (first (krma::window-viewports window)))
						(camera (krma::viewport-3d-camera viewport)))
					   (camera-position camera))))))))

  (values))
		 

(defmethod clim:handle-event :after ((window krma-window-mixin) (event clim:pointer-motion-event))
  (let ((mouse-click-record (middle-click-mode-record window)))
    (when mouse-click-record
      (let ((mouse-zero-origin (mouse-click-record-zero-origin mouse-click-record))
	    (last-ndc (mouse-click-record-last-ndc mouse-click-record)))
	(multiple-value-bind (width height) (window-framebuffer-size window)
	  (let* ((new-x-ndc (/ (- (vx mouse-zero-origin) (clim:pointer-event-x event)) width))
		 (new-y-ndc (/ (- (vy mouse-zero-origin) (clim:pointer-event-y event)) height))
		 (viewport (first (krma::window-viewports window)))
		 (camera (krma::viewport-3d-camera viewport))
		 (point (vec3 0 0 0) #+NIL(un-project camera mouse-zero-origin 0)))
	    (arcball-camera-about camera
				 (- new-x-ndc (vx last-ndc))
				 (- new-y-ndc (vy last-ndc))
				 point)
	    (setf (mouse-click-record-last-ndc mouse-click-record) (vec2 new-x-ndc new-y-ndc)))))))
  (values))

(defun graphics-zoom (window event)
  (let* ((vp (first (krma::window-viewports window)))
	 (camera (krma::viewport-3d-camera vp))
	 (origin (camera-position camera))
	 (dir (vunit
	       (v- (un-project camera (vec2 (clim:pointer-event-x event) (clim:pointer-event-y event)) 1.0d0)
		   origin)))
	 (coefficient (* -0.1 (clui::pointer-wheel-event-yoffset event) (vlength origin)))
	 (new-origin (v+ origin (v* coefficient dir))))
    (setf (camera-position camera) new-origin)
    (%recompute-view-matrix camera)))

(defmethod clim:handle-event ((window krma-window-mixin) (event clui::pointer-wheel-event-mixin))
  (graphics-zoom window event)
  (values))
