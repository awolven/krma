(in-package :krma)

(defun quaternion-to-euler-angles.d (q)
  (declare (type 3dm.d::quat q))
  (let* ((sinr*cosp (* 2.0d0 (+ (* (qw q) (qx q)) (* (qy q) (qz q)))))
	 (cosr*cosp (- 1.0d0 (* 2.0d0 (+ (* (qx q) (qx q)) (* (qy q) (qy q))))))
	 (roll (atan sinr*cosp cosr*cosp))
	 (qty (* 2.0d0 (- (* (qw q) (qy q)) (* (qx q) (qz q)))))
	 (sinp (sqrt (+ 1.0d0 qty)))
	 (cosp (sqrt (- 1.0d0 qty)))
	 (pitch (- (* 2.0d0 (atan sinp cosp)) #.(/ pi 2.0d0)))
	 (siny*cosp (* 2.0d0 (+ (* (qw q) (qz q)) (* (qx q) (qy q)))))
	 (cosy*cosp (- 1.0d0 (* 2.0d0 (+ (* (qy q) (qy q)) (* (qz q) (qz q))))))
	 (yaw (atan siny*cosp cosy*cosp)))
    (values roll pitch yaw)))


(defun scene-draw-object-aligned-foreground-3d-xy-arc (scene camera group closed? translation
						       rotation
						       cx cy cz radius start-angle end-angle
						       line-thickness color
						       &optional (object-id 0))
  (let* ((delta-theta (- end-angle start-angle))
	 (sign (signum delta-theta))
	 (niter (floor (abs (/ (* delta-theta 180.0d0) pi))))
	 (verts (make-array niter))
	 (view-matrix (minv (slot-value camera 'adhoc-scene-graph::rotation-matrix)))
	 (reflect (mrotate (mrotate (meye 4) (vec3 0 1 0) pi) (vec3 0 0 1) pi)))
    (loop for i from 0 below niter
	  with theta = end-angle
	  with step = #.(/ pi 180.0d0)
	  do (let ((angle (- theta (* sign i step))))
	       (setf (aref verts i)
		     (safe-euclid
		      (m* translation reflect view-matrix rotation
			  (vec4 (+ cx (* radius (cos angle)))
				(+ cy (* radius (sin angle)))
				cz
				1.0d0))))))
    (scene-draw-filled-foreground-3d-instanced-line-primitive
     scene group closed? line-thickness color
     (nconc
      (let ((start
	      (safe-euclid
	       (m* translation reflect view-matrix rotation
		   (vec4 (+ cx (* radius (cos start-angle)))
			 (+ cy (* radius (sin start-angle)))
			 cz
			 1.0d0)))))
	(list (vx start) (vy start) (vz start)))
      (loop for coord across verts
	    with vertices = ()
	    do (setq vertices (cons (vx coord) (cons (vy coord) (cons (vz coord) vertices))))
	    finally (return vertices)))
     object-id)))

(defun scene-draw-object-aligned-foreground-3d-yz-arc (scene camera group closed? translation
						       rotation
						       cx cy cz radius start-angle end-angle
						       line-thickness color
						       &optional (object-id 0))
  (let* ((delta-theta (- end-angle start-angle))
	 (sign (signum delta-theta))
	 (niter (floor (abs (/ (* delta-theta 180.0d0) pi))))
	 (verts (make-array niter))
	 (view-matrix (minv (slot-value camera 'adhoc-scene-graph::rotation-matrix)))
	 (reflect (mrotate (mrotate (meye 4) (vec3 0 1 0) pi) (vec3 0 0 1) pi)))
    (loop for i from 0 below niter
	  with theta = end-angle
	  with step = #.(/ pi 180.0d0)
	  do (let ((angle (- theta (* sign i step))))
	       (setf (aref verts i)
		     (safe-euclid
		      (m* translation reflect view-matrix rotation
			  (vec4 cx
				(+ cy (* radius (cos angle)))
				(+ cz (* radius (sin angle)))
				1.0d0))))))
    (scene-draw-filled-foreground-3d-instanced-line-primitive
     scene group closed? line-thickness color
     (nconc
      (let ((start
	      (safe-euclid
	       (m* translation reflect view-matrix rotation
		   (vec4 cx
			 (+ cy (* radius (cos start-angle)))
			 (+ cz (* radius (sin start-angle)))
			 1.0d0)))))
	(list (vx start) (vy start) (vz start)))
      (loop for coord across verts
	    with vertices = ()
	    do (setq vertices (cons (vx coord) (cons (vy coord) (cons (vz coord) vertices))))
	    finally (return vertices)))
     object-id)))

(defun scene-draw-object-aligned-foreground-3d-xz-arc (scene camera group closed? translation
						       rotation
						       cx cy cz radius start-angle end-angle
						       line-thickness color
						       &optional (object-id 0))
  (let* ((delta-theta (- end-angle start-angle))
	 (sign (signum delta-theta))
	 (niter (floor (abs (/ (* delta-theta 180.0d0) pi))))
	 (verts (make-array niter))
	 (view-matrix (minv (slot-value camera 'adhoc-scene-graph::rotation-matrix)))
	 (reflect (mrotate (mrotate (meye 4) (vec3 0 1 0) pi) (vec3 0 0 1) pi)))
    (loop for i from 0 below niter
	  with theta = end-angle
	  with step = #.(/ pi 180.0d0)
	  do (let ((angle (- theta (* sign i step))))
	       (setf (aref verts i)
		     (safe-euclid
		      (m* translation reflect view-matrix rotation
			  (vec4 (+ cx (* radius (cos angle)))
				cy
				(+ cz (* radius (sin angle)))
				1.0d0))))))
    (scene-draw-filled-foreground-3d-instanced-line-primitive
     scene group closed? line-thickness color
     (nconc
      (let ((start
	      (safe-euclid
	       (m* translation reflect view-matrix rotation
		   (vec4 (+ cx (* radius (cos start-angle)))
			 cy
			 (+ cz (* radius (sin start-angle)))
			 1.0d0)))))
	(list (vx start) (vy start) (vz start)))
      (loop for coord across verts
	    with vertices = ()
	    do (setq vertices (cons (vx coord) (cons (vy coord) (cons (vz coord) vertices))))
	    finally (return vertices)))
     object-id)))


(defconstant -pi/2 #.(/ pi -2))
(defconstant pi/2 #.(/ pi 2))

(defvar *gizmo-roll-oid* (adhoc-scene-graph::new-object-id))
(defvar *gizmo-pitch-oid* (adhoc-scene-graph::new-object-id))
(defvar *gizmo-yaw-oid* (adhoc-scene-graph::new-object-id))
(defvar *gizmo-ring-oid* (adhoc-scene-graph::new-object-id))

#+NOTYET
(let* ((scene (default-scene))
       (window (main-window clim:*default-frame-manager*))
       (viewport (first (window-viewports window)))
       (camera (viewport-3d-camera viewport))
       (object rgn-geo::df)
       (world-origin (slot-value object 'adhoc-scene-graph::origin))
       
       )
  (setf (immediate-mode-work-function-6 (default-display))
	#'(lambda ()
	    (multiple-value-bind (roll pitch yaw) (quaternion-to-euler-angles.d (adhoc::send rgn-geo::df adhoc-scene-graph::rotation))
	      (let* ((rotation (matrix-from-quaternion (adhoc::send rgn-geo::df adhoc-scene-graph::rotation)))
		     (origin (safe-euclid
			      (m* (camera-proj-matrix camera) (camera-view-matrix camera)
				  (vec4 (vx world-origin) (vy world-origin) (vz world-origin) 1.0d0))))
		     (origin (vec3 (* (/ (+ (vx origin) 1.0d0) 2.0d0) (adhoc::send camera climish::width))
				   (* (/ (+ (vy origin) 1.0d0) 2.0d0) (adhoc::send camera climish::height))
				   500)))
				
		(scene-draw-object-aligned-foreground-3d-xy-arc
		 scene camera :default nil (nmtranslate (meye 4) origin) rotation
		 0 0 0 100 -pi/2 pi/2 5
		 (if (and (eq *gizmo-yaw-oid* (climish::most-specifically-hovered-3d
					       (krma::krma-select-box-3d window) 0 0))
			  (not (eq *gizmo-ring-oid* (climish::most-specifically-hovered-2d
						     (krma::krma-select-box-2d window) 0 0))))
		     #xff7700ff
		     #xffff)
		 *gizmo-yaw-oid*)
		(scene-draw-object-aligned-foreground-3d-xz-arc
		 scene camera :default nil (nmtranslate (meye 4) origin) rotation
		 0 0 0 100 -pi/2 pi/2 5
		 (if (and (eq *gizmo-pitch-oid* (climish::most-specifically-hovered-3d
						 (krma::krma-select-box-3d window) 0 0))
			  (not (eq *gizmo-ring-oid* (climish::most-specifically-hovered-2d
						     (krma::krma-select-box-2d window) 0 0))))
		     #xff7700ff
		     #xff00ff)
		 *gizmo-pitch-oid*)
		(scene-draw-object-aligned-foreground-3d-yz-arc
		 scene camera :default nil (nmtranslate (meye 4) origin) rotation 
		 0 0 0 100 -pi/2 pi/2 5
		 (if (or (and (eq *gizmo-roll-oid* (climish::most-specifically-hovered-3d
						    (krma::krma-select-box-3d window) 0 0))
			      (not (eq *gizmo-ring-oid* (climish::most-specifically-hovered-2d
							 (krma::krma-select-box-2d window) 0 0))))
			 (and (climish::dragging-something? window)
			      (eq (climish::drag-target window) :roll)))
		     #xff7700ff
		     #xff0000ff)
		 *gizmo-roll-oid*)
		(scene-draw-2d-circle
		 scene :default 10
		 (if (eq *gizmo-ring-oid* (climish::most-specifically-hovered-2d
					   (krma::krma-select-box-2d window) 0 0))
		     #xff7700ff
		     #xffffffff)
		 (vx origin) (vy origin) 100 180 *gizmo-ring-oid*))))))

#+NIL
(setf (immediate-mode-work-function-6 (default-display))
      #'(lambda ()
	  (scene-draw-filled-foreground-3d-instanced-line-primitive
	   (default-scene) :default nil 
	   10 #xffff (list 0 0 10 100 100 10))))
	   



(defun scene-add-rotation-gizmo (scene group quaternion center radius
				 camera oid-roll oid-pitch oid-yaw oid-ring)
  (multiple-value-bind (roll pitch yaw) (quaternion-to-euler-angles.d quaternion)
    (let ((roll-matrix (mrotate (meye 4) (vec3 0 0 1) roll))
	  (pitch-matrix (mrotate (meye 4) (vec3 0 1 0) pitch))
	  (yaw-matrix (mrotate (meye 4) (vec3 1 0 0) yaw)))
      (list (scene-add-3d-xy-arc scene group roll-matrix nil
				 (vx center) (vy center) (vz center) radius 0 pi
				 4.0f0 #xff0000ff oid-roll)
	    (scene-add-3d-yz-arc scene group pitch-matrix nil
				 (vx center) (vy center) (vz center) radius 0 pi
				 4.0f0 #xff00ff oid-pitch)
	    (scene-add-3d-xz-arc scene group yaw-matrix nil
				 (vx center) (vy center) (vz center) radius 0 pi
				 4.0f0 #xffff oid-yaw)
	    (scene-add-3d-xy-arc scene group (camera-view-matrix camera) nil
				 (vx center) (vy center) (vz center) radius 0 2pi
				 4.0 #xffffffff oid-ring)))))



(defun add-rotation-gizmo (window object group)
  (let* ((viewport (first (window-viewports window)))
	 (camera (viewport-3d-camera viewport)))
    (scene-add-rotation-gizmo (slot-value object 'scene)
			      group
			      (slot-value object 'adhoc-scene-graph::rotation)
			      (vec3 0 0 0)
			      10.0d0
			      camera
			      *gizmo-roll-oid*
			      *gizmo-pitch-oid*
			      *gizmo-yaw-oid*
			      *gizmo-ring-oid*)))
			      
			      
			      
				   
				    
				   
    
