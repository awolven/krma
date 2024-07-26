(in-package :krma)

(eval-when (:compile-toplevel :load-toplevel)
  (when *muffle-compilation-notes*
    #+sbcl(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))))

(eval-when (:compile-toplevel :load-toplevel)
  (when krma::*debug*
    (declaim (optimize (safety 3) (debug 3))))
  (unless krma::*debug*
    (declaim (optimize (speed 3) (safety 0) (debug 0)))
    (declaim (inline %draw-data-add-2d-point-primitive))
    (declaim (inline %draw-data-add-2d-point))
    (declaim (inline %draw-data-draw-2d-point))
    (declaim (inline %draw-data-add-3d-point-primitive))
    (declaim (inline %draw-data-add-3d-point))
    (declaim (inline %draw-data-draw-3d-point))
    (declaim (inline %draw-data-add-2d-line-primitive))
    (declaim (inline %draw-data-add-2d-line))
    (declaim (inline %draw-data-draw-2d-line))
    (declaim (inline %draw-data-add-3d-line-primitive))
    (declaim (inline %draw-data-add-3d-line))
    (declaim (inline %draw-data-draw-3d-line))
    (declaim (inline %draw-data-add-2d-triangle-primitive))
    (declaim (inline %draw-data-add-multicolor-2d-polyline-primitive))
    (declaim (inline %draw-data-add-multicolor-2d-polyline))
    (declaim (inline %draw-data-draw-multicolor-2d-polyline))
    (declaim (inline %draw-data-add-2d-polyline-primitive))
    (declaim (inline %draw-data-add-2d-polyline))
    (declaim (inline %draw-data-draw-2d-polyline))
    (declaim (inline %draw-data-add-2d-circular-arc-primitive))
    (declaim (inline %draw-data-add-2d-circular-arc))
    (declaim (inline %draw-data-draw-2d-circular-arc))
    (declaim (inline %draw-data-add-2d-circle-primitive))
    (declaim (inline %draw-data-add-2d-circle))
    (declaim (inline %draw-data-draw-2d-circle))
    (declaim (inline %draw-data-add-multicolor-3d-polyline-primitive))
    (declaim (inline %draw-data-add-multicolor-3d-polyline))
    (declaim (inline %draw-data-draw-multicolor-3d-polyline))
    (declaim (inline %draw-data-add-3d-polyline-primitive))
    (declaim (inline %draw-data-add-3d-polyline))
    (declaim (inline %draw-data-draw-3d-polyline))
    (declaim (inline %draw-data-add-filled-2d-triangle-list-primitive))
    (declaim (inline %draw-data-add-filled-2d-triangle-list))
    (declaim (inline %draw-data-draw-filled-2d-triangle-list))
    (declaim (inline %draw-data-add-filled-2d-triangle-strip-primitive))
    (declaim (inline %draw-data-add-filled-2d-rectangle-list-primitive))
    (declaim (inline %draw-data-add-filled-2d-rectangle-list))
    (declaim (inline %draw-data-draw-filled-2d-rectangle-list))
    (declaim (inline %draw-data-add-textured-2d-rectangle-list-primitive))
    (declaim (inline %draw-data-add-textured-2d-rectangle-list))
    (declaim (inline %draw-data-draw-textured-2d-rectangle-list))
    (declaim (inline %draw-data-add-filled-2d-convex-polygon-primitive))
    (declaim (inline %draw-data-add-filled-2d-convex-polygon))
    (declaim (inline %draw-data-draw-filled-2d-convex-polygon))
    (declaim (inline %draw-data-add-filled-3d-convex-polygon-primitive))
    (declaim (inline %draw-data-add-filled-3d-convex-polygon))
    (declaim (inline %draw-data-draw-filled-3d-convex-polygon))
    (declaim (inline %draw-data-add-multicolor-3d-convex-polygon-primitive))
    (declaim (inline %draw-data-add-multicolor-3d-convex-polygon))
    (declaim (inline %draw-data-draw-multicolor-3d-convex-polygon))
    (declaim (inline %draw-data-add-filled-3d-convex-polygon-with-normals-primitive))
    (declaim (inline %draw-data-add-filled-3d-convex-polygon-with-normals))
    (declaim (inline %draw-data-draw-filled-3d-convex-polygon-with-normals))
    (declaim (inline %draw-data-add-multicolor-3d-convex-polygon-with-normals-primitive))
    (declaim (inline %draw-data-add-multicolor-3d-convex-polygon-with-normals))
    (declaim (inline %draw-data-draw-multicolor-3d-convex-polygon-with-normals))
    (declaim (inline %draw-data-add-filled-3d-triangle-list-primitive))
    (declaim (inline %draw-data-add-filled-3d-triangle-list))
    (declaim (inline %draw-data-draw-filled-3d-triangle-list))
    (declaim (inline %draw-data-add-filled-3d-triangle-strip-primitive))
    (declaim (inline %draw-data-add-filled-3d-triangle-list-with-normals-primitive))
    (declaim (inline %draw-data-add-filled-3d-triangle-list-with-normals))
    (declaim (inline %draw-data-draw-filled-3d-triangle-list-with-normals))
    (declaim (inline %draw-data-add-multicolor-3d-triangle-list-with-normals-primitive))
    (declaim (inline %draw-data-add-multicolor-3d-triangle-list-with-normals))
    (declaim (inline %draw-data-draw-multicolor-3d-triangle-list-with-normals))
    (declaim (inline %draw-data-add-filled-3d-triangle-strip-with-normals-primitive))
    (declaim (inline %draw-data-add-textured-3d-triangle-list-primitive))
    (declaim (inline %draw-data-add-textured-3d-triangle-list))
    (declaim (inline %draw-data-draw-textured-3d-triangle-list))
    (declaim (inline %draw-data-add-textured-3d-triangle-strip-primitive))
    (declaim (inline %draw-data-add-filled-sphere-primitive))
    (declaim (inline %draw-data-add-filled-sphere))
    (declaim (inline %draw-data-draw-filled-sphere))
    (declaim (inline %draw-data-add-text-quad-list-primitive))
    (declaim (inline %draw-data-add-text-quad-list))
    (declaim (inline %draw-data-draw-text-quad-list))))

(defstruct (standard-draw-data
	     (:conc-name "DRAW-DATA-")
             (:constructor make-standard-draw-data
			   (name)))
  (name)
  (dpy)
  ;; key is (group sf-point-size) for rm and just sf-point-size for im
  (2d-point-list-draw-list-table (make-hash-table :test #'equal))
  ;; key is (group sf-line-thickness) for rm and just sf-line-thickness for im
  (2d-line-list-draw-list-table (make-hash-table :test #'equal))
  ;; key is (group texture) for rm and just texture for im
  (2d-triangle-list-draw-list-table (make-hash-table :test #'equal))
  ;; key is (group texture) for rm and just texture for im
  (2d-triangle-list-draw-list-for-text-table (make-hash-table :test #'equal))
  ;; key is (group sf-point-size) for rm and sf-point-size for im
  (3d-point-list-draw-list-table (make-hash-table :test #'equal))
  ;; key is (group sf-line-thickness) for rm and sf-line-thickness for im
  (3d-line-list-draw-list-table (make-hash-table :test #'equal))
  ;; key is (group texture) for rm and just texture for im
  (3d-triangle-list-draw-list-table (make-hash-table :test #'equal))
  ;; key is (group texture) for rm and just texture for im
  (3d-triangle-list-with-normals-draw-list-table (make-hash-table :test #'equal))

  ;; even in immediate mode, strips must use cmds
  (2d-line-strip-draw-list (make-instance '3d-vertex-draw-list))
  (2d-triangle-strip-draw-list (make-instance '3d-vertex-draw-list))
  (2d-instanced-line-draw-list (make-instance '3d-vertex-draw-list))
  (3d-line-strip-draw-list (make-instance '3d-vertex-draw-list))
  (3d-triangle-strip-draw-list (make-instance '3d-vertex-draw-list))
  (3d-instanced-tube-draw-list (make-instance '3d-vertex-draw-list))
  (3d-triangle-strip-with-normals-draw-list (make-instance '3d-vertex-with-normal-draw-list))

  (group-hash-table (make-hash-table :test #'eq))
  (work-queue (lparallel.queue:make-queue))
  (deletion-queue (lparallel.queue:make-queue)))


(defstruct (immediate-mode-draw-data
	     (:include standard-draw-data)
             (:constructor make-immediate-mode-draw-data (name dpy))))

(defstruct (retained-mode-draw-data
	     (:include standard-draw-data)
             (:conc-name "RM-DRAW-DATA-")
             (:constructor make-retained-mode-draw-data (name dpy)))

  (2d-point-list-draw-list (make-instance '3d-vertex-draw-list))
  (2d-line-list-draw-list (make-instance '3d-vertex-draw-list))
  (2d-triangle-list-draw-list (make-instance '3d-vertex-draw-list))
  (2d-triangle-list-draw-list-for-text (make-instance '3d-vertex-draw-list))
  (3d-point-list-draw-list (make-instance '3d-vertex-draw-list))
  (3d-line-list-draw-list (make-instance '3d-vertex-draw-list))
  (3d-triangle-list-draw-list (make-instance '3d-vertex-draw-list))
  (3d-triangle-list-with-normals-draw-list (make-instance '3d-vertex-with-normal-draw-list))

  (handle-hash-table (make-hash-table :test #'eq)))


(defun %draw-data-add-2d-point-primitive (draw-data handle ub32-oid atom-group model-mtx sf-point-size ub32-color sf-elevation sf-x sf-y)
  (declare (type retained-mode-draw-data draw-data))
  (declare (type single-float sf-point-size))
  (let* ((draw-list (rm-draw-data-2d-point-list-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-2d-point draw-list ub32-oid atom-group model-mtx sf-point-size ub32-color sf-elevation sf-x sf-y))
    (values)))


(defun %draw-data-add-2d-point (draw-data ub32-oid atom-group sf-point-size ub32-color sf-elevation sf-x sf-y)
  (declare (type retained-mode-draw-data draw-data))
  (declare (type single-float sf-point-size))
  (let* ((draw-list-table (rm-draw-data-2d-point-list-draw-list-table draw-data))
	 (group-hash-table (rm-draw-data-group-hash-table draw-data))
	 (key (list atom-group sf-point-size))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
					     :point-size sf-point-size
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-2d-point draw-list ub32-oid ub32-color sf-elevation sf-x sf-y)
    (values)))


(defun %draw-data-draw-2d-point (draw-data ub32-oid atom-group sf-point-size ub32-color sf-elevation sf-x sf-y)
  (declare (type immediate-mode-draw-data draw-data))
  (declare (type single-float sf-point-size))
  (let* ((draw-list-table (draw-data-2d-point-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
         (key (list atom-group sf-point-size))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
					     :point-size sf-point-size
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-2d-point draw-list ub32-oid ub32-color sf-elevation sf-x sf-y)
    (values)))


(defun %draw-data-add-3d-point-primitive (draw-data handle ub32-oid atom-group model-mtx sf-point-size ub32-color sf-x sf-y sf-z)
  (declare (type retained-mode-draw-data draw-data))
  (declare (type single-float sf-point-size))
  (let* ((draw-list (rm-draw-data-3d-point-list-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-3d-point draw-list ub32-oid atom-group model-mtx sf-point-size ub32-color sf-x sf-y sf-z))
    (values)))


(defun %draw-data-add-3d-point (draw-data ub32-oid atom-group sf-point-size ub32-color sf-x sf-y sf-z)
  (declare (type retained-mode-draw-data draw-data))
  (declare (type single-float sf-point-size))
  (let* ((draw-list-table (rm-draw-data-3d-point-list-draw-list-table draw-data))
	 (group-hash-table (rm-draw-data-group-hash-table draw-data))
	 (key (list atom-group sf-point-size))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
					     :point-size sf-point-size
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-3d-point draw-list ub32-oid ub32-color sf-x sf-y sf-z)
    (values)))


(defun %draw-data-draw-3d-point (draw-data ub32-oid atom-group sf-point-size ub32-color sf-x sf-y sf-z)
  (declare (type immediate-mode-draw-data draw-data))
  (declare (type single-float sf-point-size))
  (let* ((draw-list-table (draw-data-3d-point-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
         (key (list atom-group sf-point-size))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
					     :point-size sf-point-size
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-3d-point draw-list ub32-oid ub32-color sf-x sf-y sf-z)
    (values)))


(defun %draw-data-add-2d-line-primitive
    (draw-data handle ub32-oid atom-group model-mtx sf-line-thickness ub32-color sf-elevation sf-x0 sf-y0 sf-x1 sf-y1)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list (rm-draw-data-2d-line-list-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-2d-line draw-list ub32-oid atom-group model-mtx sf-line-thickness ub32-color sf-elevation sf-x0 sf-y0 sf-x1 sf-y1))
    (values)))

(defun %draw-data-add-2d-line
    (draw-data ub32-oid atom-group sf-line-thickness ub32-color sf-elevation sf-x0 sf-y0 sf-x1 sf-y1)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (rm-draw-data-2d-line-list-draw-list-table draw-data))
	 (group-hash-table (rm-draw-data-group-hash-table draw-data))
	 (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
					     :line-thickness sf-line-thickness
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-2d-line draw-list ub32-oid ub32-color sf-elevation sf-x0 sf-y0 sf-x1 sf-y1)
    (values)))


(defun %draw-data-draw-2d-line (draw-data ub32-oid atom-group sf-line-thickness ub32-color sf-elevation sf-x0 sf-y0 sf-x1 sf-y1)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
         (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
					     :line-thickness sf-line-thickness
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-2d-line draw-list ub32-oid ub32-color sf-elevation sf-x0 sf-y0 sf-x1 sf-y1)
    (values)))


(defun %draw-data-add-3d-line-primitive
    (draw-data handle ub32-oid atom-group model-mtx sf-line-thickness ub32-color sf-x0 sf-y0 sf-z0 sf-x1 sf-y1 sf-z1)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list (rm-draw-data-3d-line-list-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-3d-line draw-list ub32-oid atom-group model-mtx sf-line-thickness ub32-color
				  sf-x0 sf-y0 sf-z0 sf-x1 sf-y1 sf-z1))
    (values)))


(defun %draw-data-add-3d-line
    (draw-data ub32-oid atom-group sf-line-thickness ub32-color sf-x0 sf-y0 sf-z0 sf-x1 sf-y1 sf-z1)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (rm-draw-data-3d-line-list-draw-list-table draw-data))
	 (group-hash-table (rm-draw-data-group-hash-table draw-data))
	 (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
					     :line-thickness sf-line-thickness
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-3d-line draw-list ub32-oid ub32-color sf-x0 sf-y0 sf-z0 sf-x1 sf-y1 sf-z1)
    (values)))

(defun %draw-data-draw-3d-line (draw-data ub32-oid atom-group sf-line-thickness ub32-color sf-x0 sf-y0 sf-z0 sf-x1 sf-y1 sf-z1)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-line-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
         (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
                                             :line-thickness sf-line-thickness
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-3d-line draw-list ub32-oid ub32-color sf-x0 sf-y0 sf-z0 sf-x1 sf-y1 sf-z1)
    (values)))


(defun %draw-data-add-2d-triangle-primitive (draw-data handle ub32-oid
					     atom-group model-mtx sf-line-thickness ub32-color sf-elevation
					     sf-x0 sf-y0 sf-x1 sf-y1 sf-x2 sf-y2)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list (rm-draw-data-2d-line-strip-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
	  (%draw-list-add-2d-polyline draw-list ub32-oid atom-group model-mtx t sf-line-thickness ub32-color sf-elevation
				      (list sf-x0 sf-y0 sf-x1 sf-y1 sf-x2 sf-y2)))
    
    (values)))


(defun %draw-data-add-multicolor-2d-polyline-primitive
    (draw-data handle ub32-oid atom-group model-mtx bool-closed? sf-line-thickness sf-elevation seq-vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (rm-draw-data-2d-line-strip-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-multicolor-2d-polyline
	   draw-list ub32-oid atom-group model-mtx bool-closed? sf-line-thickness sf-elevation seq-vertices))
    (values)))


(defun %draw-data-add-multicolor-2d-polyline
    (draw-data ub32-oid atom-group bool-closed? sf-line-thickness sf-elevation seq-vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (rm-draw-data-2d-line-list-draw-list-table draw-data))
         (group-hash-table (rm-draw-data-group-hash-table draw-data))
	 (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
                                             :line-thickness sf-line-thickness
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-multicolor-2d-polyline draw-list ub32-oid bool-closed? sf-elevation seq-vertices)
    (values)))

(defun %draw-data-add-multicolor-2d-instanced-line-primitive
    (draw-data handle ub32-oid atom-group model-mtx bool-closed? sf-line-thickness sf-elevation seq-vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list (rm-draw-data-2d-instanced-line-draw-list draw-data))
	 (cmd (%draw-list-add-filled-2d-triangle-strip/list draw-list ub32-oid atom-group model-mtx #xffffffff sf-elevation
							    (list 0.0f0 -0.5f0
								  1.0f0 -0.5f0
								  1.0f0  0.5f0
								  0.0f0 -0.5f0
								  1.0f0  0.5f0
								  0.0f0  0.5f0)))
	 (list (make-instance '2d-polyline-instance-list))
	 (instance-array (instance-list-array list)))
    (handler-case 
      (etypecase seq-vertices
	(list
	 (loop for (x1 y1 color1) on (cdddr seq-vertices) by #'cdddr
	    for (x0 y0 color0) on seq-vertices by #'cdddr
	    do (setq x0 (clampf x0))
	      (setq y0 (clampf y0))
	      (setq color0 (canonicalize-color color0))
	      (setq x1 (clampf x1))
	      (setq y1 (clampf y1))
	      (setq color1 (canonicalize-color color1))
	      (2d-vertex-instance-array-push-extend instance-array ub32-oid x0 y0 color0)
	      (2d-vertex-instance-array-push-extend instance-array ub32-oid x1 y1 color1)
	    finally (when bool-closed?
		      (2d-vertex-instance-array-push-extend instance-array ub32-oid
							    (clampf (car seq-vertices))
							    (clampf (cadr seq-vertices))
							    (canonicalize-color (caddr seq-vertices)))))))
      (error () (setq list nil)))
    (setf (cmd-instance-array cmd) list)
    (setf (cmd-point-size cmd) sf-line-thickness)
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
	  cmd)
    (values)))


(defun %draw-data-add-filled-3d-instanced-tube-primitive
    (draw-data handle ub32-oid atom-group model-mtx bool-closed? sf-line-thickness ub32-color seq-vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list (rm-draw-data-3d-instanced-tube-draw-list draw-data))
	 (cmd (%draw-list-add-filled-3d-triangle-strip  
	       draw-list ub32-oid atom-group model-mtx ub32-color
	       (loop for i from 0 to 9
		     append (loop for j from 0 below 2
				  append (list (cos (* pi (/ i 9)))
					       (sin (* pi (/ i 9)))
					       j
					       (cos (* pi (/ (1+ i) 9)))
					       (sin (* pi (/ (1+ i) 9)))
					       j
					       (cos (* pi (/ i 9)))
					       (sin (* pi (/ i 9)))
					       (mod (1+ j) 2))))))
	 (list (make-instance '3d-polyline-instance-list))
	 (instance-array (instance-list-array list)))
    (handler-case 
	(etypecase seq-vertices
	  (list
	   (loop for (x1 y1 z1) on (cdddr seq-vertices) by #'cdddr
		 for (x0 y0 z0) on seq-vertices by #'cdddr
		 do (setq x0 (clampf x0))
		    (setq y0 (clampf y0))
		    (setq z0 (clampf z0))
		    (setq x1 (clampf x1))
		    (setq y1 (clampf y1))
		    (setq z1 (clampf z1))
		    (3d-vertex-instance-array-push-extend instance-array ub32-oid x0 y0 z0)
		    (3d-vertex-instance-array-push-extend instance-array ub32-oid x1 y1 z1)
		 finally (when bool-closed?
			   (3d-vertex-instance-array-push-extend instance-array ub32-oid
								 (clampf (car seq-vertices))
								 (clampf (cadr seq-vertices))
								 (clampf (caddr seq-vertices)))))))
      (error () (setq list nil)))
	 (setf (cmd-instance-array cmd) list)
	 (setf (cmd-point-size cmd) sf-line-thickness)
	 (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
	       cmd)
	 (values)))

(defun %draw-data-draw-multicolor-2d-polyline
    (draw-data ub32-oid atom-group bool-closed? sf-line-thickness sf-elevation seq-vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
         (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
                                             :line-thickness sf-line-thickness
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-multicolor-2d-polyline draw-list ub32-oid bool-closed? sf-elevation seq-vertices)
    (values)))


(defun %draw-data-add-2d-polyline-primitive
    (draw-data handle ub32-oid atom-group model-mtx closed? sf-line-thickness ub32-color sf-elevation seq-vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (rm-draw-data-2d-line-strip-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-2d-polyline
           draw-list ub32-oid atom-group model-mtx closed? sf-line-thickness ub32-color sf-elevation seq-vertices))
    (values)))


(defun %draw-data-add-2d-polyline
    (draw-data ub32-oid atom-group closed? sf-line-thickness ub32-color sf-elevation seq-vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (rm-draw-data-2d-line-list-draw-list-table draw-data))
         (group-hash-table (rm-draw-data-group-hash-table draw-data))
	 (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
                                             :line-thickness sf-line-thickness
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-2d-polyline draw-list ub32-oid closed? ub32-color sf-elevation seq-vertices)
    (values)))


(defun %draw-data-draw-2d-polyline (draw-data ub32-oid atom-group closed? sf-line-thickness ub32-color sf-elevation seq-vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
         (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
                                             :line-thickness sf-line-thickness
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-2d-polyline draw-list ub32-oid closed? ub32-color sf-elevation seq-vertices))
  (values))


(defun %draw-data-add-2d-circular-arc-primitive (draw-data handle ub32-oid atom-group model-mtx closed? sf-line-thickness ub32-color sf-elevation
						 center-x center-y radius start-angle end-angle
						 number-of-segments)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (rm-draw-data-2d-line-strip-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-2d-circular-arc
           draw-list ub32-oid atom-group model-mtx
	   closed? sf-line-thickness ub32-color sf-elevation
           center-x center-y radius start-angle end-angle
           number-of-segments))
    (values)))


(defun %draw-data-add-2d-circular-arc (draw-data ub32-oid atom-group closed? sf-line-thickness ub32-color sf-elevation
				       center-x center-y radius start-angle end-angle
				       number-of-segments)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (rm-draw-data-2d-line-list-draw-list-table draw-data))
	 (group-hash-table (rm-draw-data-group-hash-table draw-data))
	 (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
					     :line-thickness sf-line-thickness
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-2d-circular-arc draw-list ub32-oid
                                     closed? ub32-color sf-elevation
                                     center-x center-y radius start-angle end-angle
                                     number-of-segments))
  (values))


(defun %draw-data-draw-2d-circular-arc (draw-data ub32-oid atom-group closed? sf-line-thickness ub32-color sf-elevation
                                        center-x center-y radius start-angle end-angle
                                        number-of-segments)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
         (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
                                             :line-thickness sf-line-thickness
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-2d-circular-arc draw-list ub32-oid
                                     closed? ub32-color sf-elevation
                                     center-x center-y radius start-angle end-angle
                                     number-of-segments)
    (values)))


(defun %draw-data-add-2d-circle-primitive (draw-data handle ub32-oid atom-group model-mtx sf-line-thickness ub32-color sf-elevation
					   center-x center-y radius number-of-segments)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (rm-draw-data-2d-line-strip-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-2d-circle
           draw-list ub32-oid atom-group model-mtx sf-line-thickness ub32-color sf-elevation
           center-x center-y radius
           number-of-segments))
    (values)))


(defun %draw-data-add-2d-circle (draw-data ub32-oid atom-group sf-line-thickness ub32-color sf-elevation
				 center-x center-y radius number-of-segments)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (rm-draw-data-2d-line-list-draw-list-table draw-data))
	 (group-hash-table (rm-draw-data-group-hash-table draw-data))
	 (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
					     :line-thickness sf-line-thickness
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-2d-circle draw-list ub32-oid ub32-color sf-elevation center-x center-y radius number-of-segments)
    (values)))


(defun %draw-data-draw-2d-circle (draw-data ub32-oid atom-group sf-line-thickness ub32-color sf-elevation
				  center-x center-y radius number-of-segments)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
         (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
                                             :line-thickness sf-line-thickness
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-2d-circle draw-list ub32-oid ub32-color sf-elevation center-x center-y radius number-of-segments)
    (values)))


(defun %draw-data-add-multicolor-3d-polyline-primitive (draw-data handle ub32-oid atom-group model-mtx closed? sf-line-thickness vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (rm-draw-data-3d-line-strip-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-multicolor-3d-polyline
           draw-list ub32-oid atom-group model-mtx closed? sf-line-thickness vertices))
    (values)))


(defun %draw-data-add-multicolor-3d-polyline (draw-data ub32-oid atom-group closed? sf-line-thickness vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (rm-draw-data-3d-line-list-draw-list-table draw-data))
	 (group-hash-table (rm-draw-data-group-hash-table draw-data))
	 (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
					     :line-thickness sf-line-thickness
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-multicolor-3d-polyline draw-list ub32-oid closed? vertices)
    (values)))


(defun %draw-data-draw-multicolor-3d-polyline (draw-data ub32-oid atom-group closed? sf-line-thickness vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-line-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
         (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
                                             :line-thickness sf-line-thickness
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-multicolor-3d-polyline draw-list ub32-oid closed? vertices)
    (values)))


(defun %draw-data-add-3d-polyline-primitive (draw-data handle ub32-oid atom-group model-mtx closed? line-thickness color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (rm-draw-data-3d-line-strip-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-3d-polyline
           draw-list ub32-oid atom-group
           model-mtx closed? line-thickness color vertices))
    (values)))


(defun %draw-data-add-3d-polyline (draw-data ub32-oid atom-group closed? sf-line-thickness color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (rm-draw-data-3d-line-list-draw-list-table draw-data))
	 (group-hash-table (rm-draw-data-group-hash-table draw-data))
	 (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
					     :line-thickness sf-line-thickness
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-3d-polyline draw-list ub32-oid closed? color vertices)
    (values)))


(defun %draw-data-draw-3d-polyline (draw-data ub32-oid atom-group closed? sf-line-thickness color vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-line-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
         (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
                                             :line-thickness sf-line-thickness
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-3d-polyline draw-list ub32-oid closed? color vertices)
    (values)))


(defun %draw-data-add-filled-2d-triangle-list-primitive (draw-data handle ub32-oid atom-group model-mtx color sf-elevation vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list (rm-draw-data-2d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
	  (%draw-list-add-filled-2d-triangle-strip/list draw-list ub32-oid atom-group model-mtx color sf-elevation vertices))
    (values)))


(defun %draw-data-add-filled-2d-triangle-list (draw-data ub32-oid atom-group ub32-color sf-elevation vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (rm-draw-data-2d-triangle-list-draw-list-table draw-data))
	 (group-hash-table (rm-draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
					     :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-filled-2d-triangle-list draw-list ub32-oid ub32-color sf-elevation vertices)
    (values)))


(defun %draw-data-draw-filled-2d-triangle-list (draw-data ub32-oid atom-group color sf-elevation vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-triangle-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
                                             :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-filled-2d-triangle-list draw-list ub32-oid color sf-elevation vertices)
    (values)))


(defun %draw-data-add-filled-2d-triangle-strip-primitive (draw-data handle ub32-oid atom-group model-mtx ub32-color sf-elevation vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (rm-draw-data-2d-triangle-strip-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-2d-triangle-strip/list draw-list ub32-oid atom-group model-mtx ub32-color sf-elevation vertices))
    (values)))


(defun %draw-data-add-filled-2d-rectangle-list-primitive (draw-data handle ub32-oid atom-group model-mtx ub32-color sf-elevation vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list (rm-draw-data-2d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
	  (%draw-list-add-filled-2d-rectangle-list draw-list ub32-oid atom-group model-mtx ub32-color sf-elevation vertices))
    (values)))


(defun %draw-data-add-filled-2d-rectangle-list (draw-data ub32-oid atom-group ub32-color sf-elevation vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (rm-draw-data-2d-triangle-list-draw-list-table draw-data))
	 (group-hash-table (rm-draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
					     :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-filled-2d-rectangle-list draw-list ub32-oid ub32-color sf-elevation vertices)
    (values)))


(defun %draw-data-draw-filled-2d-rectangle-list (draw-data ub32-oid atom-group ub32-color sf-elevation vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-triangle-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
                                             :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-filled-2d-rectangle-list draw-list ub32-oid ub32-color sf-elevation vertices)
    (values)))


(defun %draw-data-add-textured-2d-rectangle-list-primitive (draw-data handle ub32-oid atom-group model-mtx texture ub32-color sf-elevation vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (rm-draw-data-2d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-textured-2d-rectangle-list draw-list ub32-oid atom-group model-mtx texture ub32-color sf-elevation vertices))
    (values)))


(defun %draw-data-add-textured-2d-rectangle-list (draw-data ub32-oid atom-group texture ub32-color sf-elevation vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (rm-draw-data-2d-triangle-list-draw-list-table draw-data))
	 (group-hash-table (rm-draw-data-group-hash-table draw-data))
	 (key (list atom-group texture))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
					     :texture texture
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-textured-2d-rectangle-list draw-list ub32-oid ub32-color sf-elevation vertices)
    (values)))


(defun %draw-data-draw-textured-2d-rectangle-list (draw-data ub32-oid atom-group texture ub32-color sf-elevation vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-triangle-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
         (key (list atom-group texture))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
                                             :texture texture
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-textured-2d-rectangle-list draw-list ub32-oid ub32-color sf-elevation vertices)
    (values)))


(defun %draw-data-add-filled-2d-convex-polygon-primitive (draw-data handle ub32-oid atom-group model-mtx ub32-color sf-elevation vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (rm-draw-data-2d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-2d-convex-polygon
           draw-list ub32-oid atom-group
           model-mtx ub32-color sf-elevation vertices))
    (values)))


(defun %draw-data-add-filled-2d-convex-polygon (draw-data ub32-oid atom-group ub32-color sf-elevation vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (rm-draw-data-2d-triangle-list-draw-list-table draw-data))
	 (group-hash-table (rm-draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
					     :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-filled-2d-convex-polygon draw-list ub32-oid ub32-color sf-elevation vertices)
    (values)))


(defun %draw-data-draw-filled-2d-convex-polygon (draw-data ub32-oid atom-group ub32-color sf-elevation vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-triangle-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
                                             :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-filled-2d-convex-polygon draw-list ub32-oid ub32-color sf-elevation vertices)
    (values)))


(defun %draw-data-add-filled-3d-convex-polygon-primitive (draw-data handle ub32-oid atom-group model-mtx ub32-color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (rm-draw-data-3d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-3d-convex-polygon
           draw-list ub32-oid atom-group
           model-mtx ub32-color vertices))
    (values)))


(defun %draw-data-add-filled-3d-convex-polygon (draw-data ub32-oid atom-group ub32-color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (rm-draw-data-3d-triangle-list-draw-list-table draw-data))
	 (group-hash-table (rm-draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
					     :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-filled-3d-convex-polygon draw-list ub32-oid ub32-color vertices)
    (values)))


(defun %draw-data-draw-filled-3d-convex-polygon (draw-data ub32-oid atom-group ub32-color vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-with-normal-draw-list
					     :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-filled-3d-convex-polygon draw-list ub32-oid ub32-color vertices)
    (values)))


(defun %draw-data-add-multicolor-3d-convex-polygon-primitive (draw-data handle ub32-oid atom-group model-mtx vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (rm-draw-data-3d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-multicolor-3d-convex-polygon
           draw-list ub32-oid atom-group
           model-mtx vertices))
    (values)))


(defun %draw-data-add-multicolor-3d-convex-polygon (draw-data ub32-oid atom-group vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (rm-draw-data-3d-triangle-list-draw-list-table draw-data))
	 (group-hash-table (rm-draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
					     :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-multicolor-3d-convex-polygon draw-list ub32-oid vertices)
    (values)))


(defun %draw-data-draw-multicolor-3d-convex-polygon (draw-data ub32-oid atom-group vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
                                             :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-multicolor-3d-convex-polygon draw-list ub32-oid vertices)
    (values)))


(defun %draw-data-add-filled-3d-convex-polygon-with-normals-primitive
    (draw-data handle ub32-oid atom-group model-mtx ub32-color vertices material)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (rm-draw-data-3d-triangle-list-with-normals-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-3d-convex-polygon-with-normals
           draw-list ub32-oid atom-group
           model-mtx ub32-color vertices material))
    (values)))


(defun %draw-data-add-filled-3d-convex-polygon-with-normals (draw-data ub32-oid atom-group ub32-color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (rm-draw-data-3d-triangle-list-with-normals-draw-list-table draw-data))
	 (group-hash-table (rm-draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-with-normal-draw-list
					     :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-filled-3d-convex-polygon-with-normals draw-list ub32-oid ub32-color vertices)
    (values)))


(defun %draw-data-draw-filled-3d-convex-polygon-with-normals (draw-data ub32-oid atom-group ub32-color vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-with-normals-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-with-normal-draw-list
                                             :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-filled-3d-convex-polygon-with-normals draw-list ub32-oid ub32-color vertices)
    (values)))


(defun %draw-data-add-multicolor-3d-convex-polygon-with-normals-primitive
    (draw-data handle ub32-oid atom-group model-mtx vertices material)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list (rm-draw-data-3d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-multicolor-3d-convex-polygon-with-normals
           draw-list ub32-oid atom-group model-mtx vertices material))
    (values)))


(defun %draw-data-add-multicolor-3d-convex-polygon-with-normals (draw-data ub32-oid atom-group vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (rm-draw-data-3d-triangle-list-draw-list-table draw-data))
	 (group-hash-table (rm-draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-with-normal-draw-list
					     :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-multicolor-3d-convex-polygon-with-normals draw-list ub32-oid vertices)
    (values)))


(defun %draw-data-draw-multicolor-3d-convex-polygon-with-normals (draw-data ub32-oid atom-group vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
                                             :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-multicolor-3d-convex-polygon-with-normals draw-list ub32-oid vertices)
    (values)))


(defun %draw-data-add-filled-3d-triangle-list-primitive (draw-data handle ub32-oid atom-group model-mtx ub32-color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (rm-draw-data-3d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-3d-triangle-strip/list draw-list ub32-oid atom-group model-mtx ub32-color vertices))
    (values)))


(defun %draw-data-add-filled-3d-triangle-list (draw-data ub32-oid atom-group ub32-color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (rm-draw-data-3d-triangle-list-draw-list-table draw-data))
	 (group-hash-table (rm-draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
					     :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-filled-3d-triangle-strip/list draw-list ub32-oid ub32-color vertices)
    (values)))


(defun %draw-data-draw-filled-3d-triangle-list (draw-data ub32-oid atom-group ub32-color vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
                                             :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-filled-3d-triangle-strip/list draw-list ub32-oid ub32-color vertices)
    (values)))


(defun %draw-data-add-filled-3d-triangle-strip-primitive (draw-data handle ub32-oid atom-group model-mtx ub32-color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (rm-draw-data-3d-triangle-strip-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-3d-triangle-strip draw-list ub32-oid atom-group model-mtx ub32-color vertices))
    (values)))


;; can't render triangle strips without cmd!!
(defun %draw-data-add-filled-3d-triangle-list-with-normals-primitive
    (draw-data handle ub32-oid atom-group model-mtx ub32-color vertices material)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (rm-draw-data-3d-triangle-list-with-normals-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-3d-triangle-list-with-normals
           draw-list ub32-oid
           atom-group model-mtx ub32-color vertices material))
    (values)))


(defun %draw-data-add-filled-3d-triangle-list-with-normals (draw-data ub32-oid atom-group ub32-color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-with-normals-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-with-normal-draw-list
                                             :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-filled-3d-triangle-strip/list-with-normals draw-list ub32-oid ub32-color vertices)
    (values)))


(defun %draw-data-draw-filled-3d-triangle-list-with-normals (draw-data ub32-oid atom-group ub32-color vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-with-normals-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-with-normal-draw-list
                                             :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-filled-3d-triangle-strip/list-with-normals draw-list ub32-oid ub32-color vertices))
  (values))


(defun %draw-data-add-multicolor-3d-triangle-list-with-normals-primitive
    (draw-data handle ub32-oid atom-group model-mtx vertices material)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (rm-draw-data-3d-triangle-list-with-normals-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-multicolor-3d-triangle-strip/list-with-normals
           draw-list ub32-oid atom-group
           model-mtx vertices material))
    (values)))


(defun %draw-data-add-multicolor-3d-triangle-list-with-normals
    (draw-data ub32-oid atom-group vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (rm-draw-data-3d-triangle-list-with-normals-draw-list-table draw-data))
         (group-hash-table (rm-draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-with-normal-draw-list
                                             :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-multicolor-3d-triangle-strip/list-with-normals draw-list ub32-oid vertices)
    (values)))


(defun %draw-data-draw-multicolor-3d-triangle-list-with-normals (draw-data ub32-oid atom-group vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-with-normals-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-with-normal-draw-list
                                             :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-multicolor-3d-triangle-strip/list-with-normals draw-list ub32-oid vertices))
  (values))


(defun %draw-data-add-filled-3d-triangle-strip-with-normals-primitive
    (draw-data handle ub32-oid atom-group model-mtx ub32-color vertices material)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (rm-draw-data-3d-triangle-strip-with-normals-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-3d-triangle-strip-with-normals
           draw-list ub32-oid atom-group
           model-mtx ub32-color vertices material))
    (values)))

;; can't render triangle strips without cmd!!
(defun %draw-data-add-textured-3d-triangle-list-primitive (draw-data handle ub32-oid atom-group model-mtx texture ub32-color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (rm-draw-data-3d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-textured-3d-triangle-strip/list
           draw-list ub32-oid atom-group
           model-mtx texture ub32-color vertices))
    (values)))


(defun %draw-data-add-textured-3d-triangle-list (draw-data ub32-oid atom-group texture ub32-color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-draw-list-table draw-data))
	 (group-hash-table (draw-data-group-hash-table draw-data))
	 (key (list atom-group texture))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
					     :texture texture
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-textured-3d-triangle-strip/list draw-list ub32-oid ub32-color vertices)
    (values)))


(defun %draw-data-draw-textured-3d-triangle-list (draw-data ub32-oid atom-group texture ub32-color vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
         (key (list atom-group texture))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
                                             :texture texture
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-textured-3d-triangle-strip/list draw-list ub32-oid ub32-color vertices)
    (values)))


(defun %draw-data-add-textured-3d-triangle-strip-primitive (draw-data handle ub32-oid atom-group model-mtx texture ub32-color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (rm-draw-data-3d-triangle-strip-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-textured-3d-triangle-strip/list
           draw-list ub32-oid atom-group
           model-mtx texture ub32-color vertices))
    (values)))


(defun %draw-data-add-filled-sphere-primitive
    (draw-data handle ub32-oid atom-group model-mtx ub32-color origin-x origin-y origin-z radius resolution material)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (rm-draw-data-3d-triangle-list-with-normals-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-sphere
	   draw-list ub32-oid atom-group model-mtx ub32-color origin-x origin-y origin-z radius resolution material))
    (values)))


(defun %draw-data-add-filled-sphere
    (draw-data ub32-oid atom-group ub32-color origin-x origin-y origin-z radius resolution)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (rm-draw-data-3d-triangle-list-with-normals-draw-list-table draw-data))
         (group-hash-table (rm-draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-with-normal-draw-list
                                             :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-filled-sphere draw-list ub32-oid ub32-color origin-x origin-y origin-z radius resolution)
    (values)))


(defun %draw-data-draw-filled-sphere (draw-data ub32-oid atom-group ub32-color origin-x origin-y origin-z radius resolution)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-with-normals-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
         (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-with-normal-draw-list
                                             :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-filled-sphere draw-list ub32-oid ub32-color origin-x origin-y origin-z radius resolution)
    (values)))

(defun %draw-data-add-filled-ellipsoid-primitive
    (draw-data handle ub32-oid atom-group model-mtx ub32-color origin-x origin-y origin-z a b c resolution material)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (rm-draw-data-3d-triangle-list-with-normals-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-ellipsoid
	   draw-list ub32-oid atom-group model-mtx ub32-color origin-x origin-y origin-z a b c resolution material))
    (values)))


(defun %draw-data-add-text-quad-list-primitive (draw-data handle ub32-oid atom-group model-mtx font ub32-color sf-elevation vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (rm-draw-data-2d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (rm-draw-data-handle-hash-table draw-data))
          (%draw-list-add-textured-2d-rectangle-list
           draw-list ub32-oid atom-group model-mtx (font-atlas font) ub32-color sf-elevation vertices
           #'(lambda (&rest args)
               (apply #'make-text-draw-indexed-cmd
                      font args))))
    (values)))

(defun draw-data-add-text-primitive (draw-data group model-matrix font color pos-x pos-y string &optional (object-id 0) (elevation 0))
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
	  (%draw-data-add-text-quad-list-primitive draw-data -2 object-id group
						   (when model-matrix (mcopy model-matrix))
						   font color elevation
						   vertices))))))


(defun %draw-data-add-text-quad-list (draw-data ub32-oid atom-group font ub32-color sf-elevation vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (rm-draw-data-2d-triangle-list-draw-list-table draw-data))
	 (group-hash-table (rm-draw-data-group-hash-table draw-data))
	 (texture (font-atlas font))
	 (key (list atom-group texture))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
					     :texture texture :font font
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-textured-2d-rectangle-list draw-list ub32-oid ub32-color sf-elevation vertices)
    (values)))

(defun draw-data-add-text (draw-data group font color pos-x pos-y string &optional (object-id 0) (elevation 0))
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
	     (vertices (compute-text-coordinates pos-x pos-y string glyph-table scale-w scale-h)))
	(when vertices
	  (let ((color (canonicalize-color color))
		(elevation (clampf elevation)))

	    (%draw-data-add-text-quad-list draw-data object-id group font color elevation vertices)))))))


(defun %draw-data-draw-text-quad-list (draw-data ub32-oid atom-group font ub32-color sf-elevation vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-triangle-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
	 (texture (font-atlas font))
         (key (list atom-group texture))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list
                                             :texture texture :font font
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-textured-2d-rectangle-list draw-list ub32-oid ub32-color sf-elevation vertices)
    (values)))




