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
  ;; key is (group sf-point-size) for rm and just sf-point-size for im
  (2d-point-list-draw-list-table (make-hash-table :test #'equalp))
  ;; key is (group sf-line-thickness) for rm and just sf-line-thickness for im
  (2d-line-list-draw-list-table (make-hash-table :test #'equalp))
  ;; key is (group texture) for rm and just texture for im
  (2d-triangle-list-draw-list-table (make-hash-table :test #'equalp))
  ;; key is (group texture) for rm and just texture for im
  (2d-triangle-list-draw-list-for-text-table (make-hash-table :test #'equalp))
  ;; key is (group sf-point-size) for rm and sf-point-size for im
  (3d-point-list-draw-list-table (make-hash-table :test #'equalp))
  ;; key is (group sf-line-thickness) for rm and sf-line-thickness for im
  (3d-line-list-draw-list-table (make-hash-table :test #'equalp))
  ;; key is (group texture) for rm and just texture for im
  (3d-triangle-list-draw-list-table (make-hash-table :test #'equalp))
  ;; key is (group texture) for rm and just texture for im
  (3d-triangle-list-with-normals-draw-list-table (make-hash-table :test #'equalp))

  ;; even in immediate mode, strips must use cmds
  (2d-line-strip-draw-list (make-instance '2d-vertex-draw-list))
  (2d-triangle-strip-draw-list (make-instance '2d-vertex-draw-list))
  (3d-line-strip-draw-list (make-instance '3d-vertex-draw-list))
  (3d-triangle-strip-draw-list (make-instance '3d-vertex-draw-list))
  (3d-triangle-strip-with-normals-draw-list (make-instance '3d-vertex-with-normal-draw-list))

  (group-hash-table (make-hash-table :test #'eq))
  (work-queue (sb-concurrency:make-queue :name "draw-data-work-queue"))
  (deletion-queue (sb-concurrency:make-queue :name "draw-data-deletion-queue")))


(defstruct (immediate-mode-draw-data
	     (:include standard-draw-data)
	     (:conc-name "DRAW-DATA-")
             (:constructor make-immediate-mode-draw-data (name))))

(defstruct (retained-mode-draw-data
	     (:include standard-draw-data)
             (:conc-name "DRAW-DATA-")
             (:constructor make-retained-mode-draw-data (name)))

  (2d-point-list-draw-list (make-instance '2d-vertex-draw-list))
  (2d-line-list-draw-list (make-instance '2d-vertex-draw-list))
  (2d-triangle-list-draw-list (make-instance '2d-vertex-draw-list))
  (2d-triangle-list-draw-list-for-text (make-instance '2d-vertex-draw-list))
  (3d-point-list-draw-list (make-instance '3d-vertex-draw-list))
  (3d-line-list-draw-list (make-instance '3d-vertex-draw-list))
  (3d-triangle-list-draw-list (make-instance '3d-vertex-draw-list))
  (3d-triangle-list-with-normals-draw-list (make-instance '3d-vertex-with-normal-draw-list))

  (handle-hash-table (make-hash-table :test #'eq)))


(defun %draw-data-add-2d-point-primitive (draw-data handle ub32-oid atom-group model-mtx sf-point-size ub32-color sf-x sf-y)
  (declare (type retained-mode-draw-data draw-data))
  (declare (type single-float sf-point-size))
  (let* ((draw-list (draw-data-2d-point-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-2d-point draw-list ub32-oid atom-group model-mtx sf-point-size ub32-color sf-x sf-y))
    (values)))


(defun %draw-data-add-2d-point (draw-data ub32-oid atom-group sf-point-size ub32-color sf-x sf-y)
  (declare (type retained-mode-draw-data draw-data))
  (declare (type single-float sf-point-size))
  (let* ((draw-list-table (draw-data-2d-point-list-draw-list-table draw-data))
	 (group-hash-table (draw-data-group-hash-table draw-data))
	 (key (list atom-group sf-point-size))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list
					     :point-size sf-point-size
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-2d-point draw-list ub32-oid ub32-color sf-x sf-y)
    (values)))


(defun %draw-data-draw-2d-point (draw-data ub32-oid atom-group sf-point-size ub32-color sf-x sf-y)
  (declare (type immediate-mode-draw-data draw-data))
  (declare (type single-float sf-point-size))
  (let* ((draw-list-table (draw-data-2d-point-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
         (key (list atom-group sf-point-size))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list
					     :point-size sf-point-size
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-2d-point draw-list ub32-oid ub32-color sf-x sf-y)
    (values)))


(defun %draw-data-add-3d-point-primitive (draw-data handle ub32-oid atom-group model-mtx sf-point-size ub32-color sf-x sf-y sf-z)
  (declare (type retained-mode-draw-data draw-data))
  (declare (type single-float sf-point-size))
  (let* ((draw-list (draw-data-3d-point-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-3d-point draw-list ub32-oid atom-group model-mtx sf-point-size ub32-color sf-x sf-y sf-z))
    (values)))


(defun %draw-data-add-3d-point (draw-data ub32-oid atom-group sf-point-size ub32-color sf-x sf-y sf-z)
  (declare (type retained-mode-draw-data draw-data))
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
    (draw-data handle ub32-oid atom-group model-mtx sf-line-thickness ub32-color sf-x0 sf-y0 sf-x1 sf-y1)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list (draw-data-2d-line-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-2d-line draw-list ub32-oid atom-group model-mtx sf-line-thickness ub32-color sf-x0 sf-y0 sf-x1 sf-y1))
    (values)))


(defun %draw-data-add-2d-line
    (draw-data ub32-oid atom-group sf-line-thickness ub32-color sf-x0 sf-y0 sf-x1 sf-y1)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
	 (group-hash-table (draw-data-group-hash-table draw-data))
	 (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list
					     :line-thickness sf-line-thickness
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-2d-line draw-list ub32-oid ub32-color sf-x0 sf-y0 sf-x1 sf-y1)
    (values)))


(defun %draw-data-draw-2d-line (draw-data ub32-oid atom-group sf-line-thickness ub32-color sf-x0 sf-y0 sf-x1 sf-y1)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
         (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list
					     :line-thickness sf-line-thickness
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-2d-line draw-list ub32-oid ub32-color sf-x0 sf-y0 sf-x1 sf-y1)
    (values)))


(defun %draw-data-add-3d-line-primitive
    (draw-data handle ub32-oid atom-group model-mtx sf-line-thickness ub32-color sf-x0 sf-y0 sf-z0 sf-x1 sf-y1 sf-z1)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list (draw-data-3d-line-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-3d-line draw-list ub32-oid atom-group model-mtx sf-line-thickness ub32-color
				  sf-x0 sf-y0 sf-z0 sf-x1 sf-y1 sf-z1))
    (values)))


(defun %draw-data-add-3d-line
    (draw-data ub32-oid atom-group sf-line-thickness ub32-color sf-x0 sf-y0 sf-z0 sf-x1 sf-y1 sf-z1)
  (declare (type retained-mode-draw-data draw-data))
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
					     atom-group model-mtx sf-line-thickness ub32-color
					     sf-x0 sf-y0 sf-x1 sf-y1 sf-x2 sf-y2)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list (draw-data-2d-line-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
	  (%draw-list-add-2d-polyline draw-list ub32-oid atom-group model-mtx t sf-line-thickness ub32-color
				      (list sf-x0 sf-y0 sf-x1 sf-y1 sf-x2 sf-y2)))
    
    (values)))


(defun %draw-data-add-multicolor-2d-polyline-primitive
    (draw-data handle ub32-oid atom-group model-mtx bool-closed? sf-line-thickness seq-vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-2d-line-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-multicolor-2d-polyline
	   draw-list ub32-oid atom-group model-mtx bool-closed? sf-line-thickness seq-vertices))
    (values)))


(defun %draw-data-add-multicolor-2d-polyline
    (draw-data ub32-oid atom-group bool-closed? sf-line-thickness seq-vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
	 (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list
                                             :line-thickness sf-line-thickness
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-multicolor-2d-polyline draw-list ub32-oid bool-closed? seq-vertices)
    (values)))


(defun %draw-data-draw-multicolor-2d-polyline
    (draw-data ub32-oid atom-group bool-closed? sf-line-thickness seq-vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
         (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list
                                             :line-thickness sf-line-thickness
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-multicolor-2d-polyline draw-list ub32-oid bool-closed? seq-vertices)
    (values)))


(defun %draw-data-add-2d-polyline-primitive
    (draw-data handle ub32-oid atom-group model-mtx closed? sf-line-thickness ub32-color seq-vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-2d-line-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-2d-polyline
           draw-list ub32-oid atom-group model-mtx closed? sf-line-thickness ub32-color seq-vertices))
    (values)))


(defun %draw-data-add-2d-polyline
    (draw-data ub32-oid atom-group closed? sf-line-thickness ub32-color seq-vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
	 (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list
                                             :line-thickness sf-line-thickness
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-2d-polyline draw-list ub32-oid closed? ub32-color seq-vertices)
    (values)))


(defun %draw-data-draw-2d-polyline (draw-data ub32-oid atom-group closed? sf-line-thickness ub32-color seq-vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
         (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list
                                             :line-thickness sf-line-thickness
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-2d-polyline draw-list ub32-oid closed? ub32-color seq-vertices))
  (values))


(defun %draw-data-add-2d-circular-arc-primitive (draw-data handle ub32-oid atom-group model-mtx closed? line-thickness color
						 center-x center-y radius start-angle end-angle
						 number-of-segments)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-2d-line-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-2d-circular-arc
           draw-list ub32-oid atom-group model-mtx
	   closed? line-thickness color
           center-x center-y radius start-angle end-angle
           number-of-segments))
    (values)))


(defun %draw-data-add-2d-circular-arc (draw-data ub32-oid atom-group closed? sf-line-thickness color
				       center-x center-y radius start-angle end-angle
				       number-of-segments)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
	 (group-hash-table (draw-data-group-hash-table draw-data))
	 (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list
					     :line-thickness sf-line-thickness
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-2d-circular-arc draw-list ub32-oid
                                     closed? color
                                     center-x center-y radius start-angle end-angle
                                     number-of-segments))
  (values))


(defun %draw-data-draw-2d-circular-arc (draw-data ub32-oid atom-group closed? sf-line-thickness color
                                        center-x center-y radius start-angle end-angle
                                        number-of-segments)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
         (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list
                                             :line-thickness sf-line-thickness
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-2d-circular-arc draw-list ub32-oid
                                     closed? color
                                     center-x center-y radius start-angle end-angle
                                     number-of-segments)
    (values)))


(defun %draw-data-add-2d-circle-primitive (draw-data handle ub32-oid atom-group model-mtx line-thickness color
					   center-x center-y radius number-of-segments)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-2d-line-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-2d-circle
           draw-list ub32-oid atom-group model-mtx line-thickness color
           center-x center-y radius
           number-of-segments))
    (values)))


(defun %draw-data-add-2d-circle (draw-data ub32-oid atom-group sf-line-thickness ub32-color
				 center-x center-y radius number-of-segments)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
	 (group-hash-table (draw-data-group-hash-table draw-data))
	 (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list
					     :line-thickness sf-line-thickness
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-2d-circle draw-list ub32-oid ub32-color center-x center-y radius number-of-segments)
    (values)))


(defun %draw-data-draw-2d-circle (draw-data ub32-oid atom-group sf-line-thickness ub32-color
				  center-x center-y radius number-of-segments)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
         (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list
                                             :line-thickness sf-line-thickness
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-2d-circle draw-list ub32-oid ub32-color center-x center-y radius number-of-segments)
    (values)))


(defun %draw-data-add-multicolor-3d-polyline-primitive (draw-data handle ub32-oid atom-group model-mtx closed? line-thickness vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-line-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-multicolor-3d-polyline
           draw-list ub32-oid atom-group model-mtx closed? line-thickness vertices))
    (values)))


(defun %draw-data-add-multicolor-3d-polyline (draw-data ub32-oid atom-group closed? sf-line-thickness vertices)
  (declare (type retained-mode-draw-data draw-data))
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


(defun %draw-data-draw-multicolor-3d-polyline (draw-data ub32-oid atom-group closed? sf-line-thickness vertices)
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
    (%draw-list-draw-multicolor-3d-polyline draw-list ub32-oid closed? vertices)
    (values)))


(defun %draw-data-add-3d-polyline-primitive (draw-data handle ub32-oid atom-group model-mtx closed? line-thickness color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-line-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-3d-polyline
           draw-list ub32-oid atom-group
           model-mtx closed? line-thickness color vertices))
    (values)))


(defun %draw-data-add-3d-polyline (draw-data ub32-oid atom-group closed? sf-line-thickness color vertices)
  (declare (type retained-mode-draw-data draw-data))
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


(defun %draw-data-add-filled-2d-triangle-list-primitive (draw-data handle ub32-oid atom-group model-mtx color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list (draw-data-2d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
	  (%draw-list-add-filled-2d-triangle-strip/list draw-list ub32-oid atom-group model-mtx color vertices))
    (values)))


(defun %draw-data-add-filled-2d-triangle-list (draw-data ub32-oid atom-group ub32-color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-triangle-list-draw-list-table draw-data))
	 (group-hash-table (draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list
					     :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-filled-2d-triangle-list draw-list ub32-oid ub32-color vertices)
    (values)))


(defun %draw-data-draw-filled-2d-triangle-list (draw-data ub32-oid atom-group color vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-triangle-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list
                                             :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-filled-2d-triangle-list draw-list ub32-oid color vertices)
    (values)))


(defun %draw-data-add-filled-2d-triangle-strip-primitive (draw-data handle ub32-oid atom-group model-mtx ub32-color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-2d-triangle-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-2d-triangle-strip/list draw-list ub32-oid atom-group model-mtx ub32-color vertices))
    (values)))


(defun %draw-data-add-filled-2d-rectangle-list-primitive (draw-data handle ub32-oid atom-group model-mtx ub32-color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list (draw-data-2d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
	  (%draw-list-add-filled-2d-rectangle-list draw-list ub32-oid atom-group model-mtx ub32-color vertices))
    (values)))


(defun %draw-data-add-filled-2d-rectangle-list (draw-data ub32-oid atom-group ub32-color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-triangle-list-draw-list-table draw-data))
	 (group-hash-table (draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list
					     :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-filled-2d-rectangle-list draw-list ub32-oid ub32-color vertices)
    (values)))


(defun %draw-data-draw-filled-2d-rectangle-list (draw-data ub32-oid atom-group ub32-color vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-triangle-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list
                                             :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-filled-2d-rectangle-list draw-list ub32-oid ub32-color vertices)
    (values)))


(defun %draw-data-add-textured-2d-rectangle-list-primitive (draw-data handle ub32-oid atom-group model-mtx texture ub32-color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-2d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-textured-2d-rectangle-list draw-list ub32-oid atom-group model-mtx texture ub32-color vertices))
    (values)))


(defun %draw-data-add-textured-2d-rectangle-list (draw-data ub32-oid atom-group texture ub32-color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-triangle-list-draw-list-table draw-data))
	 (group-hash-table (draw-data-group-hash-table draw-data))
	 (key (list atom-group texture))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list
					     :texture texture
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-textured-2d-rectangle-list draw-list ub32-oid ub32-color vertices)
    (values)))


(defun %draw-data-draw-textured-2d-rectangle-list (draw-data ub32-oid atom-group texture ub32-color vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-triangle-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
         (key (list atom-group texture))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list
                                             :texture texture
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-textured-2d-rectangle-list draw-list ub32-oid ub32-color vertices)
    (values)))


(defun %draw-data-add-filled-2d-convex-polygon-primitive (draw-data handle ub32-oid atom-group model-mtx ub32-color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-2d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-2d-convex-polygon
           draw-list ub32-oid atom-group
           model-mtx ub32-color vertices))
    (values)))


(defun %draw-data-add-filled-2d-convex-polygon (draw-data ub32-oid atom-group ub32-color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-triangle-list-draw-list-table draw-data))
	 (group-hash-table (draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list
					     :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-filled-2d-convex-polygon draw-list ub32-oid ub32-color vertices)
    (values)))


(defun %draw-data-draw-filled-2d-convex-polygon (draw-data ub32-oid atom-group ub32-color vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-triangle-list-draw-list-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list
                                             :texture *white-texture*
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-filled-2d-convex-polygon draw-list ub32-oid ub32-color vertices)
    (values)))


(defun %draw-data-add-filled-3d-convex-polygon-primitive (draw-data handle ub32-oid atom-group model-mtx ub32-color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-3d-convex-polygon
           draw-list ub32-oid atom-group
           model-mtx ub32-color vertices))
    (values)))


(defun %draw-data-add-filled-3d-convex-polygon (draw-data ub32-oid atom-group ub32-color vertices)
  (declare (type retained-mode-draw-data draw-data))
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
  (let ((draw-list (draw-data-3d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-multicolor-3d-convex-polygon
           draw-list ub32-oid atom-group
           model-mtx vertices))
    (values)))


(defun %draw-data-add-multicolor-3d-convex-polygon (draw-data ub32-oid atom-group vertices)
  (declare (type retained-mode-draw-data draw-data))
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
    (draw-data handle ub32-oid atom-group model-mtx ub32-color vertices light-position)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-triangle-list-with-normals-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-3d-convex-polygon-with-normals
           draw-list ub32-oid atom-group
           model-mtx ub32-color vertices light-position))
    (values)))


(defun %draw-data-add-filled-3d-convex-polygon-with-normals (draw-data ub32-oid atom-group ub32-color vertices)
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
    (draw-data handle ub32-oid atom-group model-mtx vertices light-position)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list (draw-data-3d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-multicolor-3d-convex-polygon-with-normals
           draw-list ub32-oid atom-group model-mtx vertices light-position))
    (values)))


(defun %draw-data-add-multicolor-3d-convex-polygon-with-normals (draw-data ub32-oid atom-group vertices)
  (declare (type retained-mode-draw-data draw-data))
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
  (let ((draw-list (draw-data-3d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-3d-triangle-strip/list draw-list ub32-oid atom-group model-mtx ub32-color vertices))
    (values)))


(defun %draw-data-add-filled-3d-triangle-list (draw-data ub32-oid atom-group ub32-color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-draw-list-table draw-data))
	 (group-hash-table (draw-data-group-hash-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list
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
  (let ((draw-list (draw-data-3d-triangle-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-3d-triangle-strip/list draw-list ub32-oid atom-group model-mtx ub32-color vertices))
    (values)))


;; can't render triangle strips without cmd!!
(defun %draw-data-add-filled-3d-triangle-list-with-normals-primitive
    (draw-data handle ub32-oid atom-group model-mtx ub32-color vertices light-position)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-triangle-list-with-normals-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-3d-triangle-strip/list-with-normals
           draw-list ub32-oid
           atom-group model-mtx ub32-color vertices light-position))
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
    (draw-data handle ub32-oid atom-group model-mtx vertices light-position)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-triangle-list-with-normals-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-multicolor-3d-triangle-strip/list-with-normals
           draw-list ub32-oid atom-group
           model-mtx vertices light-position))
    (values)))


(defun %draw-data-add-multicolor-3d-triangle-list-with-normals
    (draw-data ub32-oid atom-group vertices)
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
    (draw-data handle ub32-oid atom-group model-mtx ub32-color vertices light-position)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-triangle-strip-with-normals-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-3d-triangle-strip/list-with-normals
           draw-list ub32-oid atom-group
           model-mtx ub32-color vertices light-position))
    (values)))

;; can't render triangle strips without cmd!!
(defun %draw-data-add-textured-3d-triangle-list-primitive (draw-data handle ub32-oid atom-group model-mtx texture ub32-color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
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
  (let ((draw-list (draw-data-3d-triangle-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-textured-3d-triangle-strip/list
           draw-list ub32-oid atom-group
           model-mtx texture ub32-color vertices))
    (values)))


(defun %draw-data-add-filled-sphere-primitive
    (draw-data handle ub32-oid atom-group model-mtx ub32-color origin-x origin-y origin-z radius resolution light-position)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-triangle-list-with-normals-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-sphere
	   draw-list ub32-oid atom-group model-mtx ub32-color origin-x origin-y origin-z radius resolution light-position))
    (values)))


(defun %draw-data-add-filled-sphere
    (draw-data ub32-oid atom-group ub32-color origin-x origin-y origin-z radius resolution)
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


(defun %draw-data-add-text-quad-list-primitive (draw-data handle ub32-oid atom-group model-mtx font ub32-color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-2d-triangle-list-draw-list-for-text draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-textured-2d-rectangle-list
           draw-list ub32-oid atom-group model-mtx (font-atlas font) ub32-color vertices
           #'(lambda (&rest args)
               (apply #'make-text-draw-indexed-cmd
                      font args))))
    (values)))


(defun %draw-data-add-text-quad-list (draw-data ub32-oid atom-group font ub32-color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-triangle-list-draw-list-for-text-table draw-data))
	 (group-hash-table (draw-data-group-hash-table draw-data))
	 (texture (font-atlas font))
	 (key (list atom-group texture))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list
					     :texture texture :font font
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-textured-2d-rectangle-list draw-list ub32-oid ub32-color vertices)
    (values)))


(defun %draw-data-draw-text-quad-list (draw-data ub32-oid atom-group font ub32-color vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-triangle-list-draw-list-for-text-table draw-data))
         (group-hash-table (draw-data-group-hash-table draw-data))
	 (texture (font-atlas font))
         (key (list atom-group texture))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list
                                             :texture texture :font font
					     :group (or (gethash atom-group group-hash-table)
							(setf (gethash atom-group group-hash-table)
							      (make-group atom-group))))))))
    (%draw-list-draw-textured-2d-rectangle-list draw-list ub32-oid ub32-color vertices)
    (values)))
