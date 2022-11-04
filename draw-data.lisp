(in-package :krma)

(defstruct (standard-draw-data
	    (:conc-name "DRAW-DATA-")
            (:constructor make-standard-draw-data (name)))
  (name)
  ;; key is (group sf-point-size) for rm and just sf-point-size for im
  (2d-point-list-draw-list-table)
  ;; key is (group sf-line-thickness) for rm and just sf-line-thickness for im
  (2d-line-list-draw-list-table) 
  ;; key is (group texture) for rm and just texture for im
  (2d-triangle-list-draw-list-table)
  ;; key is (group texture) for rm and just texture for im
  (2d-triangle-list-draw-list-for-text-table)
  ;; key is (group sf-point-size) for rm and sf-point-size for im
  (3d-point-list-draw-list-table)
  ;; key is (group sf-line-thickness) for rm and sf-line-thickness for im
  (3d-line-list-draw-list-table)
  ;; key is (group texture) for rm and just texture for im
  (3d-triangle-list-draw-list-table)
  ;; key is (group texture) for rm and just texture for im
  (3d-triangle-list-with-normals-draw-list-table)
  ;; key is (group texture) for rm and just texture for im
  (3d-triangle-list-draw-list-with-normals-table)

  ;; even in immediate mode, strips must use cmds
  (2d-line-strip-draw-list (make-instance '2d-vertex-draw-list))
  (2d-triangle-strip-draw-list (make-instance '2d-vertex-draw-list))
  (3d-line-strip-draw-list (make-instance '3d-vertex-draw-list))
  (3d-triangle-strip-draw-list (make-instance '3d-vertex-draw-list))
  (3d-triangle-strip-with-normals-draw-list (make-instance '3d-vertex-with-normal-draw-list)))
  
  
(defstruct (immediate-mode-draw-data
	    (:include standard-draw-data)
	    (:conc-name "DRAW-DATA-")
	    (:constructor %make-immediate-mode-draw-data
		(name &optional
			(2d-point-list-draw-list-table (make-hash-table :test #'eq))
			(2d-line-list-draw-list-table (make-hash-table :test #'eq))
			(2d-triangle-list-draw-list-table (make-hash-table :test #'eq))
			(2d-triangle-list-draw-list-for-text-table (make-hash-table :test #'eq))
			(3d-point-list-draw-list-table (make-hash-table :test #'eq))
			(3d-line-list-draw-list-table (make-hash-table :test #'eq))
			(3d-triangle-list-draw-list-table (make-hash-table :test #'eq))
			(3d-triangle-list-with-normals-draw-list-table (make-hash-table :test #'eq))))))

(declaim (inline make-immediate-mode-draw-data))
(defun make-immediate-mode-draw-data (name)
  (%make-immediate-mode-draw-data name))

(defstruct (retained-mode-draw-data
	    (:include standard-draw-data)
            (:conc-name "DRAW-DATA-")
            (:constructor %make-retained-mode-draw-data
		(name &optional
			(2d-point-list-draw-list-table (make-hash-table :test #'equalp))
			(2d-line-list-draw-list-table (make-hash-table :test #'equalp))
			(2d-triangle-list-draw-list-table (make-hash-table :test #'equalp))
			(2d-triangle-list-draw-list-for-text-table (make-hash-table :test #'equalp))
			(3d-point-list-draw-list-table (make-hash-table :test #'equalp))
			(3d-line-list-draw-list-table (make-hash-table :test #'equalp))
			(3d-triangle-list-draw-list-table (make-hash-table :test #'equalp))
			(3d-triangle-list-with-normals-draw-list-table (make-hash-table :test #'equalp)))))
  (2d-point-list-draw-list (make-instance '2d-vertex-draw-list))
  (2d-line-list-draw-list (make-instance '2d-vertex-draw-list))
  (2d-triangle-list-draw-list (make-instance '2d-vertex-draw-list))
  (2d-triangle-list-draw-list-for-text (make-instance '2d-vertex-draw-list))
  (3d-point-list-draw-list (make-instance '3d-vertex-draw-list))
  (3d-line-list-draw-list (make-instance '3d-vertex-draw-list))
  (3d-triangle-list-draw-list (make-instance '3d-vertex-draw-list))
  (3d-triangle-list-with-normals-draw-list (make-instance '3d-vertex-with-normal-draw-list))

  (handle-hash-table (make-hash-table))
  (work-queue (sb-concurrency:make-queue :name "draw-data-work-queue")))
   

(declaim (inline make-retained-mode-draw-data))
(defun make-retained-mode-draw-data (name)
  (%make-retained-mode-draw-data name))		

(defun %draw-data-add-2d-point-primitive (draw-data handle model-mtx sf-point-size ub32-color sf-x sf-y)
  (declare (type retained-mode-draw-data draw-data))
  (declare (type single-float sf-point-size))
  (let* ((draw-list (draw-data-2d-point-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-2d-point-cmd draw-list model-mtx sf-point-size ub32-color sf-x sf-y))
    (values)))

(defun %draw-data-add-2d-point-pseudo (draw-data handle atom-group sf-point-size ub32-color sf-x sf-y)
  (declare (type retained-mode-draw-data draw-data))
  (declare (type single-float sf-point-size))
  (let* ((draw-list-table (draw-data-2d-point-list-draw-list-table draw-data))
	 (key (list atom-group sf-point-size))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list :point-size sf-point-size)))))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-2d-point-pseudo-cmd draw-list ub32-color sf-x sf-y))
    (values)))

(defun %draw-data-draw-2d-point (draw-data sf-point-size ub32-color sf-x sf-y)
  (declare (type immediate-mode-draw-data draw-data))
  (declare (type single-float sf-point-size))
  (let* ((draw-list-table (draw-data-2d-point-list-draw-list-table draw-data))
	 (draw-list (or (gethash sf-point-size draw-list-table)
			(setf (gethash sf-point-size draw-list-table)
			      (make-instance '2d-vertex-draw-list :point-size sf-point-size)))))
    (%draw-list-add-2d-point-pseudo-cmd draw-list ub32-color sf-x sf-y)
    (values)))

(defun %draw-data-add-3d-point-primitive (draw-data handle model-mtx sf-point-size ub32-color sf-x sf-y sf-z)
  (declare (type retained-mode-draw-data draw-data))
  (declare (type single-float sf-point-size))
  (let* ((draw-list (draw-data-3d-point-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-3d-point-cmd draw-list model-mtx sf-point-size ub32-color sf-x sf-y sf-z))
    (values)))

(defun %draw-data-add-3d-point-pseudo (draw-data handle atom-group sf-point-size ub32-color sf-x sf-y sf-z)
  (declare (type retained-mode-draw-data draw-data))
  (declare (type single-float sf-point-size))
  (let* ((draw-list-table (draw-data-3d-point-list-draw-list-table draw-data))
	 (key (list atom-group sf-point-size))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list :point-size sf-point-size)))))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
	  (%draw-list-add-3d-point-pseudo-cmd draw-list ub32-color sf-x sf-y sf-z))
    (values)))

(defun %draw-data-draw-3d-point (draw-data sf-point-size ub32-color sf-x sf-y sf-z)
  (declare (type immediate-mode-draw-data draw-data))
  (declare (type single-float sf-point-size))
  (let* ((draw-list-table (draw-data-3d-point-list-draw-list-table draw-data))
	 (draw-list (or (gethash sf-point-size draw-list-table)
			(setf (gethash sf-point-size draw-list-table)
			      (make-instance '3d-vertex-draw-list :point-size sf-point-size)))))
    (%draw-list-add-3d-point-pseudo-cmd draw-list ub32-color sf-x sf-y sf-z)
    (values)))

(defun %draw-data-add-2d-line-primitive
    (draw-data handle model-mtx sf-line-thickness ub32-color sf-x0 sf-y0 sf-x1 sf-y1)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list (draw-data-2d-line-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-2d-line-cmd draw-list model-mtx sf-line-thickness ub32-color sf-x0 sf-y0 sf-x1 sf-y1))
    (values)))

(defun %draw-data-add-2d-line-pseudo
    (draw-data handle atom-group sf-line-thickness ub32-color sf-x0 sf-y0 sf-x1 sf-y1)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
	 (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list :line-thickness sf-line-thickness)))))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-2d-line-pseudo-cmd draw-list ub32-color sf-x0 sf-y0 sf-x1 sf-y1))
    (values)))

(defun %draw-data-draw-2d-line (draw-data sf-line-thickness ub32-color sf-x0 sf-y0 sf-x1 sf-y1)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
	 (draw-list (or (gethash sf-line-thickness draw-list-table)
			(setf (gethash sf-line-thickness draw-list-table)
			      (make-instance '2d-vertex-draw-list :line-thickness sf-line-thickness)))))
    (%draw-list-add-2d-line-pseudo-cmd draw-list ub32-color sf-x0 sf-y0 sf-x1 sf-y1)
    (values)))

(defun %draw-data-add-3d-line-primitive
    (draw-data handle model-mtx sf-line-thickness ub32-color sf-x0 sf-y0 sf-z0 sf-x1 sf-y1 sf-z1)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list (draw-data-3d-line-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-3d-line-cmd draw-list model-mtx sf-line-thickness ub32-color
				       sf-x0 sf-y0 sf-z0 sf-x1 sf-y1 sf-z1))
    (values)))

(defun %draw-data-add-3d-line-pseudo
    (draw-data handle atom-group sf-line-thickness ub32-color sf-x0 sf-y0 sf-z0 sf-x1 sf-y1 sf-z1)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-line-list-draw-list-table draw-data))
	 (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list :line-thickness sf-line-thickness)))))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
	  (%draw-list-add-3d-line-pseudo-cmd draw-list ub32-color sf-x0 sf-y0 sf-z0 sf-x1 sf-y1 sf-z1))
    (values)))

(defun %draw-data-draw-3d-line (draw-data sf-line-thickness ub32-color sf-x0 sf-y0 sf-z0 sf-x1 sf-y1 sf-z1)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-line-list-draw-list-table draw-data))
	 (draw-list (or (gethash sf-line-thickness draw-list-table)
			(setf (gethash sf-line-thickness draw-list-table)
			      (make-instance '3d-vertex-draw-list :line-thickness sf-line-thickness)))))
    (%draw-list-add-3d-line-pseudo-cmd draw-list ub32-color sf-x0 sf-y0 sf-z0 sf-x1 sf-y1 sf-z1)
    (values)))

(defun %draw-data-add-2d-triangle-primitive (draw-data handle model-mtx sf-line-thickness ub32-color
					     sf-x0 sf-y0 sf-x1 sf-y1 sf-x2 sf-y2)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list (draw-data-2d-line-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
	  (%draw-list-add-2d-polyline-cmd draw-list t model-mtx sf-line-thickness ub32-color 
					  (list sf-x0 sf-y0 sf-x1 sf-y1 sf-x2 sf-y2)))
    
    (values)))

(defun %draw-data-add-2d-triangle-pseudo (draw-data handle atom-group sf-line-thickness ub32-color
					  sf-x0 sf-y0 sf-x1 sf-y1 sf-x2 sf-y2)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
	 (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list :line-thickness sf-line-thickness)))))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
	  (%draw-list-add-2d-polyline-pseudo-cmd draw-list t ub32-color (list sf-x0 sf-y0 sf-x1 sf-y1 sf-x2 sf-y2)))
    (values)))

(defun %draw-data-draw-2d-triangle (draw-data sf-line-thickness ub32-color sf-x0 sf-y0 sf-x1 sf-y1 sf-x2 sf-y2)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
	 (draw-list (or (gethash sf-line-thickness draw-list-table)
			(setf (gethash sf-line-thickness draw-list-table)
			      (make-instance '2d-vertex-draw-list :line-thickness sf-line-thickness)))))
    (%draw-list-add-2d-polyline-pseudo-cmd draw-list t ub32-color (list sf-x0 sf-y0 sf-x1 sf-y1 sf-x2 sf-y2))
    (values)))

(defun %draw-data-add-2d-rectangle-primitive (draw-data handle model-mtx line-thickness color x0 y0 x1 y1)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-2d-line-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
	  (%draw-list-add-2d-rectangle-cmd draw-list model-mtx line-thickness color x0 y0 x1 y1))
    (values)))

(defun %draw-data-add-2d-rectangle-pseudo (draw-data handle atom-group sf-line-thickness color x0 y0 x1 y1)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
	 (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list :line-thickness sf-line-thickness)))))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
	  (%draw-list-add-2d-rectangle-pseudo-cmd draw-list color x0 y0 x1 y1))
    (values)))

(defun %draw-data-draw-2d-rectangle (draw-data sf-line-thickness color x0 y0 x1 y1)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
	 (draw-list (or (gethash sf-line-thickness draw-list-table)
			(setf (gethash sf-line-thickness draw-list-table)
			      (make-instance '2d-vertex-draw-list :line-thickness sf-line-thickness)))))
    (%draw-list-add-2d-rectangle-pseudo-cmd draw-list color x0 y0 x1 y1)
    (values)))

(defun %draw-data-add-multicolor-2d-polyline-primitive
    (draw-data handle model-mtx bool-closed? sf-line-thickness seq-vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-2d-line-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-multicolor-2d-polyline-cmd
	   draw-list model-mtx bool-closed? sf-line-thickness seq-vertices))
    (values)))

(defun %draw-data-add-multicolor-2d-polyline-pseudo
    (draw-data handle atom-group bool-closed? sf-line-thickness seq-vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
	 (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list :line-thickness sf-line-thickness)))))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-multicolor-2d-polyline-pseudo-cmd draw-list bool-closed? seq-vertices))
    (values)))

(defun %draw-data-draw-multicolor-2d-polyline
    (draw-data bool-closed? sf-line-thickness seq-vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
	 (draw-list (or (gethash sf-line-thickness draw-list-table)
			(setf (gethash sf-line-thickness draw-list-table)
			      (make-instance '2d-vertex-draw-list :line-thickness sf-line-thickness)))))
    (%draw-list-add-multicolor-2d-polyline-pseudo-cmd draw-list bool-closed? seq-vertices)
    (values)))

(defun %draw-data-add-2d-polyline-primitive
    (draw-data handle model-mtx closed? sf-line-thickness ub32-color seq-vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-2d-line-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-2d-polyline-cmd
           draw-list model-mtx closed? sf-line-thickness ub32-color seq-vertices))
    (values)))

(defun %draw-data-add-2d-polyline-pseudo
    (draw-data handle closed? atom-group sf-line-thickness ub32-color seq-vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
	 (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list :line-thickness sf-line-thickness)))))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-2d-polyline-pseudo-cmd draw-list closed? ub32-color seq-vertices))
    (values)))

(defun %draw-data-draw-2d-polyline (draw-data closed? sf-line-thickness ub32-color seq-vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
	 (draw-list (or (gethash sf-line-thickness draw-list-table)
			(setf (gethash sf-line-thickness draw-list-table)
			      (make-instance '2d-vertex-draw-list :line-thickness sf-line-thickness)))))
    (%draw-list-add-2d-polyline-pseudo-cmd draw-list closed? ub32-color seq-vertices))
    (values))

(defun %draw-data-add-2d-circular-arc-primitive (draw-data handle model-mtx closed? line-thickness color
						 center-x center-y radius start-angle end-angle
						 number-of-segments)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-2d-line-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-2d-circular-arc-cmd
           draw-list
           model-mtx closed? line-thickness color
           center-x center-y radius start-angle end-angle
           number-of-segments))
    (values)))

(defun %draw-data-add-2d-circular-arc-pseudo (draw-data handle atom-group closed? sf-line-thickness color
							center-x center-y radius start-angle end-angle
							number-of-segments)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
	 (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list :line-thickness sf-line-thickness)))))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-2d-circular-arc-pseudo-cmd
           draw-list
           closed? color
           center-x center-y radius start-angle end-angle
           number-of-segments))
    (values)))

(defun %draw-data-draw-2d-circular-arc (draw-data closed? sf-line-thickness color
					center-x center-y radius start-angle end-angle
					number-of-segments)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
	 (draw-list (or (gethash sf-line-thickness draw-list-table)
			(setf (gethash sf-line-thickness draw-list-table)
			      (make-instance '2d-vertex-draw-list :line-thickness sf-line-thickness)))))
    (%draw-list-add-2d-circular-arc-pseudo-cmd
     draw-list
     closed? color
     center-x center-y radius start-angle end-angle
     number-of-segments)
    (values)))

(defun %draw-data-add-2d-circle-primitive (draw-data handle model-mtx line-thickness color
					   center-x center-y radius number-of-segments)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-2d-line-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-2d-circle-cmd
           draw-list
           model-mtx line-thickness color
           center-x center-y radius
           number-of-segments))
    (values)))

(defun %draw-data-add-2d-circle-pseudo (draw-data handle atom-group sf-line-thickness color
					center-x center-y radius number-of-segments)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
	 (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list :line-thickness sf-line-thickness)))))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-2d-circle-pseudo-cmd draw-list color center-x center-y radius number-of-segments))
    (values)))

(defun %draw-data-draw-2d-circle (draw-data sf-line-thickness color
				  center-x center-y radius number-of-segments)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
	 (draw-list (or (gethash sf-line-thickness draw-list-table)
			(setf (gethash sf-line-thickness draw-list-table)
			      (make-instance '2d-vertex-draw-list :line-thickness sf-line-thickness)))))
    (%draw-list-add-2d-circle-pseudo-cmd draw-list color center-x center-y radius number-of-segments)
    (values)))

(defun %draw-data-add-multicolor-3d-polyline-primitive (draw-data handle model-mtx closed? line-thickness vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-line-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-multicolor-3d-polyline-cmd
           draw-list model-mtx closed? line-thickness vertices))
    (values)))

(defun %draw-data-add-multicolor-3d-polyline-pseudo (draw-data handle atom-group closed? sf-line-thickness vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
	 (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list :line-thickness sf-line-thickness)))))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-multicolor-3d-polyline-pseudo-cmd draw-list closed? vertices))
    (values)))

(defun %draw-data-draw-multicolor-3d-polyline (draw-data closed? sf-line-thickness vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
	 (draw-list (or (gethash sf-line-thickness draw-list-table)
			(setf (gethash sf-line-thickness draw-list-table)
			      (make-instance '2d-vertex-draw-list :line-thickness sf-line-thickness)))))
    (%draw-list-add-multicolor-3d-polyline-pseudo-cmd draw-list closed? vertices)
    (values)))

(defun %draw-data-add-3d-polyline-primitive (draw-data handle model-mtx closed? line-thickness color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-line-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-3d-polyline-cmd
           draw-list
           model-mtx closed? line-thickness color vertices))
    (values)))

(defun %draw-data-add-3d-polyline-pseudo (draw-data handle atom-group closed? sf-line-thickness color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
	 (key (list atom-group sf-line-thickness))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list :line-thickness sf-line-thickness)))))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-3d-polyline-pseudo-cmd draw-list closed? color vertices))
    (values)))

(defun %draw-data-draw-3d-polyline (draw-data closed? sf-line-thickness color vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-line-list-draw-list-table draw-data))
	 (draw-list (or (gethash sf-line-thickness draw-list-table)
			(setf (gethash sf-line-thickness draw-list-table)
			      (make-instance '2d-vertex-draw-list :line-thickness sf-line-thickness)))))
    (%draw-list-add-3d-polyline-pseudo-cmd draw-list closed? color vertices)
    (values)))

(defun %draw-data-add-filled-2d-triangle-list-primitive (draw-data handle model-mtx color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list (draw-data-2d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
	  (%draw-list-add-filled-2d-triangle-list-cmd draw-list model-mtx color vertices))
    (values)))

(defun %draw-data-add-filled-2d-triangle-list-pseudo (draw-data handle atom-group color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-triangle-list-draw-list-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list :texture *white-texture*)))))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
	  (%draw-list-add-filled-2d-triangle-list-pseudo-cmd draw-list color vertices))
    (values)))

(defun %draw-data-draw-filled-2d-triangle-list (draw-data color vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-triangle-list-draw-list-table draw-data))
	 (key *white-texture*)
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list :texture *white-texture*)))))
    (%draw-list-add-filled-2d-triangle-list-pseudo-cmd draw-list color vertices)
    (values)))

(defun %draw-data-add-filled-2d-triangle-strip-primitive (draw-data handle model-mtx color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-2d-triangle-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-2d-triangle-strip/list-cmd draw-list model-mtx color vertices))
    (values)))

(defun %draw-data-add-filled-2d-rectangle-list-primitive (draw-data handle model-mtx color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list (draw-data-2d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
	  (%draw-list-add-filled-2d-rectangle-list-cmd draw-list model-mtx color vertices))
    (values)))

(defun %draw-data-add-filled-2d-rectangle-list-pseudo (draw-data handle atom-group color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-triangle-list-draw-list-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list :texture *white-texture*)))))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
	  (%draw-list-add-filled-2d-rectangle-list-pseudo-cmd draw-list color vertices))
    (values)))

(defun %draw-data-draw-filled-2d-rectangle-list (draw-data color vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-triangle-list-draw-list-table draw-data))
	 (key *white-texture*)
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list :texture *white-texture*)))))
    (%draw-list-add-filled-2d-rectangle-list-pseudo-cmd draw-list color vertices)
    (values)))

(defun %draw-data-add-textured-2d-rectangle-list-primitive (draw-data handle model-mtx texture color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-2d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-textured-2d-rectangle-list-cmd draw-list model-mtx texture color vertices))
    (values)))

(defun %draw-data-add-textured-2d-rectangle-list-pseudo (draw-data handle atom-group texture color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-triangle-list-draw-list-table draw-data))
	 (key (list atom-group texture))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list :texture texture)))))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-textured-2d-rectangle-list-pseudo-cmd draw-list color vertices))
    (values)))

(defun %draw-data-draw-textured-2d-rectangle-list (draw-data texture color vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-triangle-list-draw-list-table draw-data))
	 (draw-list (or (gethash texture draw-list-table)
			(setf (gethash texture draw-list-table)
			      (make-instance '2d-vertex-draw-list :texture texture)))))
    (%draw-list-add-textured-2d-rectangle-list-pseudo-cmd draw-list color vertices)
    (values)))

(defun %draw-data-add-filled-2d-convex-polygon-primitive (draw-data handle model-mtx color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-2d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-2d-convex-polygon-cmd
           draw-list
           model-mtx color vertices))
    (values)))

(defun %draw-data-add-filled-2d-convex-polygon-pseudo (draw-data handle atom-group color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-triangle-list-draw-list-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list :texture *white-texture*)))))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-2d-convex-polygon-pseudo-cmd draw-list color vertices))
    (values)))

(defun %draw-data-draw-filled-2d-convex-polygon (draw-data color vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-triangle-list-draw-list-table draw-data))
	 (key *white-texture*)
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list :texture *white-texture*)))))
    (%draw-list-add-filled-2d-convex-polygon-pseudo-cmd draw-list color vertices)
    (values)))

(defun %draw-data-add-filled-3d-convex-polygon-primitive (draw-data handle model-mtx color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-3d-convex-polygon-cmd
           draw-list
           model-mtx color vertices))
    (values)))

(defun %draw-data-add-filled-3d-convex-polygon-pseudo (draw-data handle atom-group color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-draw-list-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list :texture *white-texture*)))))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-3d-convex-polygon-pseudo-cmd draw-list color vertices))
    (values)))

(defun %draw-data-draw-filled-3d-convex-polygon (draw-data color vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-draw-list-table draw-data))
	 (key *white-texture*)
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list :texture *white-texture*)))))
    (%draw-list-add-filled-3d-convex-polygon-pseudo-cmd draw-list color vertices)
    (values)))

(defun %draw-data-add-multicolor-3d-convex-polygon-primitive (draw-data handle model-mtx vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-multicolor-3d-convex-polygon-cmd
           draw-list handle
           model-mtx vertices))
    (values)))

(defun %draw-data-add-multicolor-3d-convex-polygon-pseudo (draw-data handle atom-group vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-draw-list-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list :texture *white-texture*)))))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-multicolor-3d-convex-polygon-pseudo-cmd draw-list vertices))
    (values)))

(defun %draw-data-draw-multicolor-3d-convex-polygon (draw-data vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-draw-list-table draw-data))
	 (key *white-texture*)
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list :texture *white-texture*)))))
    (%draw-list-add-multicolor-3d-convex-polygon-pseudo-cmd draw-list vertices)
    (values)))

(defun %draw-data-add-filled-3d-convex-polygon-with-normals-primitive
    (draw-data handle model-mtx ub32-color vertices light-position)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-triangle-list-with-normals-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-3d-convex-polygon-with-normals-cmd
           draw-list
           model-mtx ub32-color vertices light-position))
    (values)))

(defun %draw-data-add-filled-3d-convex-polygon-with-normals-pseudo (draw-data atom-group ub32-color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-draw-list-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-with-normal-draw-list :texture *white-texture*)))))
    (%draw-list-add-filled-3d-convex-polygon-with-normals-pseudo-cmd draw-list ub32-color vertices)
    (values)))

(defun %draw-data-draw-filled-3d-convex-polygon-with-normals (draw-data ub32-color vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-draw-list-table draw-data))
	 (key *white-texture*)
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list :texture *white-texture*)))))
    (%draw-list-add-filled-3d-convex-polygon-with-normals-pseudo-cmd draw-list ub32-color vertices)
    (values)))

(defun %draw-data-add-multicolor-3d-convex-polygon-with-normals-primitive
    (draw-data handle model-mtx vertices light-position)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list (draw-data-3d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-multicolor-3d-convex-polygon-with-normals-cmd
           draw-list model-mtx vertices light-position))
    (values)))

(defun %draw-data-add-multicolor-3d-convex-polygon-with-normals-pseudo (draw-data atom-group vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-draw-list-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-with-normal-draw-list :texture *white-texture*)))))
    (%draw-list-add-multicolor-3d-convex-polygon-with-normals-pseudo-cmd draw-list vertices)
    (values)))

(defun %draw-data-draw-multicolor-3d-convex-polygon-with-normals (draw-data vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-draw-list-table draw-data))
	 (key *white-texture*)
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list :texture *white-texture*)))))
    (%draw-list-add-multicolor-3d-convex-polygon-with-normals-pseudo-cmd draw-list vertices)
    (values)))

(defun %draw-data-add-filled-3d-triangle-list-primitive (draw-data handle model-mtx color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-3d-triangle-strip/list-cmd draw-list model-mtx color vertices))
    (values)))

(defun %draw-data-add-filled-3d-triangle-list-pseudo (draw-data handle atom-group color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-triangle-list-draw-list-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list :texture *white-texture*)))))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-3d-triangle-strip/list-pseudo-cmd draw-list color vertices))
    (values)))

(defun %draw-data-draw-filled-3d-triangle-list (draw-data color vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-draw-list-table draw-data))
	 (key *white-texture*)
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list :texture *white-texture*)))))
    (%draw-list-add-filled-3d-triangle-strip/list-pseudo-cmd draw-list color vertices)
    (values)))

(defun %draw-data-add-filled-3d-triangle-strip-primitive (draw-data handle model-mtx color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-triangle-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-3d-triangle-strip/list-cmd draw-list model-mtx color vertices))
    (values)))

;; revisit
;; can't render triangle strips without cmd!!

(defun %draw-data-add-filled-3d-triangle-list-with-normals-primitive
    (draw-data handle model-mtx color vertices light-position)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-triangle-list-with-normals-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-3d-triangle-strip/list-with-normals-cmd
           draw-list
           model-mtx color vertices light-position))
    (values)))

(defun %draw-data-add-filled-3d-triangle-list-with-normals-pseudo
    (draw-data handle atom-group color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-with-normals-draw-list-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list :texture *white-texture*)))))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-3d-triangle-strip/list-with-normals-pseudo-cmd
           draw-list color vertices))
    (values)))

(defun %draw-data-draw-filled-3d-triangle-list-with-normals
    (draw-data color vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-with-normals-draw-list-table draw-data))
	 (key *white-texture*)
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-with-normal-draw-list :texture *white-texture*)))))
    (%draw-list-add-filled-3d-triangle-strip/list-with-normals-pseudo-cmd draw-list color vertices))
  (values))

(defun %draw-data-add-multicolor-3d-triangle-list-with-normals-primitive
    (draw-data handle model-mtx vertices light-position)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-triangle-list-with-normals-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-multicolor-3d-triangle-strip/list-with-normals-cmd
           draw-list
           model-mtx vertices light-position))
    (values)))

(defun %draw-data-add-multicolor-3d-triangle-list-with-normals-pseudo
    (draw-data handle atom-group vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-with-normals-draw-list-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list :texture *white-texture*)))))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-multicolor-3d-triangle-strip/list-with-normals-pseudo-cmd
           draw-list vertices))
    (values)))

(defun %draw-data-draw-multicolor-3d-triangle-list-with-normals
    (draw-data vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-with-normals-draw-list-table draw-data))
	 (key *white-texture*)
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-with-normal-draw-list :texture *white-texture*)))))
    (%draw-list-add-multicolor-3d-triangle-strip/list-with-normals-pseudo-cmd draw-list vertices))
  (values))

(defun %draw-data-add-filled-3d-triangle-strip-with-normals-primitive
    (draw-data handle model-mtx color vertices light-position)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-triangle-strip-with-normals-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-3d-triangle-strip/list-with-normals-cmd
           draw-list
           model-mtx color vertices light-position))
    (values)))

;; revisit
;; can't render triangle strips without cmd!!

(defun %draw-data-add-textured-3d-triangle-list-primitive (draw-data handle model-mtx texture color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-textured-3d-triangle-strip/list-cmd
           draw-list
           model-mtx texture color vertices))
    (values)))

(defun %draw-data-add-textured-3d-triangle-list-pseudo (draw-data handle atom-group texture color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-draw-list-table draw-data))
	 (key (list atom-group texture))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-draw-list :texture texture)))))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-textured-3d-triangle-strip/list-pseudo-cmd
           draw-list color vertices))
    (values)))

(defun %draw-data-draw-textured-3d-triangle-list (draw-data texture color vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-draw-list-table draw-data))
	 (draw-list (or (gethash texture draw-list-table)
			(setf (gethash texture draw-list-table)
			      (make-instance '3d-vertex-draw-list :texture texture)))))
    (%draw-list-add-textured-3d-triangle-strip/list-pseudo-cmd draw-list color vertices)
    (values)))

(defun %draw-data-add-textured-3d-triangle-strip-primitive (draw-data handle model-mtx texture color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-triangle-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-textured-3d-triangle-strip/list-cmd
           draw-list
           model-mtx texture color vertices))
    (values)))
;; revisit
;; can't render triangle strips without cmd!!

(defun %draw-data-add-sphere-primitive
    (draw-data handle model-mtx color origin-x origin-y origin-z radius resolution light-position)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-triangle-list-with-normals-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-sphere-cmd
	   draw-list model-mtx color origin-x origin-y origin-z radius resolution light-position))
    (values)))

(defun %draw-data-add-sphere-pseudo
    (draw-data handle atom-group color origin-x origin-y origin-z radius resolution)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-draw-list-table draw-data))
	 (key (list atom-group *white-texture*))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '3d-vertex-with-normal-draw-list)))))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-sphere-pseudo-cmd
	   draw-list color origin-x origin-y origin-z radius resolution))
    (values)))

(defun %draw-data-draw-sphere (draw-data color origin-x origin-y origin-z radius resolution)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-3d-triangle-list-draw-list-table draw-data))
	 (draw-list (or (gethash *white-texture* draw-list-table)
			(setf (gethash *white-texture* draw-list-table)
			      (make-instance '3d-vertex-with-normal-draw-list :texture *white-texture*)))))
    (%draw-list-add-filled-sphere-pseudo-cmd
     draw-list color origin-x origin-y origin-z radius resolution)
    (values)))

(defun %draw-data-add-text-quad-list-primitive (draw-data handle model-mtx font color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-2d-triangle-list-draw-list-for-text draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-textured-2d-rectangle-list-cmd
           draw-list model-mtx (font-atlas font) color vertices
           #'(lambda (&rest args)
               (apply #'make-text-draw-indexed-cmd
                      font args))))
    (values)))

(defun %draw-data-add-text-quad-list-pseudo (draw-data handle atom-group font color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-triangle-list-draw-list-for-text-table draw-data))
	 (texture (font-atlas font))
	 (key (list atom-group texture))
	 (draw-list (or (gethash key draw-list-table)
			(setf (gethash key draw-list-table)
			      (make-instance '2d-vertex-draw-list :texture texture :font font)))))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-textured-2d-rectangle-list-pseudo-cmd draw-list color vertices))
    (values)))

(defun %draw-data-draw-text-quad-list (draw-data font color vertices)
  (declare (type immediate-mode-draw-data draw-data))
  (let* ((draw-list-table (draw-data-2d-triangle-list-draw-list-for-text-table draw-data))
	 (texture (font-atlas font))
	 (draw-list (or (gethash texture draw-list-table)
			(setf (gethash texture draw-list-table)
			      (make-instance '2d-vertex-draw-list :texture texture :font font)))))
    (%draw-list-add-textured-2d-rectangle-list-pseudo-cmd draw-list color vertices)
    (values)))
