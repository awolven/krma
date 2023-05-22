(in-package :krma)

(eval-when (:compile-toplevel :load-toplevel)
  (when *muffle-compilation-notes*
    #+sbcl(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))))

#+NIL
(eval-when (:compile-toplevel :load-toplevel)
  (when krma::*debug*
    (declaim (optimize (safety 3) (debug 3))))
  (unless krma::*debug*
    (declaim (optimize (speed 3) (safety 0) (debug 0)))
    (declaim (inline %draw-list-draw-2d-point))
    (declaim (inline %draw-list-add-2d-point))
    (declaim (inline %draw-list-draw-3d-point))
    (declaim (inline %draw-list-add-3d-point))
    (declaim (inline %draw-list-draw-2d-line))
    (declaim (inline %draw-list-add-2d-line))
    (declaim (inline %draw-list-draw-3d-line))
    (declaim (inline %draw-list-add-3d-line))
    (declaim (inline %draw-list-draw-2d-polyline))
    (declaim (inline %draw-list-draw-2d-rectangle))
    (declaim (inline %draw-list-add-2d-polyline))
    (declaim (inline %draw-list-add-2d-rectangle))
    (declaim (inline %draw-list-draw-multicolor-2d-polyline))
    (declaim (inline %draw-list-add-multicolor-2d-polyline))
    (declaim (inline %draw-list-draw-2d-line-list))
    (declaim (inline %draw-list-add-2d-line-list))
    (declaim (inline %draw-list-draw-2d-circular-arc))
    (declaim (inline %draw-list-add-2d-circular-arc))
    (declaim (inline %draw-list-draw-2d-circle))
    (declaim (inline %draw-list-add-2d-circle))
    (declaim (inline %draw-list-draw-3d-polyline))
    (declaim (inline %draw-list-add-3d-polyline))
    (declaim (inline %draw-list-draw-multicolor-3d-polyline))
    (declaim (inline %draw-list-add-multicolor-3d-polyline))
    (declaim (inline %draw-list-draw-filled-2d-triangle-list))
    (declaim (inline %draw-list-add-filled-2d-triangle-strip/list))
    (declaim (inline %draw-list-draw-filled-2d-rectangle-list))
    (declaim (inline %draw-list-add-filled-2d-rectangle-list))
    (declaim (inline %draw-list-draw-textured-2d-rectangle-list))
    (declaim (inline %draw-list-add-textured-2d-rectangle-list))
    (declaim (inline %draw-list-draw-filled-2d-convex-polygon))
    (declaim (inline %draw-list-add-filled-2d-convex-polygon))
    (declaim (inline %draw-list-draw-filled-3d-triangle-strip/list))
    (declaim (inline %draw-list-add-filled-3d-triangle-strip/list))
    (declaim (inline %draw-list-draw-filled-3d-triangle-strip/list-with-normals))
    (declaim (inline %draw-list-add-filled-3d-triangle-strip/list-with-normals))
    (declaim (inline %draw-list-draw-multicolor-3d-triangle-strip/list-with-normals))
    (declaim (inline %draw-list-add-multicolor-3d-triangle-strip/list-with-normals))
    (declaim (inline %draw-list-draw-textured-3d-triangle-strip/list))
    (declaim (inline %draw-list-add-textured-3d-triangle-strip/list))
    (declaim (inline %draw-list-draw-textured-3d-triangle-strip/list-with-normals))
    (declaim (inline %draw-list-add-textured-3d-triangle-strip/list-with-normals))
    (declaim (inline %draw-list-draw-multicolor-3d-convex-polygon-with-normals))
    (declaim (inline %draw-list-add-multicolor-3d-convex-polygon-with-normals))
    (declaim (inline %draw-list-draw-multicolor-3d-convex-polygon))
    (declaim (inline %draw-list-add-multicolor-3d-convex-polygon))
    (declaim (inline %draw-list-draw-filled-3d-convex-polygon-with-normals))
    (declaim (inline %draw-list-add-filled-3d-convex-polygon-with-normals))
    (declaim (inline %draw-list-draw-filled-3d-convex-polygon))
    (declaim (inline %draw-list-add-filled-3d-convex-polygon))
    (declaim (inline %draw-list-draw-filled-sphere))
    (declaim (inline %draw-list-add-filled-sphere))
    (declaim (inline %prim-reserve))))

(defmacro with-draw-list-transaction ((fn-name draw-list first-index initial-vtx-offset)
				      &body body)
  (let ((draw-list-sym (gensym)))
    `(let ((,draw-list-sym ,draw-list))
       (handler-case
	   (progn ,@body)
	 (error (c)
	   (warn (concatenate 'string "While in " ,(symbol-name fn-name) ": " (princ-to-string c)))
           ;;(break)
	   (setf (foreign-array-fill-pointer (draw-list-index-array ,draw-list-sym)) ,first-index
		 (foreign-array-fill-pointer (draw-list-vertex-array ,draw-list-sym)) ,initial-vtx-offset)
	   nil)))))

(defun %draw-list-draw-2d-point (2d-draw-list ub32-oid ub32-color sf-layer sf-x sf-y)
  ;; for point-list-pipeline
  (declare (type 3d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type single-float sf-x sf-y))
  (with-slots (vertex-array index-array) 2d-draw-list
    (let ((first-index (foreign-array-fill-pointer index-array))
          (vtx-offset (foreign-array-fill-pointer vertex-array)))
      (with-draw-list-transaction (%draw-list-draw-2d-point 2d-draw-list first-index vtx-offset)
	(let ((offset (standard-3d-vertex-array-push-extend vertex-array ub32-oid sf-x sf-y sf-layer ub32-color)))
          (index-array-push-extend index-array offset)
          (values))))))


(defun %draw-list-add-2d-point (2d-draw-list ub32-oid atom-group model-mtx sf-point-size ub32-color sf-layer sf-x sf-y)
  ;; for point-list-pipeline
  (declare (type 3d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type single-float sf-x sf-y))
  (with-slots (vertex-array index-array) 2d-draw-list
    (let ((first-index (foreign-array-fill-pointer index-array))
          (vtx-offset (foreign-array-fill-pointer vertex-array)))
      (with-draw-list-transaction (%draw-list-add-2d-point 2d-draw-list first-index vtx-offset)
	(standard-3d-vertex-array-push-extend vertex-array ub32-oid sf-x sf-y sf-layer ub32-color)
	(index-array-push-extend index-array 0)
	(let ((cmd (make-standard-draw-indexed-cmd
		    2d-draw-list
		    first-index 1 vtx-offset
		    atom-group model-mtx nil *white-texture* sf-point-size nil nil sf-layer)))
	  (vector-push-extend cmd (draw-list-cmd-vector 2d-draw-list))
	  cmd)))))


(defun %draw-list-draw-3d-point (3d-draw-list ub32-oid ub32-color sf-x sf-y sf-z)
  ;; for point-list pipeline
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type single-float sf-x sf-y sf-z))		 
  (with-slots (vertex-array index-array) 3d-draw-list
    (let ((first-index (foreign-array-fill-pointer index-array))
          (vtx-offset (foreign-array-fill-pointer vertex-array)))
      (with-draw-list-transaction (%draw-list-draw-3d-point 3d-draw-list first-index vtx-offset)
        (let ((offset (standard-3d-vertex-array-push-extend vertex-array ub32-oid sf-x sf-y sf-z ub32-color)))
          (index-array-push-extend index-array offset)
          (values))))))


(defun %draw-list-add-3d-point (3d-draw-list ub32-oid atom-group model-mtx sf-point-size ub32-color sf-x sf-y sf-z)
  ;; for point-list-pipeline
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type single-float sf-x sf-y))
  (with-slots (vertex-array index-array) 3d-draw-list
    (let ((first-index (foreign-array-fill-pointer index-array))
          (vtx-offset (foreign-array-fill-pointer vertex-array)))
      (with-draw-list-transaction (%draw-list-add-3d-point 3d-draw-list first-index vtx-offset)
	(standard-3d-vertex-array-push-extend vertex-array ub32-oid sf-x sf-y sf-z ub32-color)
	(index-array-push-extend index-array 0)
	(let ((cmd (make-standard-draw-indexed-cmd 
		    3d-draw-list
		    first-index 1 vtx-offset
		    atom-group model-mtx nil *white-texture* sf-point-size nil nil)))
	  (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
	  cmd)))))


(defun %draw-list-draw-2d-line (2d-draw-list ub32-oid ub32-color sf-layer sf-x0 sf-y0 sf-x1 sf-y1)
  ;; for line-list pipeline
  (declare (type 3d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type single-float sf-x0 sf-y0 sf-x1 sf-y1))
  (let* ((ia (draw-list-index-array 2d-draw-list))
         (va (draw-list-vertex-array 2d-draw-list))
         (first-index (foreign-array-fill-pointer ia))
         (vtx-offset (foreign-array-fill-pointer va)))
    (with-draw-list-transaction (%draw-list-draw-2d-line 2d-draw-list first-index vtx-offset)
      (let ((offset0 (standard-3d-vertex-array-push-extend va ub32-oid sf-x0 sf-y0 sf-layer ub32-color))
            (offset1 (standard-3d-vertex-array-push-extend va ub32-oid sf-x1 sf-y1 sf-layer ub32-color)))
        (index-array-push-extend ia offset0)
        (index-array-push-extend ia offset1)
        (values)))))


(defun %draw-list-add-2d-line (2d-draw-list ub32-oid atom-group model-mtx sf-line-thickness ub32-color sf-layer sf-x0 sf-y0 sf-x1 sf-y1)
  ;; for line-list pipeline
  (declare (type 3d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type single-float sf-x0 sf-y0 sf-x1 sf-y1))
  (let* ((ia (draw-list-index-array 2d-draw-list))
         (va (draw-list-vertex-array 2d-draw-list))
         (first-index (foreign-array-fill-pointer ia))
         (vtx-offset (foreign-array-fill-pointer va)))
    (with-draw-list-transaction (%draw-list-add-2d-line 2d-draw-list first-index vtx-offset)
      (standard-3d-vertex-array-push-extend va ub32-oid sf-x0 sf-y0 sf-layer ub32-color)
      (standard-3d-vertex-array-push-extend va ub32-oid sf-x1 sf-y1 sf-layer ub32-color)
      (index-array-push-extend ia 0)
      (index-array-push-extend ia 1)
      (let ((cmd (make-standard-draw-indexed-cmd
                  2d-draw-list
                  first-index 2 vtx-offset
                  atom-group model-mtx nil *white-texture* nil sf-line-thickness nil sf-layer)))
        (vector-push-extend cmd (draw-list-cmd-vector 2d-draw-list))
        cmd))))


(defun %draw-list-draw-3d-line (3d-draw-list ub32-oid ub32-color sf-x0 sf-y0 sf-z0 sf-x1 sf-y1 sf-z1)
  ;; for line-list pipeline
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type single-float sf-x0 sf-y0 sf-z0 sf-x1 sf-y1 sf-z1))
  (let* ((ia (draw-list-index-array 3d-draw-list))
         (va (draw-list-vertex-array 3d-draw-list))
         (first-index (foreign-array-fill-pointer ia))
         (vtx-offset (foreign-array-fill-pointer va)))
    (with-draw-list-transaction (%draw-list-draw-3d-line 3d-draw-list first-index vtx-offset)
      (let ((offset0 (standard-3d-vertex-array-push-extend va ub32-oid sf-x0 sf-y0 sf-z0 ub32-color))
            (offset1 (standard-3d-vertex-array-push-extend va ub32-oid sf-x1 sf-y1 sf-z1 ub32-color)))
        (index-array-push-extend ia offset0)
        (index-array-push-extend ia offset1)
        (values)))))


(defun %draw-list-add-3d-line (3d-draw-list ub32-oid atom-group model-mtx sf-line-thickness ub32-color
			       sf-x0 sf-y0 sf-z0 sf-x1 sf-y1 sf-z1)
  ;; for line-list pipeline
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type single-float sf-x0 sf-y0 sf-z0 sf-x1 sf-y1 sf-z1))
  (let* ((ia (draw-list-index-array 3d-draw-list))
         (va (draw-list-vertex-array 3d-draw-list))
         (first-index (foreign-array-fill-pointer ia))
         (vtx-offset (foreign-array-fill-pointer va)))
    (with-draw-list-transaction (%draw-list-add-3d-line 3d-draw-list first-index vtx-offset)
      (standard-3d-vertex-array-push-extend va ub32-oid sf-x0 sf-y0 sf-z0 ub32-color)
      (standard-3d-vertex-array-push-extend va ub32-oid sf-x1 sf-y1 sf-z1 ub32-color)
      (index-array-push-extend ia 0)
      (index-array-push-extend ia 1)
      (let ((cmd (make-standard-draw-indexed-cmd
		  3d-draw-list
		  first-index 2 vtx-offset
		  atom-group model-mtx nil *white-texture* nil sf-line-thickness nil)))
	(vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
	cmd))))


(defun %draw-list-draw-2d-polyline (2d-draw-list ub32-oid bool-closed? ub32-color sf-layer seq-vertices)
  ;; for line-list pipeline
  (declare (type 3d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type boolean bool-closed?))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type sequence seq-vertices))
  ;; there must be at least one vertex to succeed
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
	 (vtx-offset (foreign-array-fill-pointer vertex-array))
	 (number-of-vertices 0)
	 (offset -1))
    (declare (type fixnum vtx-offset number-of-vertices offset))
    (with-draw-list-transaction (%draw-list-draw-2d-polyline 2d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x1 y1) on (cddr seq-vertices) by #'cddr
	       for (x0 y0) on seq-vertices by #'cddr
               do (setq x0 (clampf x0))
	       (setq y0 (clampf y0))
	       (setq x1 (clampf x1))
	       (setq y1 (clampf y1))
	       (setq offset (standard-3d-vertex-array-push-extend vertex-array ub32-oid x0 y0 sf-layer ub32-color))
	       (index-array-push-extend index-array offset)
	       (setq offset (standard-3d-vertex-array-push-extend vertex-array ub32-oid x1 y1 sf-layer ub32-color))
	       (index-array-push-extend index-array offset)
	       (incf number-of-vertices 2)
               finally (assert (>= number-of-vertices 2))
               (when bool-closed?
                 (index-array-push-extend index-array offset)
		 (index-array-push-extend index-array vtx-offset))
	       (return (values))))))))


(defun %draw-list-draw-2d-rectangle (2d-draw-list ub32-oid ub32-color sf-layer sf-x0 sf-y0 sf-x1 sf-y1)
  ;; for line-list pipeline
  (declare (type 3d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type single-float sf-x0 sf-y0 sf-x1 sf-y1))
  (%draw-list-draw-2d-polyline 2d-draw-list ub32-oid t ub32-color sf-layer
                               (list sf-x0 sf-y0 sf-x0 sf-y1 sf-x1 sf-y1 sf-x1 sf-y0)))



(defun %draw-list-add-2d-polyline (2d-draw-list ub32-oid atom-group model-mtx bool-closed?
				   sf-line-thickness ub32-color sf-layer seq-vertices)
  ;; for line-strip pipeline
  (declare (type 3d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type boolean bool-closed?))
  (declare (type single-float sf-line-thickness))
  (declare (type sequence seq-vertices))
  ;; there must be at least one vertex to succeed
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (elem-count 0))
    (declare (type fixnum elem-count))
    (with-draw-list-transaction (%draw-list-add-2d-polyline 2d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x y) on seq-vertices by #'cddr
               do (setq x (clampf x))
	       (setq y (clampf y))
	       (standard-3d-vertex-array-push-extend vertex-array ub32-oid x y sf-layer ub32-color)
               (index-array-push-extend index-array elem-count)
               (incf elem-count)
               finally (when bool-closed?
                         (index-array-push-extend index-array 0)
                         (incf elem-count)))))
      (assert (>= elem-count 2))
      (let ((cmd (make-standard-draw-indexed-cmd
                  2d-draw-list
                  first-index elem-count vtx-offset
                  atom-group model-mtx nil *white-texture* nil sf-line-thickness nil sf-layer)))
        (vector-push-extend cmd (draw-list-cmd-vector 2d-draw-list))
        cmd))))


(defun %draw-list-add-2d-rectangle (2d-draw-list ub32-oid atom-group model-mtx sf-line-thickness ub32-color sf-layer
                                    sf-x0 sf-y0 sf-x1 sf-y1)
  ;; for line-strip pipeline
  (declare (type 3d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type single-float sf-x0 sf-y0 sf-x1 sf-y1))
  (%draw-list-add-2d-polyline 2d-draw-list ub32-oid atom-group model-mtx t sf-line-thickness sf-layer
			      ub32-color (list sf-x0 sf-y0 sf-x0 sf-y1 sf-x1 sf-y1 sf-x1 sf-y0)))

;; the argument `seq-vertices' is a cl sequence of x y color ... repeating
(defun %draw-list-draw-multicolor-2d-polyline (2d-draw-list ub32-oid bool-closed? sf-layer seq-vertices)
  ;; uses line-list
  (declare (type 3d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type boolean bool-closed?))
  (declare (type sequence seq-vertices))
  ;; there must be at least one vertex to succeed
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
	 (offset -1)
	 (number-of-vertices 0))
    (declare (type fixnum vtx-offset offset number-of-vertices))
    (with-draw-list-transaction (%draw-list-draw-multicolor-2d-polyline 2d-draw-list first-index vtx-offset)
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
	       (setq offset (standard-3d-vertex-array-push-extend vertex-array ub32-oid x0 y0 sf-layer color0))
	       (index-array-push-extend index-array offset)
	       (setq offset (standard-3d-vertex-array-push-extend vertex-array ub32-oid x1 y1 sf-layer color1))
	       (index-array-push-extend index-array offset)
	       (incf number-of-vertices 2)
	       finally (assert (>= number-of-vertices 2))
	       (when bool-closed?
                 (index-array-push-extend index-array offset)
		 (index-array-push-extend index-array vtx-offset))
	       (return (values))))))))


(defun %draw-list-add-multicolor-2d-polyline (2d-draw-list ub32-oid atom-group model-mtx bool-closed? sf-line-thickness sf-layer
					      seq-vertices)
  ;; uses line-strip
  (declare (type 3d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type boolean bool-closed?))
  (declare (type single-float sf-line-thickness))
  (declare (type sequence seq-vertices))
  ;; there must be at least one vertex to succeed
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
         (cmd-vector (draw-list-cmd-vector 2d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (elem-count 0))
    (declare (type fixnum elem-count))
    (with-draw-list-transaction (%draw-list-add-multicolor-2d-polyline 2d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x y color) on seq-vertices by #'cdddr
	       do (setq x (clampf x))
	       (setq y (clampf y))
	       (setq color (canonicalize-color color))
	       (standard-3d-vertex-array-push-extend vertex-array ub32-oid x y sf-layer color)
	       (index-array-push-extend index-array elem-count)
	       (incf elem-count)
	       finally (when bool-closed?
			 (index-array-push-extend index-array 0)
			 (incf elem-count)))))
      (assert (>= elem-count 2))
      (let ((cmd (make-standard-draw-indexed-cmd
		  2d-draw-list
		  first-index elem-count vtx-offset
		  atom-group model-mtx nil *white-texture* nil sf-line-thickness nil sf-layer)))
        (vector-push-extend cmd cmd-vector)
        cmd))))


(defun %draw-list-draw-2d-line-list (2d-draw-list ub32-oid ub32-color sf-layer seq-vertices)
  (declare (type 3d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type sequence seq-vertices))
  (let* ((index-array (draw-list-index-array 2d-draw-list))
	 (vertex-array (draw-list-vertex-array 2d-draw-list)))
    (declare (type foreign-adjustable-array index-array vertex-array))
    (let ((first-index (foreign-array-fill-pointer index-array))
	  (vtx-offset (foreign-array-fill-pointer vertex-array)))
      (with-draw-list-transaction (%draw-list-draw-2d-line-list 2d-draw-list first-index vtx-offset)
	(etypecase seq-vertices
	  (list
	   (when (cdddr seq-vertices)
	     (loop for (x0 y0 x1 y1) on seq-vertices by #'cddddr
		   do (setq x0 (clampf x0))
		   (setq y0 (clampf y0))
		   (setq x1 (clampf x1))
		   (setq y1 (clampf y1))
		   (let ((offset (standard-3d-vertex-array-push-extend vertex-array ub32-oid x0 y0 sf-layer ub32-color)))
		     (index-array-push-extend index-array offset))
		   (let ((offset (standard-3d-vertex-array-push-extend vertex-array ub32-oid x1 y1 sf-layer ub32-color)))
		     (index-array-push-extend index-array offset))
		   finally (return (values))))))))))


(defun %draw-list-add-2d-line-list (2d-draw-list ub32-oid atom-group model-mtx sf-line-thickness ub32-color sf-layer seq-vertices)
  (declare (type 3d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type sequence seq-vertices))
  (let* ((index-array (draw-list-index-array 2d-draw-list))
	 (vertex-array (draw-list-vertex-array 2d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
	 (cmd-vector (draw-list-cmd-vector 2d-draw-list))
	 (elem-count 0))
    (declare (type fixnum elem-count))
    (with-draw-list-transaction (%draw-list-add-2d-line-list 2d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (when (cdddr seq-vertices)
	   (loop for (x0 y0 x1 y1) on seq-vertices by #'cddddr
		 do (setq x0 (clampf x0))
		 (setq y0 (clampf y0))
		 (setq x1 (clampf x1))
		 (setq y1 (clampf y1))
		 (standard-3d-vertex-array-push-extend vertex-array ub32-oid x0 y0 sf-layer ub32-color)
		 (index-array-push-extend index-array elem-count)
		 (standard-3d-vertex-array-push-extend vertex-array ub32-oid x1 y1 sf-layer ub32-color)
		 (index-array-push-extend index-array (1+ elem-count))
                 (incf elem-count 2)))))

      (let ((cmd (make-standard-draw-indexed-cmd
		  2d-draw-list
		  first-index elem-count vtx-offset
		  atom-group model-mtx nil *white-texture* nil sf-line-thickness nil sf-layer)))
        (vector-push-extend cmd cmd-vector)
        cmd))))


(defun %draw-list-draw-2d-circular-arc (2d-draw-list ub32-oid bool-closed? ub32-color sf-layer
					df-center-x df-center-y df-radius df-start-angle df-end-angle
					fixnum-number-of-segments)
  ;; uses line list instead of line strip
  (declare (type 3d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type boolean bool-closed?))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type double-float df-center-x df-center-y df-radius df-start-angle df-end-angle))
  (declare (type fixnum fixnum-number-of-segments))

  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
	 (offset -1))
    (declare (type fixnum vtx-offset offset))
    (with-draw-list-transaction (%draw-list-draw-2d-circular-arc 2d-draw-list first-index vtx-offset)
      (let* ((dtheta (- df-start-angle df-end-angle)))
	(declare (type double-float dtheta))
	(setq offset
	      (standard-3d-vertex-array-push-extend vertex-array
						    ub32-oid
						    (clampf (+ df-center-x (* df-radius (cos df-start-angle))))
						    (clampf (+ df-center-y (* df-radius (sin df-start-angle))))
						    sf-layer
						    ub32-color))
	(loop repeat fixnum-number-of-segments
              with theta = df-start-angle
              with step = (/ dtheta fixnum-number-of-segments)
              do (let* ((angle (+ (cl:the double-float theta) (cl:the double-float step)))
			(coord-x (clampf (+ df-center-x (* df-radius (cos angle)))))
			(coord-y (clampf (+ df-center-y (* df-radius (sin angle))))))
                   (index-array-push-extend index-array offset)
                   (setq offset (standard-3d-vertex-array-push-extend vertex-array ub32-oid coord-x coord-y sf-layer ub32-color))
                   (index-array-push-extend index-array offset)
                   (incf theta step))
              finally (when bool-closed?
                        (index-array-push-extend index-array vtx-offset))
	      (return (values)))))))



(defun %draw-list-add-2d-circular-arc (2d-draw-list ub32-oid atom-group model-mtx bool-closed? sf-line-thickness ub32-color sf-layer
                                       df-center-x df-center-y df-radius df-start-angle df-end-angle
                                       fixnum-number-of-segments
                                       &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
  ;; for use with line strip pipeline
  (declare (type 3d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type function cmd-constructor))
  (declare (type boolean bool-closed?))
  (declare (type single-float sf-line-thickness))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type double-float df-center-x df-center-y df-radius df-start-angle df-end-angle))
  (declare (type fixnum fixnum-number-of-segments))
  
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (first-index (foreign-array-fill-pointer index-array))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array)))

    (with-draw-list-transaction (%draw-list-add-2d-circular-arc 2d-draw-list first-index vertex-array)
      (unless (or (minusp df-radius) (minusp fixnum-number-of-segments))
        (let* ((dtheta (- df-start-angle df-end-angle)))
          (loop for i from 0 to fixnum-number-of-segments
                with theta = df-start-angle
                with step = (/ dtheta fixnum-number-of-segments)
                do (let* ((coord-x (clampf
				    (+ df-center-x (* df-radius (cos (cl:the double-float theta))))))
                          (coord-y (clampf
				    (+ df-center-y (* df-radius (sin (cl:the double-float theta)))))))
		     (standard-3d-vertex-array-push-extend vertex-array ub32-oid coord-x coord-y sf-layer ub32-color)
		     (index-array-push-extend index-array i)
		     (incf (cl:the double-float theta) step))
                finally (when bool-closed?
                          (index-array-push-extend index-array 0)
			  (incf fixnum-number-of-segments)))
          (let ((cmd (funcall cmd-constructor
                              2d-draw-list
                              first-index (1+ fixnum-number-of-segments) vtx-offset
                              atom-group model-mtx nil *white-texture* sf-line-thickness nil sf-layer)))
	    (vector-push-extend cmd (draw-list-cmd-vector 2d-draw-list))
	    cmd))))))


(defun %draw-list-draw-2d-circle (2d-draw-list ub32-oid ub32-color sf-layer
				  df-center-x df-center-y df-radius
				  fixnum-number-of-segments)
  ;; uses line list instead of line strip
  (declare (type 3d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type double-float df-center-x df-center-y df-radius))
  (declare (type fixnum fixnum-number-of-segments))
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
	 (offset -1))
    (declare (type fixnum vtx-offset offset))
    (with-draw-list-transaction (%draw-list-draw-2d-circle 2d-draw-list first-index vtx-offset)
      (setq offset
	    (standard-3d-vertex-array-push-extend vertex-array
						  ub32-oid
						  (clampf (+ df-center-x (* df-radius #.(cos 0))))
						  (clampf (+ df-center-y (* df-radius #.(sin 0))))
						  sf-layer
						  ub32-color))
      (loop repeat fixnum-number-of-segments
            with theta = 0.0d0
            with step = (/ 2pi fixnum-number-of-segments)
            do (let* ((angle (+ (cl:the double-float theta) (cl:the double-float step)))
		      (coord-x (clampf (+ df-center-x (* df-radius (cos angle)))))
                      (coord-y (clampf (+ df-center-y (* df-radius (sin angle))))))
                 (index-array-push-extend index-array offset)
		 (setq offset (standard-3d-vertex-array-push-extend vertex-array ub32-oid coord-x coord-y sf-layer ub32-color))
		 (index-array-push-extend index-array offset)
		 (incf theta step))
	    finally (return (values))))))


(defun %draw-list-add-2d-circle (2d-draw-list ub32-oid atom-group model-mtx sf-line-thickness ub32-color sf-layer
                                 df-center-x df-center-y df-radius fixnum-number-of-segments
                                 &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
  (declare (type 3d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type function cmd-constructor))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type double-float df-center-x df-center-y df-radius))
  (declare (type fixnum fixnum-number-of-segments))

  (break)

  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (first-index (foreign-array-fill-pointer index-array))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array)))
    (with-draw-list-transaction (%draw-list-add-2d-circle 2d-draw-list first-index vtx-offset)
      (unless (minusp df-radius)
        (loop for i from 0 to fixnum-number-of-segments
              with theta = 0.0d0
              with step = (/ 2pi fixnum-number-of-segments)
              do (let* ((coord-x (clampf (+ df-center-x (* df-radius (cos (cl:the double-float theta))))))
                        (coord-y (clampf (+ df-center-y (* df-radius (sin (cl:the double-float theta)))))))
                   (standard-3d-vertex-array-push-extend vertex-array ub32-oid coord-x coord-y sf-layer ub32-color)
                   (index-array-push-extend index-array i)
                   (incf theta step))
              finally (index-array-push-extend index-array 0))
        (let ((cmd (funcall cmd-constructor
                            2d-draw-list
                            first-index (1+ fixnum-number-of-segments) vtx-offset
                            atom-group model-mtx nil sf-line-thickness nil sf-layer)))
          (vector-push-extend cmd (draw-list-cmd-vector 2d-draw-list))
          cmd)))))


(defun %draw-list-draw-3d-polyline (3d-draw-list ub32-oid bool-closed? ub32-color seq-vertices)
  ;; for line-list pipeline
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type boolean bool-closed?))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type sequence seq-vertices))
  ;; there must be at least one vertex to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
	 (offset -1)
	 (number-of-vertices 0))
    (declare (type fixnum vtx-offset offset number-of-vertices))
    (with-draw-list-transaction (%draw-list-draw-3d-polyline 3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x0 y0 z0 x1 y1 z1) on seq-vertices by #'cdddr
               do
               (setq x0 (clampf x0))
               (setq y0 (clampf y0))
               (setq z0 (clampf z0))

               (setq offset (standard-3d-vertex-array-push-extend vertex-array ub32-oid x0 y0 z0 ub32-color))
               (index-array-push-extend index-array offset)
               (incf number-of-vertices)

               when (and x1 y1 z1)
               do
               (setq x1 (clampf x1))
               (setq y1 (clampf y1))
               (setq z1 (clampf z1))

               (setq offset (standard-3d-vertex-array-push-extend vertex-array ub32-oid x1 y1 z1 ub32-color))
               (index-array-push-extend index-array offset)
               (incf number-of-vertices)

               finally
               (assert (>= number-of-vertices 2))
               (when bool-closed?
                 (index-array-push-extend index-array vtx-offset))
               (return (values))))))))


(defun %draw-list-add-3d-polyline (3d-draw-list ub32-oid atom-group model-mtx bool-closed? sf-line-thickness ub32-color
				   seq-vertices
				   &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
  ;; line-strip
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type function cmd-constructor))
  (declare (type single-float sf-line-thickness))
  (declare (type boolean bool-closed?))
  (declare (type sequence seq-vertices))
  ;; there must be at least one vertex to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (elem-count 0))
    (declare (type fixnum elem-count))
    (with-draw-list-transaction (%draw-list-add-3d-polyline 3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x y z) on seq-vertices by #'cdddr
	       do (setq x (clampf x))
	       (setq y (clampf y))
	       (setq z (clampf z))
	       (standard-3d-vertex-array-push-extend vertex-array ub32-oid x y z ub32-color)
	       (index-array-push-extend index-array elem-count)
	       (incf elem-count)
	       finally (assert (>= elem-count 2))
	       (when bool-closed?
		 (index-array-push-extend index-array 0)
		 (incf elem-count))))
	(array
	 (let ((len-vertices (length seq-vertices)))
	   (declare (type fixnum len-vertices))
	   (loop for i from 0 to (- len-vertices 3) by 3
		 do (let ((x (clampf (aref seq-vertices i)))
			  (y (clampf (aref seq-vertices (1+ i))))
			  (z (clampf (aref seq-vertices (+ i 2)))))
		      (standard-3d-vertex-array-push-extend vertex-array ub32-oid x y z ub32-color)
		      (index-array-push-extend index-array elem-count))
		 finally (incf elem-count (/ len-vertices 3))
		 (assert (>= elem-count 2))
		 (when bool-closed?
		   (index-array-push-extend index-array 0)
		   (incf elem-count))))))
      (let ((cmd (funcall cmd-constructor
                          3d-draw-list
                          first-index elem-count vtx-offset
                          atom-group model-mtx nil sf-line-thickness nil)))
        (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
        cmd))))


(defun %draw-list-draw-multicolor-3d-polyline (3d-draw-list ub32-oid bool-closed? seq-vertices)
  ;; uses line-list
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type boolean bool-closed?))
  (declare (type sequence seq-vertices))
  ;; there must be at least one vertex to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
	 (offset -1)
	 (number-of-vertices 0))
    (declare (type fixnum vtx-offset offset number-of-vertices))
    (with-draw-list-transaction (%draw-list-draw-multicolor-3d-polyline 3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x1 y1 z1 color1) on (cddddr seq-vertices) by #'cddddr
	       for (x0 y0 z0 color0) on seq-vertices by #'cddddr
	       do (setq x0 (clampf x0))
	       (setq y0 (clampf y0))
	       (setq z0 (clampf z0))
	       (setq color0 (canonicalize-color color0))
	       (setq x1 (clampf x1))
	       (setq y1 (clampf y1))
	       (setq z1 (clampf z1))
	       (setq color1 (canonicalize-color color1))
	       (setq offset (standard-3d-vertex-array-push-extend vertex-array ub32-oid x0 y0 z0 color0))
	       (index-array-push-extend index-array offset)
	       (setq offset (standard-3d-vertex-array-push-extend vertex-array ub32-oid x1 y1 z1 color1))
	       (index-array-push-extend index-array offset)
	       (incf number-of-vertices 2)
	       finally (assert (>= number-of-vertices 2))
	       (when bool-closed?
                 (index-array-push-extend index-array offset)
		 (index-array-push-extend index-array vtx-offset))
	       (return (values))))))))


(defun %draw-list-add-multicolor-3d-polyline
    (3d-draw-list ub32-oid atom-group model-mtx bool-closed? sf-line-thickness seq-vertices)
  ;; line-strip
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type single-float sf-line-thickness))
  (declare (type boolean bool-closed?))
  (declare (type sequence seq-vertices))
  ;; there must be at least one vertex to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (elem-count 0))
    (declare (type fixnum elem-count))
    (with-draw-list-transaction (%draw-list-add-multicolor-3d-polyline 3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x y z color) on seq-vertices by #'cddddr
	       do (setq x (clampf x))
	       (setq y (clampf y))
	       (setq z (clampf z))
	       (setq color (canonicalize-color color))
	       (standard-3d-vertex-array-push-extend vertex-array ub32-oid x y z color)
	       (index-array-push-extend index-array elem-count)
	       (incf elem-count)
	       finally (assert (>= elem-count 2))
	       (when bool-closed?
		 (index-array-push-extend index-array 0)
		 (incf elem-count)))))
      (let ((cmd (make-standard-draw-indexed-cmd
                  3d-draw-list
                  first-index elem-count vtx-offset
                  atom-group model-mtx nil sf-line-thickness nil)))
        (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
        cmd))))


(defun %draw-list-draw-filled-2d-triangle-list (2d-draw-list ub32-oid ub32-color sf-layer seq-vertices)
  ;; triangle list
  (declare (type 3d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type sequence seq-vertices))
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array)))
    (with-draw-list-transaction (%draw-list-draw-filled-2d-triangle-list 2d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x y) on seq-vertices by #'cddr
	       do (setq x (clampf x))
	       (setq y (clampf y))
	       (let ((offset (standard-3d-vertex-array-push-extend vertex-array ub32-oid x y sf-layer ub32-color)))
                 (index-array-push-extend index-array offset))
	       finally (return (values))))))))


(defun %draw-list-add-filled-2d-triangle-strip/list (2d-draw-list ub32-oid atom-group model-mtx ub32-color sf-layer seq-vertices)
  (declare (type 3d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type sequence seq-vertices))
  ;; must be at least three vertices to succeed
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (%draw-list-add-filled-2d-triangle-strip/list 2d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list (loop for (x y) on seq-vertices by #'cddr
		    for i from 0 below #.most-positive-fixnum
		    do (setq x (clampf x))
		    (setq y (clampf y))
		    (standard-3d-vertex-array-push-extend vertex-array ub32-oid x y sf-layer ub32-color)
		    (index-array-push-extend index-array i)
		    (incf number-of-vertices))))
      (assert (>= number-of-vertices 3))
      (let ((cmd (make-standard-draw-indexed-cmd
		  2d-draw-list
		  first-index number-of-vertices vtx-offset
		  atom-group model-mtx
		  nil *white-texture* nil nil nil sf-layer)))
        (vector-push-extend cmd (draw-list-cmd-vector 2d-draw-list))
        cmd))))


(defun %draw-list-draw-filled-2d-rectangle-list (2d-draw-list ub32-oid ub32-color sf-layer seq-vertices)
  ;; triangle list
  (declare (type 3d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type sequence seq-vertices))
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
         (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
	 (offset -1)
	 (number-of-vertices 0))
    (declare (type fixnum vtx-offset number-of-vertices))
    (declare (type (integer -1 #.(- most-positive-fixnum 3)) offset))
    (with-draw-list-transaction (%draw-list-draw-filled-2d-rectangle-list 2d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x0 y0 x1 y1) on seq-vertices by #'(lambda (list)
							(nthcdr 4 list))
               do (setq x0 (clampf x0))
	       (setq y0 (clampf y0))
	       (setq x1 (clampf x1))
	       (setq y1 (clampf y1))
	       (setq offset (standard-3d-vertex-array-push-extend vertex-array ub32-oid x0 y0 sf-layer ub32-color))
               (standard-3d-vertex-array-push-extend vertex-array ub32-oid x0 y1 sf-layer ub32-color)
               (standard-3d-vertex-array-push-extend vertex-array ub32-oid x1 y1 sf-layer ub32-color)
               (standard-3d-vertex-array-push-extend vertex-array ub32-oid x1 y0 sf-layer ub32-color)
               (index-array-push-extend index-array offset)
               (index-array-push-extend index-array (1+ offset))
               (index-array-push-extend index-array (+ 2 offset))
               (index-array-push-extend index-array offset)
               (index-array-push-extend index-array (+ 2 offset))
               (index-array-push-extend index-array (+ 3 offset))
	       (incf number-of-vertices 4)
	       finally (assert (>= number-of-vertices 4))
	       (return (values))))))))


(defun %draw-list-add-filled-2d-rectangle-list (2d-draw-list ub32-oid atom-group model-mtx ub32-color sf-layer seq-vertices)
  ;; triangle-list
  (declare (type 3d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type sequence seq-vertices))
  (when seq-vertices
    (let* ((index-array (draw-list-index-array 2d-draw-list))
           (vertex-array (draw-list-vertex-array 2d-draw-list))
           (vtx-offset (foreign-array-fill-pointer vertex-array))
           (first-index (foreign-array-fill-pointer index-array))
           (elem-count 0))
      (declare (type fixnum elem-count))
      (with-draw-list-transaction (%draw-list-add-filled-2d-rectangle-list 2d-draw-list first-index vtx-offset)
	(etypecase seq-vertices
	  (list
	   (loop for (x0 y0 x1 y1) on seq-vertices by #'(lambda (list)
							  (nthcdr 4 list))
		 for i from 0 by 4 below #.(- most-positive-fixnum 3)
		 do (setq x0 (clampf x0))
		    (setq y0 (clampf y0))
		    (setq x1 (clampf x1))
		    (setq y1 (clampf y1))
		    (standard-3d-vertex-array-push-extend vertex-array ub32-oid x0 y0 sf-layer ub32-color)
		    (standard-3d-vertex-array-push-extend vertex-array ub32-oid x0 y1 sf-layer ub32-color)
		    (standard-3d-vertex-array-push-extend vertex-array ub32-oid x1 y1 sf-layer ub32-color)
		    (standard-3d-vertex-array-push-extend vertex-array ub32-oid x1 y0 sf-layer ub32-color)
		    (index-array-push-extend index-array i)
		    (index-array-push-extend index-array (1+ i))
		    (index-array-push-extend index-array (+ 2 i))
		    (index-array-push-extend index-array i)
		    (index-array-push-extend index-array (+ 2 i))
		    (index-array-push-extend index-array (+ 3 i))
		    (incf elem-count 6))))
	(assert (>= elem-count 6))
	(let ((cmd (make-standard-draw-indexed-cmd
		    2d-draw-list
		    first-index elem-count vtx-offset
		    atom-group model-mtx nil *white-texture* nil nil nil sf-layer)))
          (vector-push-extend cmd (draw-list-cmd-vector 2d-draw-list))
          cmd)))))


(defun %draw-list-draw-textured-2d-rectangle-list (2d-draw-list ub32-oid ub32-color sf-layer seq-vertices)
  ;; used to implement draw-text and group-add-text
  (declare (type 3d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type sequence seq-vertices))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  ;; must be at least 2 vertices to succeed
  (when seq-vertices
    (let* ((index-array (draw-list-index-array 2d-draw-list))
           (vertex-array (draw-list-vertex-array 2d-draw-list)))
      (declare (type foreign-adjustable-array index-array vertex-array))
      (let ((first-index (foreign-array-fill-pointer index-array))
	    (vtx-offset (foreign-array-fill-pointer vertex-array))
	    (offset -1))
	(declare (type (integer 0 #.(- most-positive-fixnum 4)) vtx-offset))
	(declare (type (integer -1 #.(- most-positive-fixnum 3)) offset))
	(with-draw-list-transaction (%draw-list-draw-textured-2d-rectangle-list 2d-draw-list first-index vtx-offset)
	  (etypecase seq-vertices
	    (list
	     (loop for (x0 y0 u0 v0 x1 y1 u1 v1) on seq-vertices by #'(lambda (list)
									(nthcdr 8 list))
		   do (setq x0 (clampf x0))
		      (setq y0 (clampf y0))
		      (setq u0 (clampf u0))
		      (setq v0 (clampf v0))
		      (setq x1 (clampf x1))
		      (setq y1 (clampf y1))
		      (setq u1 (clampf u1))
		      (setq v1 (clampf v1))
		      (setq offset (textured-3d-vertex-array-push-extend vertex-array ub32-oid x0 y0 sf-layer u0 v0 ub32-color))
		      (textured-3d-vertex-array-push-extend vertex-array ub32-oid x0 y1 sf-layer u0 v1 ub32-color)
		      (textured-3d-vertex-array-push-extend vertex-array ub32-oid x1 y1 sf-layer u1 v1 ub32-color)
		      (textured-3d-vertex-array-push-extend vertex-array ub32-oid x1 y0 sf-layer u1 v0 ub32-color)
		      (index-array-push-extend index-array offset)
		      (index-array-push-extend index-array (1+ offset))
		      (index-array-push-extend index-array (+ 2 offset))
		      (index-array-push-extend index-array offset)
		      (index-array-push-extend index-array (+ 2 offset))
		      (index-array-push-extend index-array (+ 3 offset))
		   finally (assert (>= (foreign-array-fill-pointer vertex-array) (+ vtx-offset 4)))
			   (return (values))))))))))


(defun %draw-list-add-textured-2d-rectangle-list (2d-draw-list ub32-oid atom-group model-mtx texture ub32-color sf-layer seq-vertices 
                                                  &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
  ;; used to implement add-text
  (declare (type 3d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type function cmd-constructor))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type sequence seq-vertices))
  ;; must be at least 2 vertices to succeed
  (when seq-vertices
    (let* ((index-array (draw-list-index-array 2d-draw-list))
           (vertex-array (draw-list-vertex-array 2d-draw-list))
           (vtx-offset (foreign-array-fill-pointer vertex-array))
           (first-index (foreign-array-fill-pointer index-array))
           (elem-count 0))
      (declare (type fixnum elem-count))
      (declare (type (integer -1 #.(- most-positive-fixnum 4)) vtx-offset))
      (with-draw-list-transaction (%draw-list-add-textured-2d-rectangle-list 2d-draw-list first-index vtx-offset)
	(etypecase seq-vertices
	  (list
	   (loop for (x0 y0 u0 v0 x1 y1 u1 v1) on seq-vertices by #'(lambda (list)
								      (nthcdr 8 list))
		 for i from 0 by 4 below #.(- most-positive-fixnum 3)
		 do (setq x0 (clampf x0))
		    (setq y0 (clampf y0))
		    (setq u0 (clampf u0))
		    (setq v0 (clampf v0))
		    (setq x1 (clampf x1))
		    (setq y1 (clampf y1))
		    (setq u1 (clampf u1))
		    (setq v1 (clampf v1))
		    (textured-3d-vertex-array-push-extend vertex-array ub32-oid x0 y0 sf-layer u0 v0 ub32-color)
		    (textured-3d-vertex-array-push-extend vertex-array ub32-oid x0 y1 sf-layer u0 v1 ub32-color)
		    (textured-3d-vertex-array-push-extend vertex-array ub32-oid x1 y1 sf-layer u1 v1 ub32-color)
		    (textured-3d-vertex-array-push-extend vertex-array ub32-oid x1 y0 sf-layer u1 v0 ub32-color)
		    (index-array-push-extend index-array i)
		    (index-array-push-extend index-array (1+ i))
		    (index-array-push-extend index-array (+ 2 i))
		    (index-array-push-extend index-array i)
		    (index-array-push-extend index-array (+ 2 i))
		    (index-array-push-extend index-array (+ 3 i))
		    (incf elem-count 6))))
	(assert (>= (foreign-array-fill-pointer vertex-array) (+ vtx-offset 4)))
	(let ((cmd (funcall cmd-constructor
			    2d-draw-list
			    first-index elem-count vtx-offset
			    atom-group model-mtx
			    nil texture nil nil nil sf-layer)))
	  (vector-push-extend cmd (draw-list-cmd-vector 2d-draw-list))
	  cmd)))))


(defun %draw-list-draw-filled-2d-convex-polygon (2d-draw-list ub32-oid ub32-color sf-layer seq-vertices)
  (declare (type 3d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type sequence seq-vertices))
  ;; must be at least 3 vertexes to succeed
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
	 (offset -1)
         (number-of-vertices 0))
    (declare (type fixnum number-of-vertices offset vtx-offset))
    (with-draw-list-transaction (%draw-list-draw-filled-2d-convex-polygon 2d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x y) on seq-vertices by #'cddr
	       for i from 0 below #.most-positive-fixnum
	       do (setq x (clampf x))
	       (setq y (clampf y))
	       (setq offset (standard-3d-vertex-array-push-extend vertex-array ub32-oid x y sf-layer ub32-color))
	       (incf number-of-vertices)
	       when (>= i 2)
	       do (index-array-push-extend index-array vtx-offset)
               (index-array-push-extend index-array (1- offset))
	       (index-array-push-extend index-array offset)
	       finally (assert (>= number-of-vertices 3))
	       (return (values))))))))


(defun %draw-list-add-filled-2d-convex-polygon (2d-draw-list ub32-oid atom-group model-mtx ub32-color sf-layer seq-vertices)
  (declare (type 3d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type sequence seq-vertices))
  ;; must be at least 3 vertexes to succeed
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (%draw-list-add-filled-2d-convex-polygon 2d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for i from 0 below #.most-positive-fixnum
	       for (x y) on seq-vertices by #'cddr
	       do (setq x (clampf x))
	       (setq y (clampf y))
	       (standard-3d-vertex-array-push-extend vertex-array ub32-oid x y sf-layer ub32-color)
	       (incf number-of-vertices)
	       when (>= i 2)
	       do (index-array-push-extend index-array 0)
	       (index-array-push-extend index-array (1- i))
	       (index-array-push-extend index-array i))))
      (assert (>= number-of-vertices 3))
      (let ((cmd (make-standard-draw-indexed-cmd
		  2d-draw-list
		  first-index (* 3 (- number-of-vertices 2)) vtx-offset
		  atom-group model-mtx nil *white-texture* nil nil nil sf-layer)))
        (vector-push-extend cmd (draw-list-cmd-vector 2d-draw-list))
        cmd))))


(defun %draw-list-draw-filled-3d-triangle-strip/list (3d-draw-list ub32-oid ub32-color seq-vertices)
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type sequence seq-vertices))
  ;; must be at least three vertices to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
	 (offset -1)
         (number-of-vertices 0))
    (declare (type fixnum offset vtx-offset number-of-vertices))
    (with-draw-list-transaction (%draw-list-draw-filled-3d-triangle-strip/list 3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list (loop for (x y z) on seq-vertices by #'cdddr
		    do (setq x (clampf x))
		    (setq y (clampf y))
		    (setq z (clampf z))
		    (setq offset (standard-3d-vertex-array-push-extend vertex-array ub32-oid x y z ub32-color))
                    (index-array-push-extend index-array offset)
		    (incf number-of-vertices)
		    finally (assert (>= number-of-vertices 3))
		    (return (values))))))))


(defun %draw-list-add-filled-3d-triangle-strip/list (3d-draw-list ub32-oid atom-group model-mtx ub32-color seq-vertices)
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type sequence seq-vertices))
  ;; must be at least three vertices to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (%draw-list-add-filled-3d-triangle-strip/list 3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list (loop for (x y z) on seq-vertices by #'cdddr
		    for i from 0 below #.most-positive-fixnum
		    do (setq x (clampf x))
		    (setq y (clampf y))
		    (setq z (clampf z))
		    (standard-3d-vertex-array-push-extend vertex-array ub32-oid x y z ub32-color)
		    (index-array-push-extend index-array i)
		    (incf number-of-vertices))))
      (assert (>= number-of-vertices 3))
      (let ((cmd (make-standard-draw-indexed-cmd
		  3d-draw-list
		  first-index number-of-vertices vtx-offset
		  atom-group model-mtx
		  nil *white-texture* nil nil nil)))
        (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
        cmd))))


(defun %draw-list-draw-filled-3d-triangle-strip/list-with-normals (3d-draw-list ub32-oid ub32-color seq-vertices)
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type sequence seq-vertices))
  ;; must be at least three vertices to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
	 (number-of-vertices 0))
    (declare (type fixnum vtx-offset number-of-vertices))
    (with-draw-list-transaction (%draw-list-draw-filled-3d-triangle-strip/list-with-normals 3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list (loop for (x y z nx ny nz) on seq-vertices by #'cdddr
		    do (setq x (clampf x))
		    (setq y (clampf y))
		    (setq z (clampf z))
		    (setq nx (clampf nx))
		    (setq ny (clampf ny))
		    (setq nz (clampf nz))
		    (let ((offset (standard-3d-vertex-with-normal-array-push-extend
				   vertex-array ub32-oid x y z nx ny nz ub32-color)))
                      (index-array-push-extend index-array offset))
		    (incf number-of-vertices)
		    finally (assert (>= number-of-vertices 3))
		    (return (values))))))))


(defun %draw-list-add-filled-3d-triangle-strip/list-with-normals
    (3d-draw-list ub32-oid atom-group model-mtx ub32-color seq-vertices light-position)
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type sequence seq-vertices))
  ;; must be at least three vertices to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (%draw-list-add-filled-3d-triangle-strip/list-with-normals 3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list (loop for (x y z nx ny nz) on seq-vertices by #'cdddr
		    for i from 0 below #.most-positive-fixnum
		    do (setq x (clampf x))
		    (setq y (clampf y))
		    (setq z (clampf z))
		    (setq nx (clampf nx))
		    (setq ny (clampf ny))
		    (setq nz (clampf nz))
		    (standard-3d-vertex-with-normal-array-push-extend vertex-array ub32-oid x y z nx ny nz ub32-color)
		    (incf number-of-vertices)
		    (index-array-push-extend index-array i))))
      (let ((cmd (make-standard-draw-indexed-cmd
		  3d-draw-list
		  first-index number-of-vertices vtx-offset
		  atom-group model-mtx
		  nil *white-texture* nil nil light-position)))
        (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
        cmd))))


(defun %draw-list-draw-multicolor-3d-triangle-strip/list-with-normals (3d-draw-list ub32-oid seq-vertices)
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type sequence seq-vertices))
  ;; must be at least three vertices to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
	 (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (%draw-list-draw-multicolor-3d-triangle-strip/list-with-normals 3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list (loop for (x y z nx ny nz color) on seq-vertices by #'(lambda (list)
								      (nthcdr 7 list))
		    do (setq x (clampf x))
		    (setq y (clampf y))
		    (setq z (clampf z))
		    (setq nx (clampf nx))
		    (setq ny (clampf ny))
		    (setq nz (clampf nz))
		    (setq color (canonicalize-color color))
		    (let ((offset (standard-3d-vertex-with-normal-array-push-extend
				   vertex-array ub32-oid x y z nx ny nz color)))
                      (index-array-push-extend index-array offset))
		    (incf number-of-vertices)
		    finally (assert (>= number-of-vertices 3))
		    (return (values))))))))


(defun %draw-list-add-multicolor-3d-triangle-strip/list-with-normals
    (3d-draw-list ub32-oid atom-group model-mtx seq-vertices light-position)
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type sequence seq-vertices))
  ;; must be at least three vertices to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (%draw-list-add-multicolor-3d-triangle-strip/list-with-normals 3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list (loop for (x y z nx ny nz color) on seq-vertices by #'(lambda (list)
								      (nthcdr 7 list))
		    for i from 0 below #.most-positive-fixnum
		    do (setq x (clampf x))
		    (setq y (clampf y))
		    (setq z (clampf z))
		    (setq nx (clampf nx))
		    (setq ny (clampf ny))
		    (setq nz (clampf nz))
		    (setq color (canonicalize-color color))
		    (standard-3d-vertex-with-normal-array-push-extend vertex-array ub32-oid x y z nx ny nz color)
		    (incf number-of-vertices)
		    (index-array-push-extend index-array i))))
      (let ((cmd (make-standard-draw-indexed-cmd
		  3d-draw-list
		  first-index number-of-vertices vtx-offset
		  atom-group model-mtx
		  nil *white-texture* nil nil light-position)))
        (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
        cmd))))


(defun %draw-list-draw-textured-3d-triangle-strip/list (3d-draw-list ub32-oid ub32-color seq-vertices)
  ;; set the draw-list-texture!
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type sequence seq-vertices))
  ;; must be at least three vertices to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (%draw-list-draw-textured-3d-triangle-strip/list 3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x y z u v) on seq-vertices by #'(lambda (list)
						      (nthcdr 5 list))
	       do (setq x (clampf x))
	       (setq y (clampf y))
	       (setq z (clampf z))
	       (setq u (clampf u))
	       (setq v (clampf v))
	       (let ((offset (textured-3d-vertex-array-push-extend vertex-array ub32-oid x y z u v ub32-color)))
		 (index-array-push-extend index-array offset))
	       (incf number-of-vertices)
	       finally (assert (>= number-of-vertices 3))
	       (return (values))))))))


(defun %draw-list-add-textured-3d-triangle-strip/list (3d-draw-list ub32-oid atom-group model-mtx texture ub32-color seq-vertices)
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type sequence seq-vertices))
  ;; must be at least three vertices to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (%draw-list-add-textured-3d-triangle-strip/list 3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x y z u v) on seq-vertices by #'(lambda (list)
						      (nthcdr 5 list))
               for i from 0 below #.(1- most-positive-fixnum)
               do (setq x (clampf x))
	       (setq y (clampf y))
	       (setq z (clampf z))
	       (setq u (clampf u))
	       (setq v (clampf v))
	       (textured-3d-vertex-array-push-extend vertex-array ub32-oid x y z u v ub32-color)
               (index-array-push-extend index-array i)
               finally
               (setq number-of-vertices (1+ i)))))
      (let ((cmd (make-standard-draw-indexed-cmd
		  3d-draw-list
		  first-index number-of-vertices vtx-offset
		  atom-group model-mtx
		  nil texture nil nil nil)))
        (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
        cmd))))


(defun %draw-list-draw-textured-3d-triangle-strip/list-with-normals (3d-draw-list ub32-oid ub32-color seq-vertices)
  ;; set the draw-list-texture!
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type sequence seq-vertices))
  ;; must be at least three vertices to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
         (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (%draw-list-draw-textured-3d-triangle-strip/list-with-normals 3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x y z nx ny nz u v) on seq-vertices by #'(lambda (list)
							       (nthcdr 8 list))
	       do (setq x (clampf x))
	       (setq y (clampf y))
	       (setq z (clampf z))
	       (setq nx (clampf nx))
	       (setq ny (clampf ny))
	       (setq nz (clampf nz))
	       (setq u (clampf u))
	       (setq v (clampf v))
	       (let ((offset (textured-3d-vertex-with-normal-array-push-extend
			      vertex-array ub32-oid x y z nx ny nz u v ub32-color)))
                 (index-array-push-extend index-array offset))
	       (incf number-of-vertices)
	       finally (assert (>= number-of-vertices 3))
	       (return (values))))))))


(defun %draw-list-add-textured-3d-triangle-strip/list-with-normals
    (3d-draw-list ub32-oid atom-group model-mtx texture ub32-color seq-vertices light-position)
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type sequence seq-vertices))
  ;; must be at least three vertices to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (%draw-list-add-textured-3d-triangle-strip/list-with-normals 3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x y z nx ny nz u v) on seq-vertices by #'(lambda (list)
							       (nthcdr 8 list))
               for i from 0 below #.(1- most-positive-fixnum)
               do (setq x (clampf x))
	       (setq y (clampf y))
	       (setq z (clampf z))
	       (setq nx (clampf nx))
	       (setq ny (clampf ny))
	       (setq nz (clampf nz))			    
	       (setq u (clampf u))
	       (setq v (clampf v))
	       (textured-3d-vertex-with-normal-array-push-extend
		vertex-array ub32-oid x y z nx ny nz u v ub32-color)
               (index-array-push-extend index-array i)
               finally
               (setq number-of-vertices (1+ i)))))
      (assert (>= number-of-vertices 3))
      (let ((cmd (make-standard-draw-indexed-cmd
		  3d-draw-list
		  first-index number-of-vertices vtx-offset
		  atom-group model-mtx
		  nil texture nil nil light-position)))
        (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
        cmd))))


(defun %draw-list-draw-multicolor-3d-convex-polygon-with-normals (3d-draw-list ub32-oid seq-vertices)
  (declare (type 3d-vertex-with-normal-draw-list-mixin 3d-draw-list))
  (declare (type sequence seq-vertices))
  ;; must be at least 3 vertexes to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
	 (vtx-offset (foreign-array-fill-pointer vertex-array))
	 (offset -1)
	 (number-of-vertices 0))
    (declare (type fixnum vtx-offset offset number-of-vertices))
    (with-draw-list-transaction (%draw-list-draw-multicolor-3d-convex-polygon-with-normals 3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for i from 0 below #.most-positive-fixnum
	       for (x y z nx ny nz color) on seq-vertices by #'(lambda (list)
								 (nthcdr 7 list))
	       do (setq offset (standard-3d-vertex-with-normal-array-push-extend
				vertex-array ub32-oid (clampf x) (clampf y) (clampf z) (clampf nx) (clampf ny) (clampf nz)
				(canonicalize-color color)))
	       (incf number-of-vertices)
	       when (>= i 2)
	       do(index-array-push-extend index-array vtx-offset)
               (index-array-push-extend index-array (1- offset))
               (index-array-push-extend index-array offset)
	       finally (assert (>= number-of-vertices 3))
	       (return (values))))
	(array
         (let ((len (length seq-vertices)))
           (loop for j from 0 by 7 below len
	         for i from 0 below #.(- most-positive-fixnum 6)
	         do (setq offset
			  (standard-3d-vertex-with-normal-array-push-extend
			   vertex-array ub32-oid
			   (clampf (aref seq-vertices j))
			   (clampf (aref seq-vertices (1+ j)))
			   (clampf (aref seq-vertices (+ j 2)))
			   (clampf (aref seq-vertices (+ j 3)))
			   (clampf (aref seq-vertices (+ j 4)))
			   (clampf (aref seq-vertices (+ j 5)))
			   (canonicalize-color (aref seq-vertices (+ j 6)))))
		 (incf number-of-vertices)
	         when (>= i 2)
		 do (index-array-push-extend index-array vtx-offset)
		 (index-array-push-extend index-array (1- offset))
		 (index-array-push-extend index-array offset)
	         finally (assert (>= number-of-vertices 3))
		 (return (values)))))))))


(defun %draw-list-add-multicolor-3d-convex-polygon-with-normals (3d-draw-list ub32-oid atom-group model-mtx seq-vertices light-position)
  (declare (type 3d-vertex-with-normal-draw-list-mixin 3d-draw-list))
  (declare (type sequence seq-vertices))
  ;; must be at least 3 vertexes to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
	 (vtx-offset (foreign-array-fill-pointer vertex-array))
	 (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (%draw-list-add-multicolor-3d-convex-polygon-with-normals 3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for i from 0 below #.most-positive-fixnum
	       for (x y z nx ny nz color) on seq-vertices by #'(lambda (list)
								 (nthcdr 7 list))
	       do (standard-3d-vertex-with-normal-array-push-extend
		   vertex-array ub32-oid (clampf x) (clampf y) (clampf z) (clampf nx) (clampf ny) (clampf nz)
		   (canonicalize-color color))
	       (incf number-of-vertices)
	       when (>= i 2)
	       do (index-array-push-extend index-array 0)
               (index-array-push-extend index-array (1- i))
	       (index-array-push-extend index-array i)))
	(array
	 (loop for i from vtx-offset below most-positive-fixnum
	       for j from 0 by 7 below #.(- most-positive-fixnum 6)
	       do (standard-3d-vertex-with-normal-array-push-extend
		   vertex-array ub32-oid
		   (clampf (aref seq-vertices j))
		   (clampf (aref seq-vertices (1+ j)))
		   (clampf (aref seq-vertices (+ j 2)))
		   (clampf (aref seq-vertices (+ j 3)))
		   (clampf (aref seq-vertices (+ j 4)))
		   (clampf (aref seq-vertices (+ j 5)))
		   (canonicalize-color (aref seq-vertices (+ j 6))))
	       (incf number-of-vertices)
	       when (>= i 2)
	       do (index-array-push-extend index-array 0)
	       (index-array-push-extend index-array (1- i))
	       (index-array-push-extend index-array i))))
      (assert (>= number-of-vertices 3))
      (let ((cmd (make-standard-draw-indexed-cmd
		  3d-draw-list
		  first-index number-of-vertices vtx-offset
		  atom-group model-mtx
		  nil *white-texture* nil nil light-position)))
	(vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
	cmd))))


(defun %draw-list-draw-multicolor-3d-convex-polygon (3d-draw-list ub32-oid seq-vertices)
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type sequence seq-vertices))
  ;; must be at least 3 vertexes to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list)))
    (declare (foreign-adjustable-array index-array vertex-array))
    (let ((first-index (foreign-array-fill-pointer index-array))
	  (vtx-offset (foreign-array-fill-pointer vertex-array))
	  (offset -1)
	  (number-of-vertices 0))
      (declare (type fixnum vtx-offset number-of-vertices offset))
      (with-draw-list-transaction (%draw-list-draw-multicolor-3d-convex-polygon 3d-draw-list first-index vtx-offset)
	(etypecase seq-vertices
	  (list
	   (loop for (x y z color) on seq-vertices by #'(lambda (list)
							  (nthcdr 4 list))
		 for i from 0 below #.most-positive-fixnum
		 do (setq offset
			  (standard-3d-vertex-array-push-extend
			   vertex-array ub32-oid (clampf x) (clampf y) (clampf z) (canonicalize-color color)))
		 (incf number-of-vertices)
		 when (>= i 2)
		 do(index-array-push-extend index-array vtx-offset)
                 (index-array-push-extend index-array (1- offset))
                 (index-array-push-extend index-array offset)
		 finally (assert (>= number-of-vertices 3))
		 (return (values))))
	  (array
	   (loop for i from 0 below #.most-positive-fixnum
		 for k from 0 by 4 below #.(- most-positive-fixnum 3)
		 do (setq offset
			  (standard-3d-vertex-array-push-extend
			   vertex-array ub32-oid
			   (clampf (aref seq-vertices k))
			   (clampf (aref seq-vertices (1+ k)))
			   (clampf (aref seq-vertices (+ k 2)))
			   (canonicalize-color (elt seq-vertices (+ k 3)))))
		 (incf number-of-vertices)
		 when (>= i 2)
		 do (index-array-push-extend index-array vtx-offset)
		 (index-array-push-extend index-array (1- offset))
		 (index-array-push-extend index-array offset)
		 finally (assert (>= number-of-vertices 3))
		 (return (values)))))))))


(defun %draw-list-add-multicolor-3d-convex-polygon (3d-draw-list ub32-oid atom-group model-mtx seq-vertices)
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type sequence seq-vertices))
  ;; must be at least 3 vertexes to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
	 (vtx-offset (foreign-array-fill-pointer vertex-array))
	 (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (%draw-list-add-multicolor-3d-convex-polygon 3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for i from 0 below #.most-positive-fixnum
	       for (x y z color) on seq-vertices by #'(lambda (list)
							(nthcdr 4 list))
	       do (standard-3d-vertex-array-push-extend
		   vertex-array ub32-oid (clampf x) (clampf y) (clampf z)
		   (canonicalize-color color))
	       (incf number-of-vertices)
	       when (>= i 2)
	       do (index-array-push-extend index-array 0)
	       (index-array-push-extend index-array (1- i))
	       (index-array-push-extend index-array i)))
	(array
         (let ((len (length seq-vertices)))
           (loop for i from vtx-offset below most-positive-fixnum
		 for j from 0 by 4 below len
		 do (standard-3d-vertex-array-push-extend
		     vertex-array ub32-oid
		     (clampf (aref seq-vertices j))
		     (clampf (aref seq-vertices (1+ j)))
		     (clampf (aref seq-vertices (+ j 2)))
		     (canonicalize-color (aref seq-vertices (+ j 3))))
		 (incf number-of-vertices)
		 when (>= i 2)
		 do (index-array-push-extend index-array 0)
		 (index-array-push-extend index-array (1- i))
		 (index-array-push-extend index-array i)))))
      (assert (>= number-of-vertices 3))
      (let ((cmd (make-standard-draw-indexed-cmd
		  3d-draw-list
		  first-index number-of-vertices vtx-offset
		  atom-group model-mtx
		  nil *white-texture* nil nil)))
        (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
        cmd))))


(defun %draw-list-draw-filled-3d-convex-polygon-with-normals (3d-draw-list ub32-oid ub32-color seq-vertices)
  (declare (type 3d-vertex-with-normal-draw-list-mixin 3d-draw-list))
  (declare (type sequence seq-vertices))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  ;; must be at least 3 vertexes to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
	 (vtx-offset (foreign-array-fill-pointer vertex-array))
	 (offset -1)
	 (number-of-vertices 0))
    (declare (type fixnum vtx-offset offset number-of-vertices))
    (with-draw-list-transaction (%draw-list-draw-filled-3d-convex-polygon-with-normals 3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x y z nx ny nz) on seq-vertices by #'(lambda (list)
							   (nthcdr 6 list))
	       do (setq offset
			(standard-3d-vertex-with-normal-array-push-extend
			 vertex-array ub32-oid (clampf x) (clampf y) (clampf z)
			 (clampf nx) (clampf ny) (clampf nz) ub32-color))
	       (incf number-of-vertices))
         (loop for i from 2 below number-of-vertices
               ;;when (>= i 2)
	       do (index-array-push-extend index-array vtx-offset)
	       (index-array-push-extend index-array (1- (+ i vtx-offset)))
	       (index-array-push-extend index-array (+ i vtx-offset))
	       finally (assert (>= number-of-vertices 3))
	       (return (values))))
	(array
         (let ((len (length seq-vertices)))
           (assert (>= len 18))
           (loop for j from 0 by 6 below len
                 for i from 0 below #.most-positive-fixnum
	         do (setq offset
			  (standard-3d-vertex-with-normal-array-push-extend
			   vertex-array ub32-oid
			   (clampf (aref seq-vertices j))
			   (clampf (aref seq-vertices (1+ j)))
			   (clampf (aref seq-vertices (+ j 2)))
			   (clampf (aref seq-vertices (+ j 3)))
			   (clampf (aref seq-vertices (+ j 4)))
			   (clampf (aref seq-vertices (+ j 5)))
			   ub32-color))

	         when (>= i 2)
		 do (index-array-push-extend index-array vtx-offset)
                 (index-array-push-extend index-array (1- offset))
		 (index-array-push-extend index-array offset)
	         finally (return (values)))))))))


(defun %draw-list-add-filled-3d-convex-polygon-with-normals
    (3d-draw-list ub32-oid atom-group model-mtx ub32-color seq-vertices light-position)
  (declare (type 3d-vertex-with-normal-draw-list-mixin 3d-draw-list))
  (declare (type sequence seq-vertices))
  (declare (type (unsigned-byte 32) ub32-color))
  ;; must be at least 3 vertexes to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
	 (vtx-offset (foreign-array-fill-pointer vertex-array))
	 (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (%draw-list-add-filled-3d-convex-polygon-with-normals 3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for i from 0 below #.most-positive-fixnum
	       for (x y z nx ny nz) on seq-vertices by #'(lambda (list)
							   (nthcdr 6 list))
	       do (standard-3d-vertex-with-normal-array-push-extend
		   vertex-array ub32-oid (clampf x) (clampf y) (clampf z)
		   (clampf nx) (clampf ny) (clampf nz) ub32-color)
	       (incf number-of-vertices)
	       when (>= i 2)
	       do (index-array-push-extend index-array 0)
	       (index-array-push-extend index-array (1- i))
	       (index-array-push-extend index-array i)))
	(array
         (let ((len (length seq-vertices)))
           (loop for j from 0 by 6 below len
	         for i from 0 below #.most-positive-fixnum
	         do (standard-3d-vertex-with-normal-array-push-extend
		     vertex-array ub32-oid
		     (clampf (aref seq-vertices j))
		     (clampf (aref seq-vertices (1+ j)))
		     (clampf (aref seq-vertices (+ j 2)))
		     (clampf (aref seq-vertices (+ j 3)))
		     (clampf (aref seq-vertices (+ j 4)))
		     (clampf (aref seq-vertices (+ j 5)))
		     ub32-color)
		 (incf number-of-vertices)
	         when (>= i 2)
		 do (index-array-push-extend index-array 0)
		 (index-array-push-extend index-array (1- i))
		 (index-array-push-extend index-array i)))))
      (assert (>= number-of-vertices 3))
      (let ((cmd (make-standard-draw-indexed-cmd
		  3d-draw-list
		  first-index number-of-vertices vtx-offset
		  atom-group model-mtx
		  nil *white-texture* nil nil light-position)))
	(vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
	cmd))))


(defun %draw-list-draw-filled-3d-convex-polygon (3d-draw-list ub32-oid ub32-color seq-vertices)
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type sequence seq-vertices))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  ;; must be at least 3 vertexes to succeed
  (let ((index-array (draw-list-index-array 3d-draw-list))
	(vertex-array (draw-list-vertex-array 3d-draw-list)))
    (declare (type foreign-adjustable-array index-array vertex-array))
    (let ((first-index (foreign-array-fill-pointer index-array))
	  (vtx-offset (foreign-array-fill-pointer vertex-array))
	  (offset -1)
	  (number-of-vertices 0))
      (declare (type fixnum  offset number-of-vertices))
      (with-draw-list-transaction (%draw-list-draw-filled-3d-convex-polygon 3d-draw-list first-index vtx-offset)
	(etypecase seq-vertices
	  (list
	   (loop for i from 0 below #.most-positive-fixnum
		 for (x y z) on seq-vertices by #'cdddr
                 do (setq offset (standard-3d-vertex-array-push-extend
				  vertex-array ub32-oid (clampf x) (clampf y) (clampf z) ub32-color))
		 (incf number-of-vertices)
		 when (>= i 2)
		 do (index-array-push-extend index-array vtx-offset)
		 (index-array-push-extend index-array (1- offset))
		 (index-array-push-extend index-array offset)
		 finally (assert (>= number-of-vertices 3))
		 (return (values))))
	  (array
           (let ((len (length seq-vertices)))
             (assert (>= len 9))
             (loop for j from 0 by 3 below len
		   for i from 0  below #.most-positive-fixnum
		   do (setq offset
			    (standard-3d-vertex-array-push-extend
			     vertex-array ub32-oid
			     (clampf (aref seq-vertices j))
			     (clampf (aref seq-vertices (1+ j)))
			     (clampf (aref seq-vertices (+ j 2)))
			     ub32-color))
		   when (>= i 2)
		   do (index-array-push-extend index-array vtx-offset)
		   (index-array-push-extend index-array (1- offset))
		   (index-array-push-extend index-array offset)
		   finally (return (values))))))))))


(defun %draw-list-add-filled-3d-convex-polygon (3d-draw-list ub32-oid atom-group model-mtx ub32-color seq-vertices)
  (declare (type 3d-vertex-with-normal-draw-list-mixin 3d-draw-list))
  (declare (type sequence seq-vertices))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  ;; must be at least 3 vertexes to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list)))
    (declare (type foreign-adjustable-array index-array vertex-array))
    (let ((first-index (foreign-array-fill-pointer index-array))
	  (vtx-offset (foreign-array-fill-pointer vertex-array))
	  (number-of-vertices 0))
      (declare (type fixnum number-of-vertices))
      (with-draw-list-transaction (%draw-list-add-filled-3d-convex-polygon 3d-draw-list first-index vtx-offset)
	(etypecase seq-vertices
	  (list
	   (loop for i from 0 below #.most-positive-fixnum
		 for (x y z) on seq-vertices by #'cdddr
                 do (standard-3d-vertex-array-push-extend
		     vertex-array ub32-oid (clampf x) (clampf y) (clampf z) ub32-color)
		 (incf number-of-vertices)
		 when (>= i 2)
		 do (index-array-push-extend index-array 0)
		 (index-array-push-extend index-array (1- i))
		 (index-array-push-extend index-array i)))
	  (array
           (let ((len (length seq-vertices)))
             (assert (>= len 9))
             (loop for i from 0 below #.most-positive-fixnum
                   for j from 0 by 3 below len
                   do (standard-3d-vertex-array-push-extend
                       vertex-array ub32-oid
                       (clampf (aref seq-vertices j))
                       (clampf (aref seq-vertices (1+ j)))
                       (clampf (aref seq-vertices (+ j 2)))
                       ub32-color)
                   (incf number-of-vertices)
                   when (>= i 2)
                   do (index-array-push-extend index-array 0)
                   (index-array-push-extend index-array (1- i))
                   (index-array-push-extend index-array i)))))
	(let ((cmd (make-standard-draw-indexed-cmd
		    3d-draw-list
		    first-index number-of-vertices vtx-offset
		    atom-group model-mtx
		    nil *white-texture* nil nil nil)))
          (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
          cmd)))))


(defun %draw-list-draw-filled-sphere (3d-draw-list
				      ub32-oid
				      ub32-color
				      df-origin-x
				      df-origin-y
				      df-origin-z
				      df-radius
				      fixnum-resolution)
  (declare (type 3d-vertex-with-normal-draw-list-mixin 3d-draw-list))
  (declare (type (unsigned-byte 32) ub32-oid ub32-color))
  (declare (type double-float df-origin-x df-origin-y df-origin-z df-radius))
  (declare (type fixnum fixnum-resolution))
  (let* ((va (draw-list-vertex-array 3d-draw-list))
         (ia (draw-list-index-array 3d-draw-list)))
    (declare (type foreign-adjustable-array ia va))
    (let ((first-index (foreign-array-fill-pointer ia))
	  (vtx-offset (foreign-array-fill-pointer va)))
      (with-draw-list-transaction (%draw-list-draw-filled-sphere 3d-draw-list first-index vtx-offset)
	(block block
	  (when (> df-radius 0.0d0)
	    (let* ((sector-count fixnum-resolution)
		   (stack-count (floor fixnum-resolution 2))
		   (sector-step (/ 2pi sector-count))
		   (stack-step (/ pi stack-count)))
	      (declare (type fixnum stack-count sector-count))
              (loop for i from 0 to stack-count
		    do (let* ((stack-angle (- #.(/ pi 2) (* i stack-step))))
			 (declare (type double-float stack-angle))
			 (let ((xy (* df-radius (cos stack-angle)))
			       (z (+ (* df-radius (sin stack-angle)))))
			   (loop for j from 0 to sector-count
				 do (let* ((sector-angle (* j sector-step)))
				      (declare (type double-float sector-angle))
				      (let ((x (* xy (cos sector-angle)))
					    (y (* xy (sin sector-angle))))
					(declare (type double-float x y))
					(let ((nx (clampf (/ x df-radius)))
					      (ny (clampf (/ y df-radius)))
					      (nz (clampf (/ z df-radius))))

					  (standard-3d-vertex-with-normal-array-push-extend
					   va ub32-oid
					   (clampf (+ x df-origin-x))
					   (clampf (+ y df-origin-y))
					   (clampf (+ z df-origin-z))
					   nx ny nz ub32-color))))))))
              (loop for i from 0
		    repeat stack-count
		    with k1 with k2
		    do (setq k1 (+ vtx-offset (* i (1+ sector-count)))
			     k2 (+ k1 sector-count 1))

                    (loop repeat sector-count
			  when (not (= i 0))
                          do (index-array-push-extend ia k1)
                          (index-array-push-extend ia k2)
			  (index-array-push-extend ia (1+ k1))
			  when (not (= i (1- stack-count)))
                          do (index-array-push-extend ia (1+ k1))
			  (index-array-push-extend ia k2)
			  (index-array-push-extend ia (1+ k2))
			  do (incf k1)
			  (incf k2))
		    finally (return-from block (values))))))))))



(defun %draw-list-add-filled-sphere (3d-draw-list
				     ub32-oid
				     atom-group
				     model-mtx
				     ub32-color
				     df-origin-x
				     df-origin-y
				     df-origin-z
				     df-radius
				     fixnum-resolution
				     light-position)
  (declare (type 3d-vertex-with-normal-draw-list-mixin 3d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type double-float df-origin-x df-origin-y df-origin-z df-radius))
  (declare (type fixnum fixnum-resolution))
  (let* ((va (draw-list-vertex-array 3d-draw-list))
         (ia (draw-list-index-array 3d-draw-list))
         (vtx-offset (foreign-array-fill-pointer va))
         (first-index (foreign-array-fill-pointer ia)))
    (with-draw-list-transaction (%draw-list-add-filled-sphere 3d-draw-list first-index vtx-offset)
      (when (> df-radius 0.0d0)
	(let* ((sector-count fixnum-resolution)
               (stack-count (floor fixnum-resolution 2))
               (sector-step (/ 2pi sector-count))
               (stack-step (/ pi stack-count))
               (elem-count 0))
	  (declare (type fixnum elem-count sector-count stack-count))
          (loop for i from 0 to stack-count
                do (let* ((stack-angle (- #.(/ pi 2) (* i stack-step))))
		     (declare (type double-float stack-angle))
		     (let ((xy (* df-radius (cos stack-angle)))
			   (z (+ (* df-radius (sin stack-angle)) df-origin-z)))
		       (loop for j from 0 to sector-count
			     do (let* ((sector-angle (* j sector-step)))
				  (declare (type double-float sector-angle))
				  (let ((x (+ (* xy (cos sector-angle)) df-origin-x))
					(y (+ (* xy (sin sector-angle)) df-origin-y)))
				    (declare (type double-float x y))
				    (let ((nx (clampf (/ (- x df-origin-x) df-radius)))
					  (ny (clampf (/ (- y df-origin-y) df-radius)))
					  (nz (clampf (/ (- z df-origin-z) df-radius))))

				      (standard-3d-vertex-with-normal-array-push-extend
				       va ub32-oid (clampf x) (clampf y) (clampf z) nx ny nz ub32-color))))))))
          (loop for i from 0
		repeat stack-count
                with k1 with k2
                do (setq k1 (* i (1+ sector-count))
                         k2 (+ k1 sector-count 1))

                (loop for j from 0 below sector-count
                      when (not (= i 0))
                      do (index-array-push-extend ia k1)
                      (index-array-push-extend ia k2)
                      (index-array-push-extend ia (1+ k1))
                      (incf elem-count 3)
                      when (not (= i (1- stack-count)))
                      do (index-array-push-extend ia (1+ k1))
                      (index-array-push-extend ia k2)
                      (index-array-push-extend ia (1+ k2))
                      (incf elem-count 3)
                      do (incf k1)
		      (incf k2)))

          (let ((cmd (make-standard-draw-indexed-cmd
                      3d-draw-list
                      first-index elem-count vtx-offset
                      atom-group model-mtx
                      nil *white-texture* nil nil light-position)))
	    (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
	    cmd))))))

(defun %prim-reserve (draw-list vertex-count index-count vertex-type-size index-type-size)
  "%prim-reserve [potentially] allocates memory on the draw-list for indices and vertices
you can use prim-reserve when seq-vertices is an array."
  (declare (type fixnum vertex-count index-count vertex-type-size index-type-size))
  (declare (type draw-list-mixin draw-list))
  (let ((res nil))
    (block nil
      (with-slots (bytes fill-pointer allocated-count) (draw-list-vertex-array draw-list)
	(let ((reqd-size (+ vertex-count (cl:the fixnum fill-pointer))))
	  (declare (type fixnum reqd-size))
	  (when (<= reqd-size (cl:the fixnum allocated-count))
	    (return))
	  (let ((new-count allocated-count))
	    (declare (type fixnum new-count))
	    (tagbody
	     try-again
	       (setq new-count (* 2 new-count))
	       (if (<= reqd-size new-count)
		   (go exit)
		   (go try-again))
	     exit
	       (let ((vertex-type-size-in-uints (ash vertex-type-size -2)))
		 (declare (type fixnum vertex-type-size-in-uints))
		 (let* ((old-lisp-array bytes)
			(new-lisp-array (make-array (* (cl:the (integer 0 #.(ash most-positive-fixnum -9)) new-count)
						       (cl:the (integer 0 512) vertex-type-size-in-uints))
						    :element-type (array-element-type old-lisp-array))))
		   (unless (zerop fill-pointer)
		     #+sbcl(sb-sys:with-pinned-objects (new-lisp-array old-lisp-array)
			     (let ((new-lisp-array-ptr (sb-sys:vector-sap new-lisp-array))
				   (old-lisp-array-ptr (sb-sys:vector-sap old-lisp-array)))
			       (vk::memcpy new-lisp-array-ptr old-lisp-array-ptr
					   (cl:the fixnum (* (cl:the (integer 0 #.(ash most-positive-fixnum -9)) fill-pointer)
							     (cl:the (integer 0 512) vertex-type-size))))))
		     #+CCL
		     (ccl::%copy-ivector-to-ivector old-lisp-array 0 new-lisp-array 0
						    (cl:the fixnum (* (cl:the (integer 0 #.(ash most-positive-fixnum -9)) fill-pointer)
								      (cl:the (integer 0 512) vertex-type-size)))))
		   (setf bytes new-lisp-array)
		   (setf allocated-count new-count)
		   (setq res t))))))))
    (block nil
      (with-slots (bytes fill-pointer allocated-count) (draw-list-index-array draw-list)
	(let ((reqd-size (+ index-count (cl:the fixnum fill-pointer))))
	  (declare (type fixnum reqd-size))
	  (when (<= reqd-size (cl:the fixnum allocated-count))
	    (return))
	  (let ((new-count allocated-count))
	    (declare (type fixnum new-count))
	    (tagbody
	     try-again
	       (setq new-count (* 2 new-count))
	       (if (<= reqd-size new-count)
		   (go exit)
		   (go try-again))
	     exit
	       (let* ((old-lisp-array bytes)
		      (new-lisp-array (make-array (cl:the fixnum new-count)
						  :element-type (array-element-type old-lisp-array))))
		 (unless (zerop fill-pointer)
		   #+sbcl(sb-sys:with-pinned-objects (new-lisp-array old-lisp-array)
			   (let ((new-lisp-array-ptr (sb-sys:vector-sap new-lisp-array))
				 (old-lisp-array-ptr (sb-sys:vector-sap old-lisp-array)))
			     (vk::memcpy new-lisp-array-ptr old-lisp-array-ptr
					 (cl:the fixnum (* (cl:the (integer 0 #.(ash most-positive-fixnum -2)) fill-pointer)
							   index-type-size)))))
		   #+CCL
		   (ccl::%copy-ivector-to-ivector old-lisp-array 0 new-lisp-array 0
						  (cl:the fixnum (* (cl:the (integer 0 #.(ash most-positive-fixnum -2)) fill-pointer)
								    index-type-size))))
		 (setf bytes new-lisp-array)
		 (setf allocated-count new-count)
		 (setq res t)))))))
    res))
