(in-package :krma)

;;(declaim (optimize (safety 0) (speed 3) (debug 3)))

(defmacro with-draw-list-transaction ((draw-list first-index initial-vtx-offset)
				      &body body)
  (let ((draw-list-sym (gensym)))
    `(let ((,draw-list-sym ,draw-list))
       (handler-case
	   (progn ,@body)
	 (error (c)
	   (warn (princ-to-string c))
	   ;;(break)
	   (setf (foreign-array-fill-pointer (draw-list-index-array ,draw-list-sym)) ,first-index
		 (foreign-array-fill-pointer (draw-list-vertex-array ,draw-list-sym)) ,initial-vtx-offset)
	   nil)))))

(defun %prim-reserve (draw-list vertex-count index-count vertex-type-size index-type-size)
  ;; prim-reserve [potentially] allocates memory on the draw-list for indices and vertices
  ;; you can use prim-reserve when seq-vertices is an array
  ;; you can use index-array-set and vertex-array-set instead of
  ;; index-array-push-extend and ...-vertex-array-push-extend
  ;; and optimize out the incf of the fill pointer, if you were so anal retentive  
  (declare (type fixnum vertex-count index-count vertex-type-size index-type-size))
  ;; todo: make sure arguments passed into this function:
  ;; vertex-count * vertex-type-size < most-positive-fixnum
  ;; index-count * index-type-size < most-positive-fixnum
  (let ((res nil))
    (with-slots (ptr fill-pointer allocated-count) (draw-list-vertex-array draw-list)
      (let ((reqd-size (+ vertex-count (cl:the fixnum fill-pointer))))
	(declare (type fixnum reqd-size))
	;; fill-pointer will always be less than or equal to allocated-count
	#+NIL(assert (< (cl:the fixnum fill-pointer) (cl:the fixnum
							(- most-positive-fixnum
							   (cl:the fixnum (* vertex-count vertex-type-size))))))
        (unless (<= reqd-size (cl:the fixnum allocated-count))
          (let ((new-count allocated-count))
	    (declare (type fixnum new-count))
            (tagbody
             try-again
	       ;; don't let new-count overflow (that's alot of memory on a 64 bit machine!)
	       (assert (< new-count #.(/ most-positive-fixnum 2)))
               (setq new-count (* 2 new-count))
               (if (<= reqd-size new-count)
                   (go exit)
                   (go try-again))
             exit
               (let ((new-array (foreign-alloc :unsigned-char :count (* new-count vertex-type-size)))
                     (old-array ptr))
                 (memcpy new-array old-array
			 (cl:the fixnum (* (cl:the fixnum fill-pointer) vertex-type-size)))
                 (setf ptr new-array)
                 (setf allocated-count new-count)
                 (foreign-free old-array)
                 (setq res t)))))))

    (with-slots (ptr fill-pointer allocated-count) (draw-list-index-array draw-list)
      (let ((reqd-size (+ index-count (cl:the fixnum fill-pointer))))
	(declare (type fixnum reqd-size))
	#+NIL(assert (< (cl:the fixnum fill-pointer) (cl:the fixnum
							(- most-positive-fixnum
							   (cl:the fixnum (* index-count index-type-size))))))
        (unless (<= reqd-size (cl:the fixnum allocated-count))
          (let ((new-count allocated-count))
	    (declare (type fixnum new-count))
            (tagbody
             try-again
	       (assert (< new-count #.(/ most-positive-fixnum 2)))
               (setq new-count (* 2 new-count))
               (if (<= reqd-size new-count)
                   (go exit)
                   (go try-again))
             exit
               (let ((new-array (foreign-alloc :unsigned-char :count (* new-count index-type-size)))
                     (old-array ptr))
                 (memcpy new-array old-array (cl:the fixnum (* (cl:the fixnum fill-pointer) index-type-size)))
                 (setf ptr new-array)
                 (setf allocated-count new-count)
                 (foreign-free old-array)
                 (setq res t)))))))
    res))

;; whether we use a draw-index style of drawing points
;; or just a 'draw' style of drawing points depends on
;; how we want to delete them.
;; this function returns the index (n) of the vertex-index
;; of the vertex in the vertex-array of the point
;; so if you want to delete the point incrementally
;; repack the index array without the nth index, you can leave the vertex-array alone
;; so the returned 'n' is your reciept to deal with that point in the future
;; another possibility would be to use a "draw" command instead of a
;; "draw-indexed" command where you would have to delete the point
;; from the vertex-array based on comparing the coordinates (and repack the vertex array)
;; I don't think that is as efficient as using an index array, because of repetative compare ops
;; To "recompact" this draw-list, re-make a vertex array, minus all the 'n' vertices
;; and write the index array as a continuous sequence of non-negative integers
;; this is intended to be drawn in one draw-indexed call
;; so you can't have more than 65536 points for the small draw-list
;; the large draw list would be used if you're drawing starts in a simulated sky or something
;; in which case you might want a different vertex type with point size
(defun %draw-list-add-2d-point-pseudo-cmd (2d-draw-list ub32-color sf-x sf-y)
  ;; for point-list-pipeline
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type single-float sf-x sf-y))
  (with-slots (vertex-array index-array) 2d-draw-list
    (let ((first-index (foreign-array-fill-pointer index-array))
          (vtx-offset (foreign-array-fill-pointer vertex-array)))
      (with-draw-list-transaction (2d-draw-list first-index vtx-offset)
	(let ((index (standard-2d-vertex-array-push-extend vertex-array sf-x sf-y ub32-color)))
	  (list* 2d-draw-list (index-array-push-extend index-array index)))))))

(defun %draw-list-add-2d-point-cmd (2d-draw-list model-mtx sf-point-size ub32-color sf-x sf-y)
  ;; for point-list-pipeline
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type single-float sf-x sf-y))
  (with-slots (vertex-array index-array) 2d-draw-list
    (let ((first-index (foreign-array-fill-pointer index-array))
          (vtx-offset (foreign-array-fill-pointer vertex-array)))
      (with-draw-list-transaction (2d-draw-list first-index vtx-offset)
	(standard-2d-vertex-array-push-extend vertex-array sf-x sf-y ub32-color)
	(index-array-push-extend index-array 0)
	(let ((cmd (make-standard-draw-indexed-cmd
		    2d-draw-list
		    first-index 1 vtx-offset
		    model-mtx nil *white-texture* sf-point-size nil nil)))
	  (vector-push-extend cmd (draw-list-cmd-vector 2d-draw-list))
	  cmd)))))

(defun %draw-list-add-3d-point-pseudo-cmd (3d-draw-list ub32-color sf-x sf-y sf-z)
  ;; for point-list pipeline
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type single-float sf-x sf-y sf-z))		 
  (with-slots (vertex-array index-array) 3d-draw-list
    (let ((first-index (foreign-array-fill-pointer index-array))
          (vtx-offset (foreign-array-fill-pointer vertex-array)))
      (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
          (let ((index (standard-3d-vertex-array-push-extend vertex-array sf-x sf-y sf-z ub32-color)))
            (list 3d-draw-list (index-array-push-extend index-array index)))))))

(defun %draw-list-add-3d-point-cmd (3d-draw-list model-mtx sf-point-size ub32-color sf-x sf-y sf-z)
  ;; for point-list-pipeline
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type single-float sf-x sf-y))
  (with-slots (vertex-array index-array) 3d-draw-list
    (let ((first-index (foreign-array-fill-pointer index-array))
          (vtx-offset (foreign-array-fill-pointer vertex-array)))
      (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
	(standard-3d-vertex-array-push-extend vertex-array sf-x sf-y sf-z ub32-color)
	(index-array-push-extend index-array 0)
	(let ((cmd (make-standard-draw-indexed-cmd 
		    3d-draw-list
		    first-index 1 vtx-offset
		    model-mtx nil *white-texture* sf-point-size nil nil)))
	  (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
	  cmd)))))

;; A [2d|3d]-line-list-draw-list is intended to be processed in one cmd
;; so the index-array ends up being a increasing list of non-negative integers
;; when the index-array is not fragmented (meaining no lines have been deleted)
;; then the indices happen to be consecutive
;; since it is processed in one cmd the number of vertices for a small-draw-list cannot exceed 65536
;; and the number of vertices for the large-draw-list cannot exceed (expt 2 32)
;; since it's a single cmd it should be faster than line strips but take a bigger vertex-array
;; the indexes returned by the draw function will be the "reciepts"
;; for deleting the line.  If you delete indices you *must* repack the index-array
;; but don't need to necessarily repack the vertex array
;; you can use multiple line-list-draw-lists if you need more lines than 32768 for the small
;; and (expt 2 31) for the large

;; draw-2d-line-internal returns a cons of index (index-indexes) for a "receipt"
(defun %draw-list-add-2d-line-pseudo-cmd (2d-draw-list ub32-color sf-x0 sf-y0 sf-x1 sf-y1)
  ;; for line-list pipeline
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type single-float sf-x0 sf-y0 sf-x1 sf-y1))
  (let* ((ia (draw-list-index-array 2d-draw-list))
         (va (draw-list-vertex-array 2d-draw-list))
         (first-index (foreign-array-fill-pointer ia))
         (vtx-offset (foreign-array-fill-pointer va)))
    (with-draw-list-transaction (2d-draw-list first-index vtx-offset)
      (let ((offset0 (standard-2d-vertex-array-push-extend va sf-x0 sf-y0 ub32-color))
	    (offset1 (standard-2d-vertex-array-push-extend va sf-x1 sf-y1 ub32-color)))
	(list 2d-draw-list
	      (index-array-push-extend ia offset0)
	      (index-array-push-extend ia offset1))))))

(defun %draw-list-add-2d-line-cmd (2d-draw-list model-mtx sf-line-thickness ub32-color sf-x0 sf-y0 sf-x1 sf-y1)
  ;; for line-list pipeline
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (let* ((ia (draw-list-index-array 2d-draw-list))
         (va (draw-list-vertex-array 2d-draw-list))
         (first-index (foreign-array-fill-pointer ia))
         (vtx-offset (foreign-array-fill-pointer va)))
    (with-draw-list-transaction (2d-draw-list first-index vtx-offset)
      (let ((offset0 (standard-2d-vertex-array-push-extend va sf-x0 sf-y0 ub32-color))
	    (offset1 (standard-2d-vertex-array-push-extend va sf-x1 sf-y1 ub32-color)))
	(index-array-push-extend ia offset0)
	(index-array-push-extend ia offset1)
	(let ((cmd (make-standard-draw-indexed-cmd
		    2d-draw-list
		    first-index 2 vtx-offset
		    model-mtx nil *white-texture* nil sf-line-thickness nil)))
	  (vector-push-extend cmd (draw-list-cmd-vector 2d-draw-list))
	  cmd)))))

(defun %draw-list-add-3d-line-pseudo-cmd (3d-draw-list ub32-color sf-x0 sf-y0 sf-z0 sf-x1 sf-y1 sf-z1)
  ;; for line-list pipeline
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type single-float sf-x0 sf-y0 sf-z0 sf-x1 sf-y1 sf-z1))
  (let* ((ia (draw-list-index-array 3d-draw-list))
         (va (draw-list-vertex-array 3d-draw-list))
         (first-index (foreign-array-fill-pointer ia))
         (vtx-offset (foreign-array-fill-pointer va)))
    (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
      (let ((offset0 (standard-3d-vertex-array-push-extend va sf-x0 sf-y0 sf-z0 ub32-color))
	    (offset1 (standard-3d-vertex-array-push-extend va sf-x1 sf-y1 sf-z1 ub32-color)))
	(list 3d-draw-list
	      (index-array-push-extend ia offset0)
	      (index-array-push-extend ia offset1))))))

(defun %draw-list-add-3d-line-cmd (3d-draw-list model-mtx sf-line-thickness ub32-color
				   sf-x0 sf-y0 sf-z0 sf-x1 sf-y1 sf-z1)
  ;; for line-list pipeline
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (let* ((ia (draw-list-index-array 3d-draw-list))
         (va (draw-list-vertex-array 3d-draw-list))
         (first-index (foreign-array-fill-pointer ia))
         (vtx-offset (foreign-array-fill-pointer va)))
    (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
      (standard-3d-vertex-array-push-extend va sf-x0 sf-y0 sf-z0 ub32-color)
      (standard-3d-vertex-array-push-extend va sf-x1 sf-y1 sf-z1 ub32-color)
      (index-array-push-extend ia 0)
      (index-array-push-extend ia 1)
      (let ((cmd (make-standard-draw-indexed-cmd
		  3d-draw-list
		  first-index 2 vtx-offset
		  model-mtx nil *white-texture* nil sf-line-thickness nil)))
	(vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
	cmd))))

(defun %draw-list-add-2d-polyline-pseudo-cmd (2d-draw-list bool-closed? ub32-color seq-vertices)
  ;; for line-list pipeline
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type boolean bool-closed?))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type sequence seq-vertices))
  ;; there must be at least one vertex to succeed
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
	 (vtx-offset (foreign-array-fill-pointer vertex-array)))
    (declare (type fixnum vtx-offset))
    (with-draw-list-transaction (2d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x1 y1) on (cddr seq-vertices) by #'cddr
	       for (x0 y0) on seq-vertices by #'cddr
	       with indices = ()
	       do (setq x0 (clampf x0))
		  (setq y0 (clampf y0))
		  (setq x1 (clampf x1))
		  (setq y1 (clampf y1))
		  (let ((offset (standard-2d-vertex-array-push-extend vertex-array x0 y0 ub32-color)))
		    (push (index-array-push-extend index-array offset) indices))
		  (let ((offset (standard-2d-vertex-array-push-extend vertex-array x1 y1 ub32-color)))
		    (push (index-array-push-extend index-array offset) indices))
	       finally (when bool-closed?
			 (push (index-array-push-extend index-array vtx-offset) indices))
		       (return (list* 2d-draw-list indices))))))))

(defun %draw-list-add-2d-triangle-pseudo-cmd (2d-draw-list ub32-color sf-x0 sf-y0 sf-x1 sf-y1 sf-x2 sf-y2)
  ;; for line-list pipeline
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type single-float sf-x0 sf-y0 sf-x1 sf-y1 sf-x2 sf-y2))
  (%draw-list-add-2d-polyline-pseudo-cmd 2d-draw-list t ub32-color (list sf-x0 sf-y0 sf-x1 sf-y1 sf-x2 sf-y2)))

(defun %draw-list-add-2d-rectangle-pseudo-cmd (2d-draw-list ub32-color sf-x0 sf-y0 sf-x1 sf-y1)
  ;; for line-list pipeline
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type single-float sf-x0 sf-y0 sf-x1 sf-y1))
  (%draw-list-add-2d-polyline-pseudo-cmd 2d-draw-list t ub32-color
					 (list sf-x0 sf-y0 sf-x0 sf-y1 sf-x1 sf-y1 sf-x1 sf-y0)))

;; the argument `vertices' is a list of x y ... repeating
;; for single color polylines
(defun %draw-list-add-2d-polyline-cmd (2d-draw-list model-mtx bool-closed?
				       sf-line-thickness ub32-color seq-vertices)
  ;; for line-strip pipeline
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
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
    (with-draw-list-transaction (2d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x y) on seq-vertices by #'cddr
               do (setq x (clampf x))
		  (setq y (clampf y))
		  (standard-2d-vertex-array-push-extend vertex-array x y ub32-color)
                  (index-array-push-extend index-array elem-count)
                  (incf elem-count)
               finally (when bool-closed?
                         (index-array-push-extend index-array 0)
                         (incf elem-count)))))
          (let ((cmd (make-standard-draw-indexed-cmd
                              2d-draw-list
                              first-index elem-count vtx-offset
                              model-mtx nil *white-texture* nil sf-line-thickness nil)))
            (vector-push-extend cmd (draw-list-cmd-vector 2d-draw-list))
            cmd))))

(defun %draw-list-add-2d-triangle-cmd (2d-draw-list model-mtx sf-line-thickness ub32-color
				       sf-x0 sf-y0 sf-x1 sf-y1 sf-x2 sf-y2)
  ;; for line-strip pipeline
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type single-float sf-x0 sf-y0 sf-x1 sf-y1 sf-x2 sf-y2))
  (%draw-list-add-2d-polyline-cmd 2d-draw-list t model-mtx sf-line-thickness
				  ub32-color (list sf-x0 sf-y0 sf-x1 sf-y1 sf-x2 sf-y2)))

(defun %draw-list-add-2d-rectangle-cmd (2d-draw-list model-mtx sf-line-thickness ub32-color
					sf-x0 sf-y0 sf-x1 sf-y1)
  ;; for line-strip pipeline
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type single-float sf-x0 sf-y0 sf-x1 sf-y1))
  (%draw-list-add-2d-polyline-cmd 2d-draw-list t model-mtx sf-line-thickness
				  ub32-color (list sf-x0 sf-y0 sf-x0 sf-y1 sf-x1 sf-y1 sf-x1 sf-y0)))

;; the argument `seq-vertices' is a cl sequence of x y color ... repeating
(defun %draw-list-add-multicolor-2d-polyline-pseudo-cmd (2d-draw-list bool-closed? seq-vertices)
  ;; uses line-list
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type boolean bool-closed?))
  (declare (type sequence seq-vertices))
  ;; there must be at least one vertex to succeed
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array)))
    (declare (type fixnum vtx-offset))
    (with-draw-list-transaction (2d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x1 y1 color1) on (cdddr seq-vertices) by #'cdddr
	       for (x0 y0 color0) on seq-vertices by #'cdddr
	       with indices = ()
	       do (setq x0 (clampf x0))
		  (setq y0 (clampf y0))
		  (setq color0 (canonicalize-color color0))
		  (setq x1 (clampf x1))
		  (setq y1 (clampf y1))
		  (setq color1 (canonicalize-color color1))
		  (let ((offset (standard-2d-vertex-array-push-extend vertex-array x0 y0 color0)))
		    (push (index-array-push-extend index-array offset) indices))
		  (let ((offset (standard-2d-vertex-array-push-extend vertex-array x1 y1 color1)))
		    (push (index-array-push-extend index-array offset) indices))
	       finally (when bool-closed?
			 (push (index-array-push-extend index-array vtx-offset) indices))
		       (return (list* 2d-draw-list indices))))))))

(defun %draw-list-add-multicolor-2d-polyline-cmd (2d-draw-list model-mtx bool-closed? sf-line-thickness
						  seq-vertices)
  ;; uses line-strip
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
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
    (with-draw-list-transaction (2d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x y color) on seq-vertices by #'cdddr
	       do (setq x (clampf x))
		  (setq y (clampf y))
		  (setq color (canonicalize-color color))
		  (standard-2d-vertex-array-push-extend vertex-array x y color)
		  (index-array-push-extend index-array elem-count)
		  (incf elem-count)
	       finally (when bool-closed?
			 (index-array-push-extend index-array 0)
			 (incf elem-count)))))
	  (let ((cmd (make-standard-draw-indexed-cmd
		      2d-draw-list
		      first-index elem-count vtx-offset
		      model-mtx nil *white-texture* nil sf-line-thickness nil)))
            (vector-push-extend cmd cmd-vector)
            cmd))))

(defun %draw-list-add-2d-line-list-pseudo-cmd (2d-draw-list ub32-color seq-vertices)
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type sequence seq-vertices))
  (let* ((index-array (draw-list-index-array 2d-draw-list))
	 (vertex-array (draw-list-vertex-array 2d-draw-list)))
    (declare (type foreign-adjustable-array index-array vertex-array))
    (let ((first-index (foreign-array-fill-pointer index-array))
	  (vtx-offset (foreign-array-fill-pointer vertex-array)))
      (with-draw-list-transaction (2d-draw-list first-index vtx-offset)
	(etypecase seq-vertices
	  (list
	   (when (cdddr seq-vertices)
	     (loop for (x0 y0 x1 y1) on seq-vertices by #'cddddr
		   with indices = ()
		   do (setq x0 (clampf x0))
		      (setq y0 (clampf y0))
		      (setq x1 (clampf x1))
		      (setq y1 (clampf y1))
		      (let ((offset (standard-2d-vertex-array-push-extend vertex-array x0 y0 ub32-color)))
			(push (index-array-push-extend index-array offset) indices))
		      (let ((offset (standard-2d-vertex-array-push-extend vertex-array x1 y1 ub32-color)))
			(push (index-array-push-extend index-array offset) indices))
		   finally (return (list* 2d-draw-list indices))))))))))

(defun %draw-list-add-2d-line-list-cmd (2d-draw-list model-mtx sf-line-thickness ub32-color seq-vertices)
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type sequence seq-vertices))
  (let* ((index-array (draw-list-index-array 2d-draw-list))
	 (vertex-array (draw-list-vertex-array 2d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
	 (cmd-vector (draw-list-cmd-vector 2d-draw-list))
	 (elem-count -1))
    (with-draw-list-transaction (2d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (when (cdddr seq-vertices)
	   (loop for (x0 y0 x1 y1) on seq-vertices by #'cddddr
		 with indices = ()
		 do (setq x0 (clampf x0))
		    (setq y0 (clampf y0))
		    (setq x1 (clampf x1))
		    (setq y1 (clampf y1))
		    (standard-2d-vertex-array-push-extend vertex-array x0 y0 ub32-color)
		    (index-array-push-extend index-array (incf elem-count))
		    (standard-2d-vertex-array-push-extend vertex-array x1 y1 ub32-color)
		    (index-array-push-extend index-array (incf elem-count))))))
	  (let ((cmd (make-standard-draw-indexed-cmd
		      2d-draw-list
		      first-index elem-count vtx-offset
		      model-mtx nil *white-texture* nil sf-line-thickness nil)))
            (vector-push-extend cmd cmd-vector)
            cmd))))

(defun %draw-list-add-2d-circular-arc-pseudo-cmd (2d-draw-list bool-closed? ub32-color
						  df-center-x df-center-y df-radius df-start-angle df-end-angle
						  fixnum-number-of-segments)
  ;; uses line list instead of line strip
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type boolean bool-closed?))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type double-float df-center-x df-center-y df-radius df-start-angle df-end-angle))
  (declare (type fixnum fixnum-number-of-segments))

  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array)))
    (declare (type fixnum vtx-offset))
    (with-draw-list-transaction (2d-draw-list first-index vtx-offset)
      (let* ((dtheta (- df-start-angle df-end-angle)))
	(declare (type double-float dtheta))
	(standard-2d-vertex-array-push-extend vertex-array
                                              (clampf (+ df-center-x (* df-radius (cos df-start-angle))))
                                              (clampf (+ df-center-y (* df-radius (sin df-start-angle))))
                                              ub32-color)
	(loop for i from vtx-offset
              repeat fixnum-number-of-segments
              with theta = df-start-angle
              with step = (/ dtheta fixnum-number-of-segments)
              do (let* ((angle (+ (cl:the double-float theta) (cl:the double-float step)))
			(coord-x (clampf (+ df-center-x (* df-radius (cos angle)))))
			(coord-y (clampf (+ df-center-y (* df-radius (sin angle))))))
                   (index-array-push-extend index-array i)
                   (standard-2d-vertex-array-push-extend vertex-array coord-x coord-y ub32-color)
                   (index-array-push-extend index-array (1+ (cl:the fixnum i)))
                   (incf theta step))
              finally (when bool-closed?
			(index-array-push-extend index-array vtx-offset)))))))


(defun %draw-list-add-2d-circular-arc-cmd (2d-draw-list model-mtx bool-closed? sf-line-thickness ub32-color
                                           df-center-x df-center-y df-radius df-start-angle df-end-angle
                                           fixnum-number-of-segments
                                           &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
  ;; for use with line strip pipeline
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type function cmd-constructor))
  (declare (type boolean bool-closed?))
  (declare (type single-float sf-line-thickness))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type double-float df-center-x df-center-y df-radius df-start-angle df-end-angle))
  (declare (type fixnum fixnum-number-of-segments))
  
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (first-index (foreign-array-fill-pointer index-array))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array)))

    (with-draw-list-transaction (2d-draw-list first-index vertex-array)
      (unless (or (minusp df-radius) (minusp fixnum-number-of-segments))
        (let* ((dtheta (- df-start-angle df-end-angle)))
          (loop for i from 0 to fixnum-number-of-segments
                with theta = df-start-angle
                with step = (/ dtheta fixnum-number-of-segments)
                do (let* ((coord-x (clampf
				    (+ df-center-x (* df-radius (cos (cl:the double-float theta))))))
                          (coord-y (clampf
				    (+ df-center-y (* df-radius (sin (cl:the double-float theta)))))))
		     (standard-2d-vertex-array-push-extend vertex-array coord-x coord-y ub32-color)
		     (index-array-push-extend index-array i)
		     (incf (cl:the double-float theta) step))
                finally (when bool-closed?
                          (index-array-push-extend index-array 0)
			  (incf fixnum-number-of-segments)))
          (let ((cmd (funcall cmd-constructor
                              2d-draw-list
                              first-index (1+ fixnum-number-of-segments) vtx-offset
                              model-mtx nil *white-texture* sf-line-thickness nil)))
	    (vector-push-extend cmd (draw-list-cmd-vector 2d-draw-list))
	    cmd))))))

(defun %draw-list-add-2d-circle-pseudo-cmd (2d-draw-list ub32-color
					    df-center-x df-center-y df-radius
					    fixnum-number-of-segments)
  ;; uses line list instead of line strip
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type double-float df-center-x df-center-y df-radius))
  (declare (type fixnum fixnum-number-of-segments))
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array)))
    (declare (type fixnum vtx-offset))
    (with-draw-list-transaction (2d-draw-list first-index vtx-offset)
      (standard-2d-vertex-array-push-extend vertex-array
                                            (clampf (+ df-center-x (* df-radius #.(cos 0))))
                                            (clampf (+ df-center-y (* df-radius #.(sin 0))))
                                            ub32-color)
      (loop for i from vtx-offset
            repeat fixnum-number-of-segments
            with theta = 0.0d0
            with step = (/ 2pi fixnum-number-of-segments)
	    with indices = ()
            do (let* ((angle (+ (cl:the double-float theta) (cl:the double-float step)))
		      (coord-x (clampf (+ df-center-x (* df-radius (cos angle)))))
                      (coord-y (clampf (+ df-center-y (* df-radius (sin angle))))))
		 (push (index-array-push-extend index-array i) indices)
		 (let ((offset (standard-2d-vertex-array-push-extend vertex-array coord-x coord-y ub32-color)))
		   (push (index-array-push-extend index-array offset) indices))
		 (incf theta step))
	    finally (return (list* 2d-draw-list indices))))))

(defun %draw-list-add-2d-circle-cmd (2d-draw-list model-mtx sf-line-thickness ub32-color
                                     df-center-x df-center-y df-radius fixnum-number-of-segments
                                     &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type function cmd-constructor))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type double-float df-center-x df-center-y df-radius))
  (declare (type fixnum fixnum-number-of-segments))

  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (first-index (foreign-array-fill-pointer index-array))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array)))
    (with-draw-list-transaction (2d-draw-list first-index vtx-offset)
      (unless (minusp df-radius)
        (loop for i from 0 to fixnum-number-of-segments
              with theta = 0.0d0
              with step = (/ 2pi fixnum-number-of-segments)
              do (let* ((coord-x (clampf (+ df-center-x (* df-radius (cos (cl:the double-float theta))))))
                        (coord-y (clampf (+ df-center-y (* df-radius (sin (cl:the double-float theta)))))))
                   (standard-2d-vertex-array-push-extend vertex-array coord-x coord-y ub32-color)
                   (index-array-push-extend index-array i)
                   (incf theta step))
              finally (index-array-push-extend index-array 0))
        (let ((cmd (funcall cmd-constructor
                            2d-draw-list
                            first-index (1+ fixnum-number-of-segments) vtx-offset
                            model-mtx nil sf-line-thickness nil)))
          (vector-push-extend cmd (draw-list-cmd-vector 2d-draw-list))
          cmd)))))

(defun %draw-list-add-3d-polyline-pseudo-cmd (3d-draw-list bool-closed? ub32-color seq-vertices)
  ;; for line-list pipeline
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type boolean bool-closed?))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type sequence seq-vertices))
  ;; there must be at least one vertex to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array)))
    (declare (type fixnum vtx-offset))
    (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x1 y1 z1) on (cdddr seq-vertices) by #'cdddr
	       for (x0 y0 z0) on seq-vertices by #'cdddr
	       with indices = ()
	       do (setq x0 (clampf x0))
		  (setq y0 (clampf y0))
		  (setq z0 (clampf z0))
		  (setq x1 (clampf x1))
		  (setq y1 (clampf y1))
		  (setq z1 (clampf z1))
		  (let ((offset (standard-3d-vertex-array-push-extend vertex-array x0 y0 z0 ub32-color)))
		    (push (index-array-push-extend index-array offset) indices))
		  (let ((offset (standard-3d-vertex-array-push-extend vertex-array x1 y1 z1 ub32-color)))
		    (push (index-array-push-extend index-array offset) indices))
	       finally (when bool-closed?
			 (push (index-array-push-extend index-array vtx-offset) indices))
		       (return (list* 3d-draw-list indices))))))))

(defun %draw-list-add-3d-polyline-cmd (3d-draw-list model-mtx bool-closed? sf-line-thickness ub32-color
				       seq-vertices
				       &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
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
    (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x y z) on seq-vertices by #'cdddr
	       do (setq x (clampf x))
		  (setq y (clampf y))
		  (setq z (clampf z))
		  (standard-3d-vertex-array-push-extend vertex-array x y z ub32-color)
		  (index-array-push-extend index-array elem-count)
		  (incf elem-count)
	       finally (when bool-closed?
			 (index-array-push-extend index-array 0)
			 (incf elem-count))))
	(array
	 (let ((len-vertices (length seq-vertices)))
	   (declare (type fixnum len-vertices))
	   (loop for i from 0 to (- len-vertices 3) by 3
		 do (let ((x (clampf (aref seq-vertices i)))
			  (y (clampf (aref seq-vertices (1+ i))))
			  (z (clampf (aref seq-vertices (+ i 2)))))
		      (standard-3d-vertex-array-push-extend vertex-array x y z ub32-color)
		      (index-array-push-extend index-array elem-count))
		 finally (incf elem-count (/ len-vertices 3))
			 (when bool-closed?
			   (index-array-push-extend index-array 0)
			   (incf elem-count))))))
          (let ((cmd (funcall cmd-constructor
                              3d-draw-list
                              first-index elem-count vtx-offset
                              model-mtx nil sf-line-thickness nil)))
            (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
            cmd))))

(defun %draw-list-add-multicolor-3d-polyline-pseudo-cmd (3d-draw-list bool-closed? seq-vertices)
  ;; uses line-list
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type boolean bool-closed?))
  (declare (type sequence seq-vertices))
  ;; there must be at least one vertex to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array)))
    (declare (type fixnum vtx-offset))
    (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x1 y1 z1 color1) on (cddddr seq-vertices) by #'cddddr
	       for (x0 y0 z0 color0) on seq-vertices by #'cddddr
	       with indices = ()
	       do (setq x0 (clampf x0))
		  (setq y0 (clampf y0))
		  (setq z0 (clampf z0))
		  (setq color0 (canonicalize-color color0))
		  (setq x1 (clampf x1))
		  (setq y1 (clampf y1))
		  (setq z1 (clampf z1))
		  (setq color1 (canonicalize-color color1))
		  (let ((offset (standard-3d-vertex-array-push-extend vertex-array x0 y0 z0 color0)))
		    (push (index-array-push-extend index-array offset) indices))
		  (let ((offset (standard-3d-vertex-array-push-extend vertex-array x1 y1 z1 color1)))
		    (push (index-array-push-extend index-array offset) indices))
	       finally (when bool-closed?
			 (push (index-array-push-extend index-array vtx-offset) indices))
		       (return (list* 3d-draw-list indices))))))))

(defun %draw-list-add-multicolor-3d-polyline-cmd
    (3d-draw-list model-mtx bool-closed? sf-line-thickness seq-vertices
     &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
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
    (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x y z color) on seq-vertices by #'cddddr
	       do (setq x (clampf x))
		  (setq y (clampf y))
		  (setq z (clampf z))
		  (setq color (canonicalize-color color))
		  (standard-3d-vertex-array-push-extend vertex-array x y z color)
		  (index-array-push-extend index-array elem-count)
		  (incf elem-count)
	       finally (when bool-closed?
			 (index-array-push-extend index-array 0)
			 (incf elem-count)))))
          (let ((cmd (funcall cmd-constructor
                              3d-draw-list
                              first-index elem-count vtx-offset
                              model-mtx nil sf-line-thickness nil)))
            (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
            cmd))))

(defun %draw-list-add-filled-2d-triangle-list-pseudo-cmd (2d-draw-list ub32-color seq-vertices)
  ;; triangle list
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type sequence seq-vertices))
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (2d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x y) on seq-vertices by #'cddr
	       for i from vtx-offset
	       with indices = ()
	       do (setq x (clampf x))
		  (setq y (clampf y))
		  (standard-2d-vertex-array-push-extend vertex-array x y ub32-color)
		  (incf number-of-vertices)
		  (push (index-array-push-extend index-array i) indices)
	       finally (return (list* 2d-draw-list indices))))))))

(defun %draw-list-add-filled-2d-triangle-list-cmd (2d-draw-list model-mtx ub32-color seq-vertices)
  ;; triangle list
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type sequence seq-vertices))
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (elem-count 0))
    (declare (type fixnum elem-count))
    (with-draw-list-transaction (2d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x0 y0 x1 y1 x2 y2) on seq-vertices by #'(lambda (list)
							      (nthcdr 6 list))
               for i from 0 by 3 below #.(- most-positive-fixnum 4)
               do (setq x0 (clampf x0))
		  (setq y0 (clampf y0))
		  (setq x1 (clampf x1))
		  (setq y1 (clampf y1))
		  (setq x2 (clampf x2))
		  (setq y2 (clampf y2))
		  (standard-2d-vertex-array-push-extend vertex-array x0 y0 ub32-color)
		  (index-array-push-extend index-array i)
                  (standard-2d-vertex-array-push-extend vertex-array x1 y1 ub32-color)
		  (index-array-push-extend index-array (1+ i))
                  (standard-2d-vertex-array-push-extend vertex-array x2 y2 ub32-color)
                  (index-array-push-extend index-array (+ i 2))
                  (incf elem-count 3))))
          (let ((cmd (make-standard-draw-indexed-cmd
		      2d-draw-list
		      first-index elem-count vtx-offset model-mtx
		      nil *white-texture* nil nil nil)))
            (vector-push-extend cmd (draw-list-cmd-vector 2d-draw-list))
            cmd))))

(defun %draw-list-add-filled-2d-triangle-strip/list-cmd (2d-draw-list model-mtx ub32-color seq-vertices)
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type sequence seq-vertices))
  ;; must be at least three vertices to succeed
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (2d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list (loop for (x y) on seq-vertices by #'cddr
		    for i from 0 below #.most-positive-fixnum
		    do (setq x (clampf x))
		       (setq y (clampf y))
		       (standard-2d-vertex-array-push-extend vertex-array x y ub32-color)
		       (index-array-push-extend index-array i)
		       (incf number-of-vertices))))
	  (assert (> number-of-vertices 3))
          (let ((cmd (make-standard-draw-indexed-cmd
		      2d-draw-list
		      first-index number-of-vertices vtx-offset model-mtx
		      nil *white-texture* nil nil nil)))
            (vector-push-extend cmd (draw-list-cmd-vector 2d-draw-list))
            cmd))))

(defun %draw-list-add-filled-2d-rectangle-list-pseudo-cmd (2d-draw-list ub32-color seq-vertices)
  ;; triangle list
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type sequence seq-vertices))
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
         (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (elem-count 0))
    (declare (type fixnum elem-count))
    (with-draw-list-transaction (2d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x0 y0 x1 y1) on seq-vertices by #'(lambda (list)
							(nthcdr 4 list))
               for i from vtx-offset by 4
	       with indices = ()
               do (setq x0 (clampf x0))
		  (setq y0 (clampf y0))
		  (setq x1 (clampf x1))
		  (setq y1 (clampf y1))
		  (standard-2d-vertex-array-push-extend vertex-array x0 y0 ub32-color)
                  (standard-2d-vertex-array-push-extend vertex-array x0 y1 ub32-color)
                  (standard-2d-vertex-array-push-extend vertex-array x1 y1 ub32-color)
                  (standard-2d-vertex-array-push-extend vertex-array x1 y0 ub32-color)
                  (push (index-array-push-extend index-array i) indices)
                  (push (index-array-push-extend index-array (1+ i)) indices)
                  (push (index-array-push-extend index-array (+ 2 i)) indices)
                  (push (index-array-push-extend index-array i) indices)
                  (push (index-array-push-extend index-array (+ 2 i)) indices)
                  (push (index-array-push-extend index-array (+ 3 i)) indices)
                  (incf elem-count 6)
	       finally (list* 2d-draw-list indices)))))))

(defun %draw-list-add-filled-2d-rectangle-list-cmd (2d-draw-list model-mtx ub32-color seq-vertices
                                                    &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
  ;; triangle-list
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type function cmd-constructor))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type sequence seq-vertices))
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (elem-count 0))
    (declare (type fixnum elem-count))
    (with-draw-list-transaction (2d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x0 y0 x1 y1) on seq-vertices by #'(lambda (list)
							(nthcdr 4 list))
               for i from 0 by 4 below #.(- most-positive-fixnum 4)
               do (setq x0 (clampf x0))
		  (setq y0 (clampf y0))
		  (setq x1 (clampf x1))
		  (setq y1 (clampf y1))
		  (standard-2d-vertex-array-push-extend vertex-array x0 y0 ub32-color)
                  (standard-2d-vertex-array-push-extend vertex-array x0 y1 ub32-color)
                  (standard-2d-vertex-array-push-extend vertex-array x1 y1 ub32-color)
                  (standard-2d-vertex-array-push-extend vertex-array x1 y0 ub32-color)
                  (index-array-push-extend index-array i)
                  (index-array-push-extend index-array (1+ i))
                  (index-array-push-extend index-array (+ 2 i))
                  (index-array-push-extend index-array i)
                  (index-array-push-extend index-array (+ 2 i))
                  (index-array-push-extend index-array (+ 3 i))
                  (incf elem-count 6))))
          (let ((cmd (funcall cmd-constructor
                              2d-draw-list
                              first-index elem-count vtx-offset model-mtx
                              nil *white-texture* nil)))
            (vector-push-extend cmd (draw-list-cmd-vector 2d-draw-list))
            cmd))))


(defun %draw-list-add-textured-2d-rectangle-list-pseudo-cmd (2d-draw-list ub32-color seq-vertices)
  ;; used to implement draw-text
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type sequence seq-vertices))
  (declare (type (unsigned-byte 32) ub32-color))
  ;; must be at least 2 vertices to succeed
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list)))
    (declare (type foreign-adjustable-array index-array vertex-array))
    (let ((first-index (foreign-array-fill-pointer index-array))
	  (vtx-offset (foreign-array-fill-pointer vertex-array)))
      (with-draw-list-transaction (2d-draw-list first-index vtx-offset)
	(etypecase seq-vertices
	  (list
	   (loop for (x0 y0 u0 v0 x1 y1 u1 v1) on seq-vertices by #'(lambda (list)
								      (nthcdr 8 list))
		 for i from 0 by 4 below #.(- most-positive-fixnum 3)
		 with indices = ()
		 do (setq x0 (clampf x0))
		    (setq y0 (clampf y0))
		    (setq u0 (clampf u0))
		    (setq v0 (clampf v0))
		    (setq x1 (clampf x1))
		    (setq y1 (clampf y1))
		    (setq u1 (clampf u1))
		    (setq v1 (clampf v1))
		    (textured-2d-vertex-array-push-extend vertex-array x0 y0 u0 v0 ub32-color)
		    (textured-2d-vertex-array-push-extend vertex-array x0 y1 u0 v1 ub32-color)
		    (textured-2d-vertex-array-push-extend vertex-array x1 y1 u1 v1 ub32-color)
		    (textured-2d-vertex-array-push-extend vertex-array x1 y0 u1 v0 ub32-color)
		    (push (index-array-push-extend index-array i) indices)
		    (push (index-array-push-extend index-array (1+ i)) indices)
		    (push (index-array-push-extend index-array (+ 2 i)) indices)
		    (push (index-array-push-extend index-array i) indices)
		    (push (index-array-push-extend index-array (+ 2 i)) indices)
		    (push (index-array-push-extend index-array (+ 3 i)) indices)
		 finally (return (list* 2d-draw-list indices)))))))))

(defun %draw-list-add-textured-2d-rectangle-list-cmd (2d-draw-list model-mtx texture ub32-color seq-vertices
                                                      &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
  ;; used to implement add-text
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type function cmd-constructor))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type sequence seq-vertices))
  ;; must be at least 2 vertices to succeed
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (elem-count 0))
    (declare (type fixnum elem-count))
    (with-draw-list-transaction (2d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x0 y0 u0 v0 x1 y1 u1 v1) on seq-vertices by #'(lambda (list)
								    (nthcdr 8 list))
               for i from 0 by 4 below #.(- most-positive-fixnum 4)
               do (setq x0 (clampf x0))
		  (setq y0 (clampf y0))
		  (setq u0 (clampf u0))
		  (setq v0 (clampf v0))
		  (setq x1 (clampf x1))
		  (setq y1 (clampf y1))
		  (setq u1 (clampf u1))
		  (setq v1 (clampf v1))
		  (textured-2d-vertex-array-push-extend vertex-array x0 y0 u0 v0 ub32-color)
                  (textured-2d-vertex-array-push-extend vertex-array x0 y1 u0 v1 ub32-color)
                  (textured-2d-vertex-array-push-extend vertex-array x1 y1 u1 v1 ub32-color)
                  (textured-2d-vertex-array-push-extend vertex-array x1 y0 u1 v0 ub32-color)
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
			  first-index elem-count vtx-offset model-mtx
			  nil texture nil nil nil)))
	(vector-push-extend cmd (draw-list-cmd-vector 2d-draw-list))
	cmd))))

(defun %draw-list-add-filled-2d-convex-polygon-pseudo-cmd (2d-draw-list ub32-color seq-vertices)
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type sequence seq-vertices))
  ;; must be at least 3 vertexes to succeed
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (2d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for i from vtx-offset below #.most-positive-fixnum
	       for (x y) on seq-vertices by #'cddr
	       with indices = ()
	       do (setq x (clampf x))
		  (setq y (clampf y))
		  (standard-2d-vertex-array-push-extend vertex-array x y ub32-color)
		  (incf number-of-vertices)
	       when (>= i 2)
		 do (push (index-array-push-extend index-array 0) indices)
		    (push (index-array-push-extend index-array (1- i)) indices)
		    (push (index-array-push-extend index-array i) indices)
	       finally (return (list* 2d-draw-list indices))))))))
      

(defun %draw-list-add-filled-2d-convex-polygon-cmd (2d-draw-list model-mtx ub32-color seq-vertices)
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type sequence seq-vertices))
  ;; must be at least 3 vertexes to succeed
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (2d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for i from 0 below #.most-positive-fixnum
	       for (x y) on seq-vertices by #'cddr
	       do (setq x (clampf x))
		  (setq y (clampf y))
		  (standard-2d-vertex-array-push-extend vertex-array x y ub32-color)
		  (incf number-of-vertices)
	       when (>= i 2)
		 do (index-array-push-extend index-array 0)
		    (index-array-push-extend index-array (1- i))
		    (index-array-push-extend index-array i))))
	  (assert (>= number-of-vertices 3))
          (let ((cmd (make-standard-draw-indexed-cmd
		      2d-draw-list
		      first-index (* 3 (- number-of-vertices 2)) vtx-offset
		      model-mtx nil *white-texture* nil nil nil)))
            (vector-push-extend cmd (draw-list-cmd-vector 2d-draw-list))
            cmd))))

(defun %draw-list-add-filled-3d-triangle-strip/list-pseudo-cmd (3d-draw-list ub32-color seq-vertices)
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type sequence seq-vertices))
  ;; must be at least three vertices to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list (loop for (x y z) on seq-vertices by #'cdddr
		    for i from vtx-offset below #.most-positive-fixnum
		    with indices = ()
		    do (setq x (clampf x))
		       (setq y (clampf y))
		       (setq z (clampf z))
		       (standard-3d-vertex-array-push-extend vertex-array x y z ub32-color)
		       (push (index-array-push-extend index-array i) indices)
		       (incf number-of-vertices)
		    finally (assert (> number-of-vertices 3))
			    (return (list* 3d-draw-list indices))))))))

(defun %draw-list-add-filled-3d-triangle-strip/list-cmd (3d-draw-list model-mtx ub32-color seq-vertices)
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type sequence seq-vertices))
  ;; must be at least three vertices to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list (loop for (x y z) on seq-vertices by #'cdddr
		    for i from 0 below #.most-positive-fixnum
		    do (setq x (clampf x))
		       (setq y (clampf y))
		       (setq z (clampf z))
		       (standard-3d-vertex-array-push-extend vertex-array x y z ub32-color)
		       (index-array-push-extend index-array i)
		       (incf number-of-vertices))))
	  (assert (> number-of-vertices 3))
          (let ((cmd (make-standard-draw-indexed-cmd
		      3d-draw-list
		      first-index number-of-vertices vtx-offset model-mtx
		      nil *white-texture* nil nil nil)))
            (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
            cmd))))

(defun %draw-list-add-filled-3d-triangle-strip/list-with-normals-pseudo-cmd (3d-draw-list ub32-color seq-vertices)
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type sequence seq-vertices))
  ;; must be at least three vertices to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array)))
    (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list (loop for (x y z nx ny nz) on seq-vertices by #'cdddr
		    for i from vtx-offset below #.most-positive-fixnum
		    with indices = ()
		    do (setq x (clampf x))
		       (setq y (clampf y))
		       (setq z (clampf z))
		       (setq nx (clampf nx))
		       (setq ny (clampf ny))
		       (setq nz (clampf nz))
		       (standard-3d-vertex-with-normal-array-push-extend vertex-array x y z nx ny nz ub32-color)
		       (push (index-array-push-extend index-array i) indices)
		    finally (return (list* 3d-draw-list indices))))))))

(defun %draw-list-add-filled-3d-triangle-strip/list-with-normals-cmd
    (3d-draw-list model-mtx ub32-color seq-vertices light-position)
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type sequence seq-vertices))
  ;; must be at least three vertices to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list (loop for (x y z nx ny nz) on seq-vertices by #'cdddr
		    for i from 0 below #.most-positive-fixnum
		    do (setq x (clampf x))
		       (setq y (clampf y))
		       (setq z (clampf z))
		       (setq nx (clampf nx))
		       (setq ny (clampf ny))
		       (setq nz (clampf nz))
		       (standard-3d-vertex-with-normal-array-push-extend vertex-array x y z nx ny nz ub32-color)
		       (incf number-of-vertices)
		       (index-array-push-extend index-array i))))
          (let ((cmd (make-standard-draw-indexed-cmd
		      3d-draw-list
		      first-index number-of-vertices vtx-offset model-mtx
		      nil *white-texture* nil nil light-position)))
            (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
            cmd))))

(defun %draw-list-add-multicolor-3d-triangle-strip/list-with-normals-pseudo-cmd (3d-draw-list seq-vertices)
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type sequence seq-vertices))
  ;; must be at least three vertices to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array)))
    (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list (loop for (x y z nx ny nz color) on seq-vertices by #'(lambda (list)
								      (nthcdr 7 list))
		    for i from vtx-offset below #.most-positive-fixnum
		    with indices = ()
		    do (setq x (clampf x))
		       (setq y (clampf y))
		       (setq z (clampf z))
		       (setq nx (clampf nx))
		       (setq ny (clampf ny))
		       (setq nz (clampf nz))
		       (setq color (canonicalize-color color))
		       (standard-3d-vertex-with-normal-array-push-extend vertex-array x y z nx ny nz color)
		       (push (index-array-push-extend index-array i) indices)
		    finally (return (list* 3d-draw-list indices))))))))

(defun %draw-list-add-multicolor-3d-triangle-strip/list-with-normals-cmd
    (3d-draw-list model-mtx seq-vertices light-position)
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type sequence seq-vertices))
  ;; must be at least three vertices to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
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
		       (standard-3d-vertex-with-normal-array-push-extend vertex-array x y z nx ny nz color)
		       (incf number-of-vertices)
		       (index-array-push-extend index-array i))))
          (let ((cmd (make-standard-draw-indexed-cmd
		      3d-draw-list
		      first-index number-of-vertices vtx-offset model-mtx
		      nil *white-texture* nil nil light-position)))
            (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
            cmd))))

(defun %draw-list-add-textured-3d-triangle-strip/list-pseudo-cmd (3d-draw-list ub32-color seq-vertices)
  ;; set the draw-list-texture!
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type sequence seq-vertices))
  ;; must be at least three vertices to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x y z u v) on seq-vertices by #'(lambda (list)
						      (nthcdr 5 list))
	       for i from vtx-offset below #.most-positive-fixnum
	       with indices = ()
	       do (setq x (clampf x))
		  (setq y (clampf y))
		  (setq z (clampf z))
		  (setq u (clampf u))
		  (setq v (clampf v))
		  (textured-3d-vertex-array-push-extend vertex-array x y z u v ub32-color)
		  (push (index-array-push-extend index-array i) indices)
		  (incf number-of-vertices)
	       finally (assert (> number-of-vertices 3))
		       (return (list* 3d-draw-list indices))))))))

(defun %draw-list-add-textured-3d-triangle-strip/list-cmd (3d-draw-list model-mtx texture ub32-color seq-vertices)
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type sequence seq-vertices))
  ;; must be at least three vertices to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
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
		  (textured-3d-vertex-array-push-extend vertex-array x y z u v ub32-color)
                  (index-array-push-extend index-array i)
               finally
                  (setq number-of-vertices (1+ i)))))
          (let ((cmd (make-standard-draw-indexed-cmd
		      3d-draw-list
		      first-index number-of-vertices vtx-offset model-mtx
		      nil texture nil nil nil)))
            (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
            cmd))))

(defun %draw-list-add-textured-3d-triangle-strip/list-with-normals-pseudo-cmd (3d-draw-list ub32-color seq-vertices)
  ;; set the draw-list-texture!
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type sequence seq-vertices))
  ;; must be at least three vertices to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
         (first-index (foreign-array-fill-pointer index-array))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for (x y z nx ny nz u v) on seq-vertices by #'(lambda (list)
							       (nthcdr 8 list))
	       for i from vtx-offset below #.most-positive-fixnum
	       with indices = ()
	       do (setq x (clampf x))
		  (setq y (clampf y))
		  (setq z (clampf z))
		  (setq nx (clampf nx))
		  (setq ny (clampf ny))
		  (setq nz (clampf nz))
		  (setq u (clampf u))
		  (setq v (clampf v))
		  (textured-3d-vertex-with-normal-array-push-extend vertex-array x y z nx ny nz u v ub32-color)
		  (push (index-array-push-extend index-array i) indices)
		  (incf number-of-vertices)
	       finally (assert (> number-of-vertices 3))
		       (return (list* 3d-draw-list indices))))))))

(defun %draw-list-add-textured-3d-triangle-strip/list-with-normals-cmd
    (3d-draw-list model-mtx texture ub32-color seq-vertices light-position)
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type sequence seq-vertices))
  ;; must be at least three vertices to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
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
		   vertex-array x y z nx ny nz u v ub32-color)
                  (index-array-push-extend index-array i)
               finally
                  (setq number-of-vertices (1+ i)))))
          (let ((cmd (make-standard-draw-indexed-cmd
		      3d-draw-list
		      first-index number-of-vertices vtx-offset model-mtx
		      nil texture nil nil light-position)))
            (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
            cmd))))

(defun %draw-list-add-multicolor-3d-convex-polygon-with-normals-pseudo-cmd (3d-draw-list seq-vertices)
  (declare (type 3d-vertex-with-normal-draw-list-mixin 3d-draw-list))
  (declare (type sequence seq-vertices))
  ;; must be at least 3 vertexes to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
	 (vtx-offset (foreign-array-fill-pointer vertex-array)))
    (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for i from vtx-offset below #.most-positive-fixnum
	       with indices = ()
	       for (x y z nx ny nz color) on seq-vertices by #'(lambda (list)
								 (nthcdr 7 list))
	       do (standard-3d-vertex-with-normal-array-push-extend
		   vertex-array (clampf x) (clampf y) (clampf z) (clampf nx) (clampf ny) (clampf nz)
		   (canonicalize-color color))
	       when (>= i 2)
		 do (push (index-array-push-extend index-array 0) indices)
		    (push (index-array-push-extend index-array (1- i)) indices)
		    (push (index-array-push-extend index-array i) indices)
	       finally (return (list* 3d-draw-list indices))))
	(array
	 (loop for i from vtx-offset below most-positive-fixnum
	       for j from 0 by 7
	       with indices = ()
	       do (standard-3d-vertex-with-normal-array-push-extend
		   vertex-array
		   (clampf (aref seq-vertices j))
		   (clampf (aref seq-vertices (1+ j)))
		   (clampf (aref seq-vertices (+ j 2)))
		   (clampf (aref seq-vertices (+ j 3)))
		   (clampf (aref seq-vertices (+ j 4)))
		   (clampf (aref seq-vertices (+ j 5)))
		   (canonicalize-color (aref seq-vertices (+ j 6))))
	       when (>= i 2)
		 do (push (index-array-push-extend index-array vtx-offset) indices)
		    (push (index-array-push-extend index-array (1- i)) indices)
		    (push (index-array-push-extend index-array i) indices)
	       finally (return (list* 3d-draw-list indices))))))))

(defun %draw-list-add-multicolor-3d-convex-polygon-with-normals-cmd (3d-draw-list model-mtx seq-vertices light-position)
  (declare (type 3d-vertex-with-normal-draw-list-mixin 3d-draw-list))
  (declare (type sequence seq-vertices))
  ;; must be at least 3 vertexes to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
	 (vtx-offset (foreign-array-fill-pointer vertex-array))
	 (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for i from 0 below #.most-positive-fixnum
	       with indices = ()
	       for (x y z nx ny nz color) on seq-vertices by #'(lambda (list)
								 (nthcdr 7 list))
	       do (standard-3d-vertex-with-normal-array-push-extend
		   vertex-array (clampf x) (clampf y) (clampf z) (clampf nx) (clampf ny) (clampf nz)
		   (canonicalize-color color))
		  (incf number-of-vertices)
	       when (>= i 2)
		 do (push (index-array-push-extend index-array 0) indices)
		    (push (index-array-push-extend index-array (1- i)) indices)
		    (push (index-array-push-extend index-array i) indices)
	       finally (return (list* 3d-draw-list indices))))
	(array
	 (loop for i from vtx-offset below most-positive-fixnum
	       for j from 0 by 7
	       with indices = ()
	       do (standard-3d-vertex-with-normal-array-push-extend
		   vertex-array
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
	  (let ((cmd (make-standard-draw-indexed-cmd
		      3d-draw-list
		      first-index number-of-vertices vtx-offset model-mtx
		      nil *white-texture* nil nil light-position)))
            (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
            cmd))))

(defun %draw-list-add-multicolor-3d-convex-polygon-pseudo-cmd (3d-draw-list seq-vertices)
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type sequence seq-vertices))
  ;; must be at least 3 vertexes to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list)))
    (declare (foreign-adjustable-array index-array vertex-array))
    (let ((first-index (foreign-array-fill-pointer index-array))
	  (vtx-offset (foreign-array-fill-pointer vertex-array)))
      (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
	(etypecase seq-vertices
	  (list
	   (loop for i from vtx-offset below #.most-positive-fixnum
		 for (x y z color) on seq-vertices by #'(lambda (list)
							  (nthcdr 4 list))
		 with indices = ()
		 do (standard-3d-vertex-array-push-extend
		     vertex-array (clampf x) (clampf y) (clampf z) (canonicalize-color color))
		 when (>= i 2)
		   do (push (index-array-push-extend index-array vtx-offset) indices)
		      (push (index-array-push-extend index-array (1- i)) indices)
		      (push (index-array-push-extend index-array i) indices)
		 finally (return (list* 3d-draw-list indices))))
	  (array
	   (loop for i from vtx-offset below #.most-positive-fixnum
		 for k from 0 by 4
		 with indices = ()
		 do (standard-3d-vertex-array-push-extend
		     vertex-array
		     (clampf (aref seq-vertices k))
		     (clampf (aref seq-vertices (1+ k)))
		     (clampf (aref seq-vertices (+ k 2)))
		     (canonicalize-color (elt seq-vertices (+ k 3))))
		 when (>= i 2)
		   do (push (index-array-push-extend index-array vtx-offset) indices)
		      (push (index-array-push-extend index-array (1- i)) indices)
		      (push (index-array-push-extend index-array i) indices)
		 finally (return (list* 3d-draw-list indices)))))))))

(defun %draw-list-add-multicolor-3d-convex-polygon-cmd (3d-draw-list model-mtx seq-vertices light-position)
  (declare (type 3d-vertex-with-normal-draw-list-mixin 3d-draw-list))
  (declare (type sequence seq-vertices))
  ;; must be at least 3 vertexes to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
	 (vtx-offset (foreign-array-fill-pointer vertex-array))
	 (number-of-vertices 0))
    (declare (type fixnum number-of-vertices))
    (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for i from 0 below #.most-positive-fixnum
	       with indices = ()
	       for (x y z color) on seq-vertices by #'(lambda (list)
							(nthcdr 4 list))
	       do (standard-3d-vertex-array-push-extend
		   vertex-array (clampf x) (clampf y) (clampf z)
		   (canonicalize-color color))
		  (incf number-of-vertices)
	       when (>= i 2)
		 do (push (index-array-push-extend index-array 0) indices)
		    (push (index-array-push-extend index-array (1- i)) indices)
		    (push (index-array-push-extend index-array i) indices)
	       finally (return (list* 3d-draw-list indices))))
	(array
	 (loop for i from vtx-offset below most-positive-fixnum
	       for j from 0 by 4
	       with indices = ()
	       do (standard-3d-vertex-array-push-extend
		   vertex-array
		   (clampf (aref seq-vertices j))
		   (clampf (aref seq-vertices (1+ j)))
		   (clampf (aref seq-vertices (+ j 2)))
		   (canonicalize-color (aref seq-vertices (+ j 3))))
		  (incf number-of-vertices)
	       when (>= i 2)
		 do (index-array-push-extend index-array 0)
		    (index-array-push-extend index-array (1- i))
		    (index-array-push-extend index-array i))))
	  (let ((cmd (make-standard-draw-indexed-cmd
		      3d-draw-list
		      first-index number-of-vertices vtx-offset model-mtx
		      nil *white-texture* nil nil light-position)))
            (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
            cmd))))

(defun %draw-list-add-filled-3d-convex-polygon-with-normals-pseudo-cmd (3d-draw-list ub32-color seq-vertices)
  (declare (type 3d-vertex-with-normal-draw-list-mixin 3d-draw-list))
  (declare (type sequence seq-vertices))
  (declare (type (unsigned-byte 32) ub32-color))
  ;; must be at least 3 vertexes to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
	 (vtx-offset (foreign-array-fill-pointer vertex-array)))
    (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for i from vtx-offset below #.most-positive-fixnum
	       for (x y z nx ny nz) on seq-vertices by #'(lambda (list)
							   (nthcdr 6 list))
	       with indices = ()
	       do (standard-3d-vertex-with-normal-array-push-extend
		   vertex-array (clampf x) (clampf y) (clampf z)
		   (clampf nx) (clampf ny) (clampf nz) ub32-color)
	       when (>= i 2)
		 do (push (index-array-push-extend index-array vtx-offset) indices)
		    (push (index-array-push-extend index-array (1- i)) indices)
		    (push (index-array-push-extend index-array i) indices)
	       finally (return (list* 3d-draw-list indices))))
	(array
	 (loop for i from vtx-offset below #.most-positive-fixnum
	       for j from 0 by 6
	       with indices = ()
	       do (standard-3d-vertex-with-normal-array-push-extend
		   vertex-array
		   (clampf (aref seq-vertices i))
		   (clampf (aref seq-vertices (1+ i)))
		   (clampf (aref seq-vertices (+ i 2)))
		   (clampf (aref seq-vertices (+ i 3)))
		   (clampf (aref seq-vertices (+ i 4)))
		   (clampf (aref seq-vertices (+ i 5)))
		   ub32-color)
	       when (>= i 2)
		 do (push (index-array-push-extend index-array vtx-offset) indices)
		    (push (index-array-push-extend index-array (1- i)) indices)
		    (push (index-array-push-extend index-array i) indices)
	       finally (return (list* 3d-draw-list indices))))))))

(defun %draw-list-add-filled-3d-convex-polygon-with-normals-cmd
    (3d-draw-list model-mtx ub32-color seq-vertices light-position)
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
    (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for i from 0 below #.most-positive-fixnum
	       for (x y z nx ny nz) on seq-vertices by #'(lambda (list)
							   (nthcdr 6 list))
	       do (standard-3d-vertex-with-normal-array-push-extend
		   vertex-array (clampf x) (clampf y) (clampf z)
		   (clampf nx) (clampf ny) (clampf nz) ub32-color)
		  (incf number-of-vertices)
	       when (>= i 2)
		 do (index-array-push-extend index-array 0)
		    (index-array-push-extend index-array (1- i))
		    (index-array-push-extend index-array i)))
	(array
	 (loop for i from 0 by 6
	       do (standard-3d-vertex-with-normal-array-push-extend
		   vertex-array
		   (clampf (aref seq-vertices i))
		   (clampf (aref seq-vertices (1+ i)))
		   (clampf (aref seq-vertices (+ i 2)))
		   (clampf (aref seq-vertices (+ i 3)))
		   (clampf (aref seq-vertices (+ i 4)))
		   (clampf (aref seq-vertices (+ i 5)))
		   ub32-color)
		  (incf number-of-vertices)
	       when (>= i 2)
		 do (index-array-push-extend index-array 0)
		    (index-array-push-extend index-array (1- i))
		    (index-array-push-extend index-array i))))
	  (let ((cmd (make-standard-draw-indexed-cmd
		      3d-draw-list
		      first-index number-of-vertices vtx-offset model-mtx
		      nil *white-texture* nil nil light-position)))
            (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
            cmd))))

(defun %draw-list-add-filled-3d-convex-polygon-pseudo-cmd (3d-draw-list ub32-color seq-vertices)
  (declare (type 3d-vertex-with-normal-draw-list-mixin 3d-draw-list))
  (declare (type sequence seq-vertices))
  (declare (type (unsigned-byte 32) ub32-color))
  ;; must be at least 3 vertexes to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
	 (first-index (foreign-array-fill-pointer index-array))
	 (vtx-offset (foreign-array-fill-pointer vertex-array)))
    (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for i from vtx-offset below #.most-positive-fixnum
	       for (x y z) on seq-vertices by #'cdddr
	       with indices = ()
	       do (standard-3d-vertex-array-push-extend
		   vertex-array (clampf x) (clampf y) (clampf z) ub32-color)
	       when (>= i 2)
		 do (push (index-array-push-extend index-array vtx-offset) indices)
		    (push (index-array-push-extend index-array (1- i)) indices)
		    (push (index-array-push-extend index-array i) indices)
	       finally (return (list* 3d-draw-list indices))))
	(array
	 (assert (> (length seq-vertices) 3))
	 (loop for j from 0 by 3
	       for i from vtx-offset below #.most-positive-fixnum
	       with indices = ()
	       do (standard-3d-vertex-array-push-extend
		   vertex-array
		   (clampf (aref seq-vertices i))
		   (clampf (aref seq-vertices (1+ i)))
		   (clampf (aref seq-vertices (+ i 2)))
		   ub32-color)
	       when (>= i 2)
		 do (push (index-array-push-extend index-array vtx-offset) indices)
		    (push (index-array-push-extend index-array (1- i)) indices)
		    (push (index-array-push-extend index-array i) indices)
	       finally (return (list* 3d-draw-list indices))))))))

(defun %draw-list-add-filled-3d-convex-polygon-cmd (3d-draw-list model-mtx ub32-color seq-vertices)
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
    (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
      (etypecase seq-vertices
	(list
	 (loop for i from 0 below #.most-positive-fixnum
	       for (x y z) on seq-vertices by #'cdddr
	       with indices = ()
	       do (standard-3d-vertex-array-push-extend
		   vertex-array (clampf x) (clampf y) (clampf z) ub32-color)
		  (incf number-of-vertices)
	       when (>= i 2)
		 do (push (index-array-push-extend index-array 0) indices)
		    (push (index-array-push-extend index-array (1- i)) indices)
		    (push (index-array-push-extend index-array i) indices)))
	(array
	 (assert (> (length seq-vertices) 3))
	 (loop for i from 0 below #.most-positive-fixnum
	       for j from 0 by 3
	       with indices = ()
	       do (standard-3d-vertex-array-push-extend
		   vertex-array
		   (clampf (aref seq-vertices j))
		   (clampf (aref seq-vertices (1+ j)))
		   (clampf (aref seq-vertices (+ j 2)))
		   ub32-color)
		  (incf number-of-vertices)
	       when (>= i 2)
		 do (push (index-array-push-extend index-array 0) indices)
		    (push (index-array-push-extend index-array (1- i)) indices)
		    (push (index-array-push-extend index-array i) indices))))
	  (let ((cmd (make-standard-draw-indexed-cmd
		      3d-draw-list
		      first-index number-of-vertices vtx-offset model-mtx
		      nil *white-texture* nil nil nil)))
            (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
            cmd))))
	  

(defun %draw-list-add-filled-sphere-pseudo-cmd (3d-draw-list
						ub32-color
						df-origin-x
						df-origin-y
						df-origin-z
						df-radius
						fixnum-resolution)
  (declare (type 3d-vertex-with-normal-draw-list-mixin 3d-draw-list))
  (declare (type (unsigned-byte 32) ub32-color))
  (declare (type double-float df-origin-x df-origin-y df-origin-z df-radius))
  (declare (type fixnum fixnum-resolution))
  (let* ((va (draw-list-vertex-array 3d-draw-list))
         (ia (draw-list-index-array 3d-draw-list)))
    (declare (type foreign-adjustable-array ia va))
    (let ((first-index (foreign-array-fill-pointer ia))
	  (vtx-offset (foreign-array-fill-pointer va)))
      (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
	(when (> df-radius 0.0d0)
	  (let* ((sector-count fixnum-resolution)
		 (stack-count (floor fixnum-resolution 2))
		 (sector-step (/ 2pi sector-count))
		 (stack-step (/ pi stack-count))
		 (elem-count 0))
	    (declare (type fixnum elem-count stack-count sector-count))
            (loop for i from 0
		  repeat stack-count
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
					 va (clampf x) (clampf y) (clampf z) nx ny nz ub32-color))))))))

            (loop for i from 0
		  repeat stack-count
		  with k1 with k2
		  with indices = ()
		  do (setq k1 (* i (1+ sector-count))
			   k2 (+ k1 sector-count 1))

                     (loop for j from 0 below sector-count
			   when (not (= i 0))
                             do (push (index-array-push-extend ia (+ vtx-offset k1)) indices)
				(push (index-array-push-extend ia (+ vtx-offset k2)) indices)
				(push (index-array-push-extend ia (+ vtx-offset (1+ k1))) indices)
				(incf elem-count 3)
			   when (not (= i (1- stack-count)))
                             do (push (index-array-push-extend ia (+ vtx-offset (1+ k1))) indices)
				(push (index-array-push-extend ia (+ vtx-offset k2)) indices)
				(push (index-array-push-extend ia (+ vtx-offset (1+ k2))) indices)
				(incf elem-count 3)
			   do (incf k1)
			      (incf k2)
			   finally (return (list* 3d-draw-list indices))))))))))
	

(defun %draw-list-add-filled-sphere-cmd (3d-draw-list model-mtx
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
    (with-draw-list-transaction (3d-draw-list first-index vtx-offset)
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
				       va (clampf x) (clampf y) (clampf z) nx ny nz ub32-color))))))))

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
                      model-mtx
                      nil *white-texture* nil nil light-position)))
	    (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
	    cmd))))))
