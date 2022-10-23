(in-package :krma)

(defclass draw-list-mixin ()
  ((index-array
    :accessor draw-list-index-array
    :initform (make-index-array))
   (vertex-array
    :reader draw-list-vertex-array)
   (cmd-vector
    :initform (make-array +draw-list-alloc-size+ :adjustable t :fill-pointer 0)
    :reader draw-list-cmd-vector)
   (changed?
    :initform t
    :accessor draw-list-changed?)
   (needs-compaction?
    :initform nil
    :accessor draw-list-needs-compaction?)
   (index-buffer
    :accessor draw-list-index-buffer
    :initform nil)
   (vertex-buffer
    :accessor draw-list-vertex-buffer
    :initform nil)
   (texture
    :accessor draw-list-texture
    :initform nil)
   (line-thickness
    :accessor draw-list-line-thickness
    :initform nil)
   (point-size
    :accessor draw-list-point-size
    :initform nil)
   (color-override
    :accessor draw-list-color-override
    :initform nil)
   (model-mtx
    :accessor draw-list-model-mtx
    :initform nil)))

;; we are using textured vertices for standard (non-textured) primitives
;; for two reasons:
;; - cuts down on the number of shader variants we have to implement/track
;; - textured and non-textured vertices can be stored in the same draw list
;;   - less draw lists, less buffers, less cmds, etc.
;; we will not use "textured-" in the name for brevity, "textured-...-vertices" are implied.
;; we also don't associate the draw-list with a particular primitive type,
;; since this is a function of the pipeline, not the draw-list
;; and different pipelines using different primitives can use the same draw-lists
(defclass 2d-vertex-draw-list-mixin (draw-list-mixin)
  ((vertex-array
    :initform (make-textured-2d-vertex-array))))

(defclass 2d-vertex-draw-list (2d-vertex-draw-list-mixin)
  ())

(defclass 3d-vertex-draw-list-mixin (draw-list-mixin)
  ((vertex-array
    :initform (make-textured-3d-vertex-array))))

(defclass 3d-vertex-draw-list (3d-vertex-draw-list-mixin)
  ())

(defclass 3d-vertex-with-normal-draw-list-mixin (draw-list-mixin)
  ((vertex-array
    :initform (make-textured-3d-vertex-with-normal-array))))

(defclass 3d-vertex-with-normal-draw-list (3d-vertex-with-normal-draw-list-mixin)
  ())

(defun %prim-reserve (draw-list vertex-count index-count vertex-type-size index-type-size)
  (let ((res nil))

    (with-slots (ptr fill-pointer allocated-count) (draw-list-vertex-array draw-list)
      (let ((reqd-size (+ vertex-count fill-pointer)))
        (unless (<= reqd-size allocated-count)
          (let ((new-count allocated-count))
            (tagbody
             try-again
               (setq new-count (* 2 new-count))
               (if (<= reqd-size new-count)
                   (go exit)
                   (go try-again))
             exit
               (let ((new-array (foreign-alloc :unsigned-char :count (* new-count vertex-type-size)))
                     (old-array ptr))
                 (memcpy new-array old-array (* fill-pointer vertex-type-size))
                 (setf ptr new-array)
                 (setf allocated-count new-count)
                 (foreign-free old-array)
                 (setq res t)))))))

    (with-slots (ptr fill-pointer allocated-count) (draw-list-index-array draw-list)
      (let ((reqd-size (+ index-count fill-pointer)))
        (unless (<= reqd-size allocated-count)
          (let ((new-count allocated-count))
            (tagbody
             try-again
               (setq new-count (* 2 new-count))
               (if (<= reqd-size new-count)
                   (go exit)
                   (go try-again))
             exit
               (let ((new-array (foreign-alloc :unsigned-char :count (* new-count index-type-size)))
                     (old-array ptr))
                 (memcpy new-array old-array (* fill-pointer index-type-size))
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
(defun %draw-list-add-2d-point (2d-draw-list color x y)
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (with-slots (vertex-array index-array) 2d-draw-list
    (let ((first-index (foreign-array-fill-pointer index-array))
          (vtx-offset (foreign-array-fill-pointer vertex-array)))
      (handler-case
          (let ((index (standard-2d-vertex-array-push-extend vertex-array x y color)))
            (index-array-push-extend index-array index))
        (error (c)
          (warn (princ-to-string c))
          (setf (foreign-array-fill-pointer vertex-array) vtx-offset
                (foreign-array-fill-pointer index-array) first-index)
          nil)))))

(defun %draw-list-add-3d-point (3d-draw-list color x y z)
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (with-slots (vertex-array index-array) 3d-draw-list
    (let ((first-index (foreign-array-fill-pointer index-array))
          (vtx-offset (foreign-array-fill-pointer vertex-array)))

      (handler-case
          (let ((index (standard-3d-vertex-array-push-extend vertex-array x y z color)))
            (index-array-push-extend index-array index))
        (error (c)
          (warn (princ-to-string c))
          (setf (foreign-array-fill-pointer vertex-array) vtx-offset
                (foreign-array-fill-pointer index-array) first-index)
          nil)))))

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
(defun %draw-list-add-2d-line (2d-draw-list color x0 y0 x1 y1)
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (let* ((ia (draw-list-index-array 2d-draw-list))
         (va (draw-list-vertex-array 2d-draw-list))
         (first-index (foreign-array-fill-pointer ia))
         (vtx-offset (foreign-array-fill-pointer va)))

    (handler-case
        (let ((offset0 (standard-2d-vertex-array-push-extend va x0 y0 color))
              (offset1 (standard-2d-vertex-array-push-extend va x1 y1 color)))
          (list (index-array-push-extend ia offset0)
                (index-array-push-extend ia offset1)))
      (error (c)
        (warn (princ-to-string c))
        (setf (foreign-array-fill-pointer ia) first-index
              (foreign-array-fill-pointer va) vtx-offset)
        nil))))

(defun %draw-list-add-3d-line (3d-draw-list color x0 y0 z0 x1 y1 z1)
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (let* ((ia (draw-list-index-array 3d-draw-list))
         (va (draw-list-vertex-array 3d-draw-list))
         (first-index (foreign-array-fill-pointer ia))
         (vtx-offset (foreign-array-fill-pointer va)))
    (handler-case
        (let ((offset0 (standard-3d-vertex-array-push-extend va x0 y0 z0 color))
              (offset1 (standard-3d-vertex-array-push-extend va x1 y1 z1 color)))
          (list (index-array-push-extend ia offset0)
                (index-array-push-extend ia offset1)))
      (error (c)
        (warn (princ-to-string c))
        (setf (foreign-array-fill-pointer ia) first-index
              (foreign-array-fill-pointer va) vtx-offset)
        nil))))

;; cmd-constructor is the "make" function of the particular cmd you want to instantiate
;; all cmds must :include essential-draw-indexed-cmd

;; this "-1" version of draw-2d-polyline-internal doesn't require the num-vertices as an arg
;; but must check for each vertex being added that the vertex and index arrays are big enough
;; todo: implement a "-2" version of draw-[2d|3d]-polyline that doesn't do extra checks
;; polylines don't need textured or with-normal versions, but polygons will
;; you can copy this cmd and issue it with different model-matrices if you wish
;; rather than baking a copy into the draw list
;; the argument `vertices' is a list of x y color ... repeating

;; for multicolored polylines
;; draw list should be used with a line-strip pipeline
(defun %draw-list-add-multicolor-2d-polyline-cmd-1 (2d-draw-list closed? model-mtx line-thickness vertices
                                                    &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type function cmd-constructor))
  ;; there must be at least one vertex to succeed
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
         (cmd-vector (draw-list-cmd-vector 2d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (elem-count 0))

    (handler-case
        (progn
          (assert (realp line-thickness))
          (loop for (x y color) on vertices by #'cdddr
                do (standard-2d-vertex-array-push-extend vertex-array x y color)
                   (index-array-push-extend index-array elem-count)
                   (incf elem-count)
                finally (when closed?
                          (index-array-push-extend index-array 0)
                          (incf elem-count)))
          (let ((cmd (funcall cmd-constructor
                              2d-draw-list
                              first-index elem-count vtx-offset
                              model-mtx nil *white-texture* (clampf line-thickness))))
            (vector-push-extend cmd cmd-vector)
            cmd))
      (error (c)
        (warn (princ-to-string c))
        (setf (foreign-array-fill-pointer vertex-array) vtx-offset
              (foreign-array-fill-pointer index-array) first-index)
        nil))))

;; the argument `vertices' is a list of x y ... repeating
;; for single color polylines
;; draw-list should be used with a line-strip pipeline
(defun %draw-list-add-2d-polyline-cmd-1 (2d-draw-list closed? model-mtx line-thickness color vertices
                                        &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type function cmd-constructor))
  ;; there must be at least one vertex to succeed
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (elem-count 0))

    (handler-case
        (progn
          (assert (realp line-thickness))
          (loop for (x y) on vertices by #'cddr
                do (standard-2d-vertex-array-push-extend vertex-array x y color)
                   (index-array-push-extend index-array elem-count)
                   (incf elem-count)
                finally (when closed?
                          (index-array-push-extend index-array 0)
                          (incf elem-count)))
          (let ((cmd (funcall cmd-constructor
                              2d-draw-list
                              first-index elem-count vtx-offset
                              model-mtx nil *white-texture* (clampf line-thickness) nil)))
            (vector-push-extend cmd (draw-list-cmd-vector 2d-draw-list))
            cmd))
      (error (c)
        (warn (princ-to-string c))
        (setf (foreign-array-fill-pointer vertex-array) vtx-offset
              (foreign-array-fill-pointer index-array) first-index)
        nil))))

(defun %draw-list-add-2d-line-list (2d-draw-list color vertices)
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) color))
  (declare (type list vertices))
  (when (cdddr vertices)
    (let* ((index-array (draw-list-index-array 2d-draw-list))
           (vertex-array (draw-list-vertex-array 2d-draw-list))
           (vtx-offset (foreign-array-fill-pointer vertex-array))
           (elem-count 0))
      (loop for (x0 y0 x1 y1) on vertices by #'cddddr
            do (standard-2d-vertex-array-push-extend vertex-array x0 y0 color)
               (index-array-push-extend index-array (+ vtx-offset elem-count))
               (incf elem-count)
               (standard-2d-vertex-array-push-extend vertex-array x1 y1 color)
               (index-array-push-extend index-array (+ vtx-offset elem-count))
               (incf elem-count)))))

;; for use with line strip pipeline
#+NIL
(defun %draw-list-add-2d-triangle-cmd (2d-draw-list model-mtx line-thickness color
                                       x0 y0 x1 y1 x2 y2
                                       &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type function cmd-constructor))
  (%draw-list-add-2d-polyline-cmd-1 2d-draw-list t model-mtx line-thickness color
                                    (list x0 y0 x1 y1 x2 y2) cmd-constructor))

;; for use with line_list pipeline, suitable for immediate mode
#+NIL
(defun %draw-list-add-2d-triangle (2d-draw-list color x0 y0 x1 y1 x2 y2)
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) color))
  (declare (type real x0 y0 x1 y1 x2 y2))
  (%draw-list-add-2d-polyline-1 2d-draw-list t color (list x0 y0 x1 y1 x2 y2)))

;; for use with line strip pipeline
#+NIL
(defun %draw-list-add-2d-rectangle-cmd (2d-draw-list model-mtx line-thickness color
                                        x0 y0 x1 y1
                                        &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type function cmd-constructor))
  (%draw-list-add-2d-polyline-cmd-1 2d-draw-list t model-mtx line-thickness color
                                    (list x0 y0 x0 y1 x1 y1 x1 y0) cmd-constructor))

;; for use with line_list pipeline, suitable for immediate-mode
#+NIL
(defun %draw-list-add-2d-rectangle (2d-draw-list color x0 y0 x1 y1)
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) color))
  (declare (type real x0 y0 x1 y1))
  (%draw-list-add-2d-polyline-1 2d-draw-list t color (list x0 y0 x0 y1 x1 y1 x1 y0)))

;; for use with line strip pipeline
(defun %draw-list-add-2d-circular-arc-cmd (2d-draw-list closed? model-mtx line-thickness color
                                           center-x center-y radius start-angle end-angle
                                           number-of-segments
                                           &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type function cmd-constructor))

  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (first-index (foreign-array-fill-pointer index-array))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array)))

    (handler-case
        (unless (minusp radius)
          (assert (typep line-thickness 'real))
          (let* ((dtheta (- start-angle end-angle)))
            (loop for i from 0 to number-of-segments
                  with theta = start-angle
                  with step = (/ dtheta number-of-segments)
                  do (let* ((coord-x (+ center-x (* radius (cos theta))))
                            (coord-y (+ center-y (* radius (sin theta)))))
                       (standard-2d-vertex-array-push-extend vertex-array coord-x coord-y color)
                       (index-array-push-extend index-array i)
                       (incf theta step))
                  finally (when closed?
                            (index-array-push-extend index-array 0)
                            (incf number-of-segments)))
            (let ((cmd (funcall cmd-constructor
                                2d-draw-list
                                first-index (1+ number-of-segments) vtx-offset
                                model-mtx nil *white-texture* (clampf line-thickness) nil)))
              (vector-push-extend cmd (draw-list-cmd-vector 2d-draw-list))
              cmd)))
      (error (c)
             (warn (princ-to-string c))
             (setf (foreign-array-fill-pointer vertex-array) vtx-offset
                   (foreign-array-fill-pointer index-array) first-index)
        nil))))

(defun %draw-list-add-2d-circular-arc (2d-draw-list closed? color
                                       center-x center-y radius start-angle end-angle
                                       number-of-segments)
  ;; uses line list instead of line strip to avoid need for cmds
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) color))
  (declare (type boolean closed?))
  (declare (type real center-x center-y radius start-angle end-angle))
  (declare (type (integer 0) number-of-segments))
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array)))
    (let* ((dtheta (- start-angle end-angle)))
      (standard-2d-vertex-array-push-extend vertex-array
                                            (+ center-x (* radius (cos start-angle)))
                                            (+ center-y (* radius (sin start-angle)))
                                            color)
      (loop for i from vtx-offset
            repeat number-of-segments
            with theta = start-angle
            with step = (/ dtheta number-of-segments)
            do (let* ((coord-x (+ center-x (* radius (cos (+ theta step)))))
                      (coord-y (+ center-y (* radius (sin (+ theta step))))))
                 (index-array-push-extend index-array i)
                 (standard-2d-vertex-array-push-extend vertex-array coord-x coord-y color)
                 (index-array-push-extend index-array (1+ i))
                 (incf theta step))
            finally (when closed?
                      (index-array-push-extend index-array vtx-offset))))))

(defun %draw-list-add-2d-circle-cmd (2d-draw-list model-mtx line-thickness color
                                     center-x center-y radius number-of-segments
                                     &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type function cmd-constructor))

  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (first-index (foreign-array-fill-pointer index-array))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array)))

    (handler-case
        (unless (minusp radius)
          (assert (realp line-thickness))
          (loop for i from 0 to number-of-segments
                with theta = 0.0d0
                with step = (/ 2pi number-of-segments)
                do (let* ((coord-x (+ center-x (* radius (cos theta))))
                          (coord-y (+ center-y (* radius (sin theta)))))
                     (standard-2d-vertex-array-push-extend vertex-array coord-x coord-y color)
                     (index-array-push-extend index-array i)
                     (incf theta step))
                finally (index-array-push-extend index-array 0))
          (let ((cmd (funcall cmd-constructor
                              2d-draw-list
                              first-index (1+ number-of-segments) vtx-offset
                              model-mtx nil (clampf line-thickness) nil)))
            (vector-push-extend cmd (draw-list-cmd-vector 2d-draw-list))
            cmd))
      (error (c)
        (warn (princ-to-string c))
        (setf (foreign-array-fill-pointer vertex-array) vtx-offset
              (foreign-array-fill-pointer index-array) first-index)
        nil))))

(defun %draw-list-add-2d-circle (2d-draw-list color
                                 center-x center-y radius
                                 number-of-segments)
  ;; uses line list instead of line strip to avoid need for cmds
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type (unsigned-byte 32) color))
  (declare (type real center-x center-y radius))
  (declare (type (integer 0) number-of-segments))
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array)))
    (standard-2d-vertex-array-push-extend vertex-array
                                          (+ center-x (* radius #.(cos 0)))
                                          (+ center-y (* radius #.(sin 0)))
                                          color)
    (loop for i from vtx-offset
          repeat number-of-segments
          with theta = 0
          with step = (/ 2pi number-of-segments)
          do (let* ((coord-x (+ center-x (* radius (cos (+ theta step)))))
                    (coord-y (+ center-y (* radius (sin (+ theta step))))))
               (index-array-push-extend index-array i)
               (standard-2d-vertex-array-push-extend vertex-array coord-x coord-y color)
               (index-array-push-extend index-array (1+ i))
               (incf theta step)))))

(defun %draw-list-add-multicolor-3d-polyline-cmd-1
    (3d-draw-list closed? model-mtx line-thickness vertices
     &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type function cmd-constructor))
  ;; there must be at least one vertex to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (elem-count 0))
    (handler-case
        (progn
          (assert (realp line-thickness))
          (loop for (x y z color) on vertices by #'cddddr
                do (standard-3d-vertex-array-push-extend vertex-array x y z color)
                   (index-array-push-extend index-array elem-count)
                   (incf elem-count)
                finally (when closed?
                          (index-array-push-extend index-array 0)
                          (incf elem-count)))
          (let ((cmd (funcall cmd-constructor
                              3d-draw-list
                              first-index elem-count vtx-offset
                              model-mtx nil (clampf line-thickness) nil)))
            (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
            cmd))
      (error (c)
        (warn (princ-to-string c))
        (setf (foreign-array-fill-pointer vertex-array) vtx-offset
              (foreign-array-fill-pointer index-array) first-index)
        nil))))

(defun %draw-list-add-3d-polyline-cmd-1 (3d-draw-list model-mtx closed? line-thickness color vertices
                                         &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type function cmd-constructor))
  ;; there must be at least one vertex to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (elem-count 0))
    (handler-case
        (progn
          (assert (realp line-thickness))
          (loop for (x y z) on vertices by #'cdddr
                do (standard-3d-vertex-array-push-extend vertex-array x y z color)
                   (index-array-push-extend index-array elem-count)
                   (incf elem-count)
                finally (when closed?
                          (index-array-push-extend index-array 0)
                          (incf elem-count)))
          (let ((cmd (funcall cmd-constructor
                              3d-draw-list
                              first-index elem-count vtx-offset
                              model-mtx nil (clampf line-thickness) nil)))
            (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
            cmd))
      (error (c)
        (warn (princ-to-string c))
        (setf (foreign-array-fill-pointer vertex-array) vtx-offset
              (foreign-array-fill-pointer index-array) first-index)
        nil))))

(defun %draw-list-add-filled-2d-triangle-list-cmd (2d-draw-list model-mtx color vertices
                                                   &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type function cmd-constructor))
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (number-of-vertices 0))
    (handler-case
        (progn
          (loop for (x y) on vertices by #'cddr
                for i from 0
                do (standard-2d-vertex-array-push-extend vertex-array x y color)
                   (incf number-of-vertices)
                   (index-array-push-extend index-array i))
          (let ((cmd (funcall cmd-constructor
                              2d-draw-list
                              first-index number-of-vertices vtx-offset model-mtx
                              nil *white-texture* nil)))
            (vector-push-extend cmd (draw-list-cmd-vector 2d-draw-list))
            cmd))
      (error (c)
        (warn (princ-to-string c))
        (setf (foreign-array-fill-pointer vertex-array) vtx-offset
              (foreign-array-fill-pointer index-array) first-index)
        nil))))

(defun %draw-list-add-filled-2d-rectangle-list-cmd (2d-draw-list model-mtx color vertices
                                                    &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type function cmd-constructor))
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (elem-count 0))
    (handler-case
        (progn
          (loop for (x0 y0 x1 y1) on vertices by #'(lambda (list)
                                                     (nthcdr 4 list))
                for i from 0 by 4
                do (standard-2d-vertex-array-push-extend vertex-array x0 y0 color)
                   (standard-2d-vertex-array-push-extend vertex-array x0 y1 color)
                   (standard-2d-vertex-array-push-extend vertex-array x1 y1 color)
                   (standard-2d-vertex-array-push-extend vertex-array x1 y0 color)
                   (index-array-push-extend index-array i)
                   (index-array-push-extend index-array (1+ i))
                   (index-array-push-extend index-array (+ 2 i))
                   (index-array-push-extend index-array i)
                   (index-array-push-extend index-array (+ 2 i))
                   (index-array-push-extend index-array (+ 3 i))
                   (incf elem-count 6))
          (let ((cmd (funcall cmd-constructor
                              2d-draw-list
                              first-index elem-count vtx-offset model-mtx
                              nil *white-texture* nil)))
            (vector-push-extend cmd (draw-list-cmd-vector 2d-draw-list))
            cmd))
      (error (c)
        (warn (princ-to-string c))
        (setf (foreign-array-fill-pointer vertex-array) vtx-offset
              (foreign-array-fill-pointer index-array) first-index)
        nil))))

;; used to implement add-text
(defun %draw-list-add-textured-2d-rectangle-list-cmd (2d-draw-list model-mtx texture color vertices
                                                      &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type function cmd-constructor))
  ;; must be at least 2 vertices to succeed
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (elem-count 0))
    (handler-case
        (progn
          (loop for (x0 y0 u0 v0 x1 y1 u1 v1) on vertices by #'(lambda (list)
                                                                 (nthcdr 8 list))
                for i from 0 by 4
                do (textured-2d-vertex-array-push-extend vertex-array x0 y0 u0 v0 color)
                   (textured-2d-vertex-array-push-extend vertex-array x0 y1 u0 v1 color)
                   (textured-2d-vertex-array-push-extend vertex-array x1 y1 u1 v1 color)
                   (textured-2d-vertex-array-push-extend vertex-array x1 y0 u1 v0 color)
                   (index-array-push-extend index-array i)
                   (index-array-push-extend index-array (1+ i))
                   (index-array-push-extend index-array (+ 2 i))
                   (index-array-push-extend index-array i)
                   (index-array-push-extend index-array (+ 2 i))
                   (index-array-push-extend index-array (+ 3 i))
                   (incf elem-count 6))
          (let ((cmd (funcall cmd-constructor
                              2d-draw-list
                              first-index elem-count vtx-offset model-mtx
                              nil texture nil)))
            (vector-push-extend cmd (draw-list-cmd-vector 2d-draw-list))
            cmd))
      (error (c)
        (warn (princ-to-string c))
        (setf (foreign-array-fill-pointer vertex-array) vtx-offset
              (foreign-array-fill-pointer index-array) first-index)
        nil))))

;; used to implement draw-text
;; will render the whole list in one draw command
(defun %draw-list-add-textured-2d-rectangle-list (2d-draw-list color vertices)
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  ;; must be at least 2 vertices to succeed
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list)))
    (loop for (x0 y0 u0 v0 x1 y1 u1 v1) on vertices by #'(lambda (list)
                                                           (nthcdr 8 list))
          for i from 0 by 4
          do (textured-2d-vertex-array-push-extend vertex-array x0 y0 u0 v0 color)
             (textured-2d-vertex-array-push-extend vertex-array x0 y1 u0 v1 color)
             (textured-2d-vertex-array-push-extend vertex-array x1 y1 u1 v1 color)
             (textured-2d-vertex-array-push-extend vertex-array x1 y0 u1 v0 color)
             (index-array-push-extend index-array i)
             (index-array-push-extend index-array (1+ i))
             (index-array-push-extend index-array (+ 2 i))
             (index-array-push-extend index-array i)
             (index-array-push-extend index-array (+ 2 i))
             (index-array-push-extend index-array (+ 3 i)))
    (values)))

(defun %draw-list-add-filled-2d-polygon-cmd (2d-draw-list model-mtx color vertices
                                             &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
  (declare (type 2d-vertex-draw-list-mixin 2d-draw-list))
  (declare (type function cmd-constructor))
  ;; must be at least 3 vertexes to succeed
  (let* ((index-array (draw-list-index-array 2d-draw-list))
         (vertex-array (draw-list-vertex-array 2d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (number-of-vertices 0))

    (handler-case
        (progn
          (loop for i from 0
                for (x y) on vertices by #'cddr

                do (standard-2d-vertex-array-push-extend vertex-array x y color)
                   (incf number-of-vertices)
                when (>= i 2)
                  do (index-array-push-extend index-array 0)
                     (index-array-push-extend index-array (1- i))
                     (index-array-push-extend index-array i))

          (let ((cmd (funcall cmd-constructor
                              2d-draw-list
                              first-index (* 3 (- number-of-vertices 2)) vtx-offset model-mtx
                              nil *white-texture* nil)))
            (vector-push-extend cmd (draw-list-cmd-vector 2d-draw-list))
            cmd))
      (error (c)
        (warn (princ-to-string c))
        (setf (foreign-array-fill-pointer vertex-array) vtx-offset
              (foreign-array-fill-pointer index-array) first-index)
        nil))))

(defun %draw-list-add-filled-3d-triangle-strip-cmd (3d-draw-list model-mtx color vertices
                                                   &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type function cmd-constructor))
  ;; must be at least three vertices to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (number-of-vertices 0))

    (handler-case
        (progn
          (loop for (x y z) on vertices by #'cdddr
                for i from 0
                do (standard-3d-vertex-array-push-extend vertex-array x y z color)
                   (index-array-push-extend index-array i)
                   (incf number-of-vertices))
          (let ((cmd (funcall cmd-constructor
                              3d-draw-list
                              first-index number-of-vertices vtx-offset model-mtx
                              nil *white-texture* nil)))
            (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
            cmd))
      (error (c)
        (warn (princ-to-string c))
        (setf (foreign-array-fill-pointer vertex-array) vtx-offset
              (foreign-array-fill-pointer index-array) first-index)
        nil))))

(defun %draw-list-add-filled-3d-triangle-strip-with-normals-cmd
    (3d-draw-list model-mtx color vertices
     &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type function cmd-constructor))
  ;; must be at least three vertices to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (number-of-vertices 0))

    (handler-case
        (progn
          (loop for (x y z nx ny nz) on vertices by #'cdddr
                for i from 0
                do (standard-3d-vertex-with-normal-array-push-extend vertex-array x y z nx ny nz color)
                   (incf number-of-vertices)
                   (index-array-push-extend index-array i))
          (let ((cmd (funcall cmd-constructor
                              3d-draw-list
                              first-index number-of-vertices vtx-offset model-mtx
                              nil nil *white-texture*)))
            (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
            cmd))
      (error (c)
        (warn (princ-to-string c))
        (setf (foreign-array-fill-pointer vertex-array) vtx-offset
              (foreign-array-fill-pointer index-array) first-index)
        nil))))

(defun %draw-list-add-textured-3d-triangle-strip-cmd (3d-draw-list model-mtx texture color vertices
                                                      &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
  (declare (type 3d-vertex-draw-list-mixin 3d-draw-list))
  (declare (type function cmd-constructor))
  ;; must be at least three vertices to succeed
  (let* ((index-array (draw-list-index-array 3d-draw-list))
         (vertex-array (draw-list-vertex-array 3d-draw-list))
         (vtx-offset (foreign-array-fill-pointer vertex-array))
         (first-index (foreign-array-fill-pointer index-array))
         (number-of-vertices 0))

    (handler-case
        (progn
          (loop for (x y z u v) on vertices by #'(lambda (list)
                                                   (nthcdr 5 list))
                for i from 0
                do (textured-3d-vertex-array-push-extend vertex-array x y z u v color)
                   (incf number-of-vertices)
                   (index-array-push-extend index-array i)
                finally
                   (setq number-of-vertices (1+ i)))
          (let ((cmd (funcall cmd-constructor
                              3d-draw-list
                              first-index number-of-vertices vtx-offset model-mtx
                              nil nil texture)))
            (vector-push-extend cmd (draw-list-cmd-vector 3d-draw-list))
            cmd))
      (error (c)
        (warn (princ-to-string c))
        (setf (foreign-array-fill-pointer vertex-array) vtx-offset
              (foreign-array-fill-pointer index-array) first-index)
        nil))))

(defun %draw-list-add-filled-sphere-cmd (3d-draw-list model-mtx color origin-x origin-y origin-z radius resolution light-position)
  (let* ((va (draw-list-vertex-array 3d-draw-list))
         (ia (draw-list-index-array 3d-draw-list))
         (vtx-offset (foreign-array-fill-pointer va))
         (first-index (foreign-array-fill-pointer ia)))

    (handler-case
        (progn
          (let* ((sector-count resolution)
                 (stack-count (/ resolution 2))
                 (sector-step (/ 2pi sector-count))
                 (stack-step (/ pi stack-count))
                 (elem-count 0))

            (loop for i from 0 to stack-count
                  do (let* ((stack-angle (- #.(/ pi 2) (* i stack-step)))
                            (xy (* radius (cos stack-angle)))
                            (z (+ (* radius (sin stack-angle)) origin-z)))
                       (loop for j from 0 to sector-count
                             do (let* ((sector-angle (* j sector-step))
                                       (x (+ (* xy (cos sector-angle)) origin-x))
                                       (y (+ (* xy (sin sector-angle)) origin-y))
                                       (nx (/ (- x origin-x) radius))
                                       (ny (/ (- y origin-y) radius))
                                       (nz (/ (- z origin-z) radius)))

                                  (standard-3d-vertex-with-normal-array-push-extend va x y z nx ny nz color)))))

            (loop for i from 0 below stack-count
                  with k1 with k2
                  do (setq k1 (* i (1+ sector-count))
                           k2 (+ k1 sector-count 1))

                     (loop for j from 0 below sector-count
                           when (not (zerop i))
                             do (index-array-push-extend ia k1)
                                (index-array-push-extend ia k2)
                                (index-array-push-extend ia (1+ k1))
                                (incf elem-count 3)
                           when (not (eq i (1- stack-count)))
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
              cmd)))
      (error (c)
        (warn (princ-to-string c))
        (setf (foreign-array-fill-pointer va) vtx-offset
              (foreign-array-fill-pointer ia) first-index)
        nil))))

(defun compact-draw-list (draw-list &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
  (block nil
    (if (not (draw-list-needs-compaction? draw-list))

        (return draw-list)

        (let* ((cmd-vector (draw-list-cmd-vector draw-list))
               (new-draw-list (make-instance (class-of draw-list)))
               (new-index-array (draw-list-index-array new-draw-list))
               (new-vertex-array (draw-list-vertex-array new-draw-list))
               (old-index-array (draw-list-index-array draw-list))
               (old-vertex-array (draw-list-vertex-array draw-list))
               (fp-old-vertex-array (foreign-array-fill-pointer old-vertex-array))
               (vertex-type-size (foreign-array-foreign-type-size old-vertex-array))
               (index-type (foreign-array-foreign-type old-index-array)))

          (%prim-reserve new-draw-list
                         fp-old-vertex-array
                         (foreign-array-fill-pointer old-index-array)
                         (foreign-array-foreign-type-size old-vertex-array)
                         (foreign-array-foreign-type-size old-index-array))


          (cond ((zerop (fill-pointer cmd-vector))
                 ;; case #1: (for point list and line list)
                 ;; it does not have cmds (and never did) (the drawindexed parameters are stored elsewhere)
                 ;; memcpy segments and recreate index-array

                 (let ((i 0)
                       (prev-index nil)
                       (this-index nil)
                       (count 0)
                       (old-array-segment-start-offset nil)
                       (new-array-segment-start-offset nil))

                   (tagbody
                    again
                      (when (eq i fp-old-vertex-array)
                        (go exit))

                      ;; these index arrays are in possibly non-consecutive increasing order
                      ;; and we put them in consecutive increasing order
                      (setq this-index (mem-aref old-index-array index-type i))

                      (unless new-array-segment-start-offset
                        (setq new-array-segment-start-offset i))
                      (unless old-array-segment-start-offset
                        (setq old-array-segment-start-offset this-index))

                      (when prev-index
                        (incf count)
                        (unless (eq prev-index (1- this-index))
                          ;; discontinuity, copy the segment
                          (memcpy (inc-pointer (foreign-array-ptr new-vertex-array)
                                               (* new-array-segment-start-offset vertex-type-size))
                                  (inc-pointer (foreign-array-ptr old-vertex-array)
                                               (* old-array-segment-start-offset vertex-type-size))
                                  (* count vertex-type-size))

                          (incf new-array-segment-start-offset count)
                          (setq old-array-segment-start-offset this-index)

                          (setq count 0)
                          (setq prev-index nil)
                          (go again)))

                      ;; no discontinuity
                      (setq prev-index this-index) ;; same as (incf prev-index).
                      (index-array-push-extend new-index-array i)
                      (incf i)
                      (go again)

                    exit
                      (foreign-free (foreign-array-ptr old-index-array))
                      (foreign-free (foreign-array-ptr old-vertex-array))
                      (return new-draw-list))))



                (t ;; case #2: usually for line_strip, triangle_strip, triangle_list or any draw-list with cmds
                 (loop for cmd across cmd-vector
                       with new-vtx-offset = -1
                       with translation-table-table = (make-hash-table :test #'eql)
                       with translation-table = nil
                       when cmd
                         do ;; cmd-first-index clues us in to whether this is a reinstanced cmd
                            ;; if so use the translation table which exists from previous instance
                            (setq translation-table (gethash (cmd-first-idx cmd) translation-table-table))
                            (unless translation-table
                              (setf (gethash (cmd-first-idx cmd) translation-table-table)
                                    (setq translation-table (make-hash-table :test #'eql))))
                            (loop for i from (cmd-first-idx cmd)
                                  repeat (cmd-elem-count cmd)
                                  with cmd-vtx-offset = (cmd-vtx-offset cmd)
                                  with translation = nil
                                  with orig-idx
                                  do (setq orig-idx (mem-aref (foreign-array-ptr old-index-array) index-type i))
                                  when (setq translation (gethash orig-idx translation-table))
                                    do ;; if there is a translation of index, add it to index-array at fill-pointer
                                       (index-array-push-extend new-index-array translation)
                                  unless translation
                                    do ;; copy one vertex into new vertex array at index (1+ new-vtx-offset)
                                       ;; no vertex should be copied twice, but some may not be copied at all
                                       ;; it doesn't matter what the vertex-array fill-pointer is. we'll adjust
                                       ;; that at the end
                                       (memcpy (inc-pointer (foreign-array-ptr new-vertex-array)
                                                            (* (incf new-vtx-offset) vertex-type-size))
                                               (inc-pointer (foreign-array-ptr old-vertex-array)
                                                            (* (+ cmd-vtx-offset orig-idx) vertex-type-size))
                                               (* 1 vertex-type-size))
                                       ;; put new index into new index array at fill-pointer
                                       (index-array-push-extend new-index-array new-vtx-offset)
                                       ;; record the translation
                                       (setf (gethash orig-idx translation-table) new-vtx-offset)
                                  finally ;; copy the cmd, except update the first-index and vtx-offset
                                          (vector-push-extend (funcall cmd-constructor
                                                                       new-draw-list
                                                                       (foreign-array-fill-pointer new-index-array)
                                                                       (cmd-elem-count cmd)
                                                                       ;; the vertex array fp hasn't been updated yet
                                                                       ;; and we want to use it as is anyway
                                                                       ;; we'll update it below for use in the next cmd
                                                                       (foreign-array-fill-pointer new-vertex-array)
                                                                       (cmd-model-mtx cmd)
                                                                       (cmd-color-override cmd)
                                                                       (cmd-texture cmd)
                                                                       (cmd-line-thickness cmd)
                                                                       (cmd-point-size cmd))
                                                              (draw-list-cmd-vector new-draw-list))
                                          ;; finally update the vertex array fp so that vertex-pushes work in the future
                                          (setf (foreign-array-fill-pointer new-vertex-array) (1+ new-vtx-offset)))
                       finally
                          (foreign-free (foreign-array-ptr old-index-array))
                          (foreign-free (foreign-array-ptr old-vertex-array))
                          (return new-draw-list))))))))
