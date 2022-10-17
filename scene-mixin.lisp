(in-package :krma)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-concurrency))

(defclass krma-essential-scene-mixin ()
  ((application :initarg :app)
   (im-draw-data :initform (make-standard-draw-data "IM Draw Data")
                 :reader im-draw-data)
   (rm-draw-data
    :reader rm-draw-data
    :initform (make-array 2 :initial-contents
                          (list
                           (make-retained-mode-draw-data "RM Draw Data 0")
                           (make-retained-mode-draw-data "RM Draw Data 1"))))))

(defclass standard-scene (krma-essential-scene-mixin) ())

(defun render-scene (app command-buffer draw-data width height
                     &optional (model-matrix (meye 4))
                       (view-matrix (meye 4) #+MESSEDUP(mlookat (vec3 0 0 10000) (vec3 0 0 -10000) (vec3 0 1 0)))
                       (projection-matrix (mortho-vulkan 0 width height 0 -1500 1500)))

  (with-slots (3d-point-list-pipeline
               3d-line-list-pipeline
               3d-line-strip-pipeline
               3d-triangle-list-pipeline
               3d-triangle-strip-pipeline
               2d-point-list-pipeline
               2d-line-list-pipeline
               2d-line-strip-pipeline
               2d-triangle-list-pipeline
               2d-triangle-strip-pipeline)

      (application-pipeline-store app)

    (let ((device (default-logical-device app)))

      (with-slots (3d-point-list-draw-list
                   3d-line-list-draw-list
                   3d-line-strip-draw-list
                   3d-triangle-list-draw-list
                   3d-triangle-strip-draw-list
                   2d-point-list-draw-list
                   2d-line-list-draw-list
                   2d-line-strip-draw-list
                   2d-triangle-list-draw-list
                   2d-triangle-strip-draw-list)

          draw-data


        (render 3d-point-list-pipeline 3d-point-list-draw-list
                device command-buffer model-matrix view-matrix projection-matrix width height)

        (render 3d-line-list-pipeline 3d-line-list-draw-list
                device command-buffer model-matrix view-matrix projection-matrix width height)

        (render 3d-line-strip-pipeline 3d-line-strip-draw-list
                device command-buffer model-matrix view-matrix projection-matrix width height)

        (render 3d-triangle-list-pipeline 3d-triangle-list-draw-list
                device command-buffer model-matrix view-matrix projection-matrix width height)
        #+NOTYET
        (render 3d-triangle-strip-pipeline 3d-triangle-strip-draw-list
                device command-buffer model-matrix view-matrix projection-matrix width height)

        (render 2d-point-list-pipeline 2d-point-list-draw-list
                device command-buffer model-matrix view-matrix projection-matrix width height)

        (render 2d-line-list-pipeline 2d-line-list-draw-list
                device command-buffer model-matrix view-matrix projection-matrix width height)

        (render 2d-line-strip-pipeline 2d-line-strip-draw-list
                device command-buffer model-matrix view-matrix projection-matrix width height)

        (render 2d-triangle-list-pipeline 2d-triangle-list-draw-list
                device command-buffer model-matrix view-matrix projection-matrix width height)
        #+NOTYET
        (render 2d-triangle-strip-pipeline 2d-triangle-strip-draw-list
                device command-buffer model-matrix view-matrix projection-matrix width height)


        (values)))))

(defun scene-add-2d-point (scene color x y)
  (declare (type krma-essential-scene-mixin scene))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-2d-point draw-data handle color x y)))

(defun scene-draw-2d-point (scene color x y)
  (declare (type krma-essential-scene-mixin scene))
  (let ((draw-data (rm-draw-data scene)))
    (%draw-list-add-2d-point (draw-data-2d-point-list-draw-list draw-data) color x y)))

(defun scene-add-3d-point (scene color x y z)
  (declare (type krma-essential-scene-mixin scene))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-3d-point draw-data handle color x y z)))

(defun scene-draw-3d-point (scene color x y z)
  (declare (type krma-essential-scene-mixin scene))
  (let ((draw-data (rm-draw-data scene)))
    (%draw-list-add-3d-point (draw-data-3d-point-list-draw-list draw-data) color x y z)))

(defun scene-add-2d-line (scene color x0 y0 x1 y1)
  (declare (type krma-essential-scene-mixin scene))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-2d-line draw-data handle color x0 y0 x1 y1)))

(defun scene-add-3d-line (scene color x0 y0 z0 x1 y1 z1)
  (declare (type krma-essential-scene-mixin scene))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-3d-line draw-data handle color x0 y0 z0 x1 y1 z1)))

(defun scene-add-multicolor-2d-polyline-1 (scene closed? model-mtx line-thickness vertices)
  (declare (type krma-essential-scene-mixin scene))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-multicolor-2d-polyline-cmd-1 draw-data handle closed? model-mtx line-thickness vertices)))

(defun scene-draw-multicolor-2d-polyline-1 (scene closed? vertices)
  (declare (type krma-essential-scene-mixin scene))
  (let ((draw-data (rm-draw-data scene)))
    (declare (type standard-draw-data draw-data))
    (%draw-list-add-multicolor-2d-polyline-1
     (draw-data-3d-line-list-draw-list draw-data) closed? vertices)))

(defun scene-add-2d-polyline-1 (scene closed? model-mtx line-thickness color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-2d-polyline-cmd-1 draw-data handle closed? model-mtx line-thickness color vertices)))

(defun scene-add-2d-triangle (scene model-mtx line-thickness color x0 y0 x1 y1 x2 y2)
  (declare (type krma-essential-scene-mixin scene))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-2d-triangle-cmd draw-data handle model-mtx line-thickness color x0 y0 x1 y1 x2 y2)))

(defun scene-draw-2d-triangle (scene color x0 y0 x1 y1 x2 y2)
  (declare (type krma-essential-scene-mixin scene))
  (let ((draw-data (rm-draw-data scene)))
    (declare (type standard-draw-data draw-data))
    (%draw-list-add-2d-triangle
     (draw-data-3d-line-list-draw-list draw-data) color x0 y0 x1 y1 x2 y2)))

(defun scene-add-2d-rectangle (scene model-mtx line-thickness color x0 y0 x1 y1)
  (declare (type krma-essential-scene-mixin scene))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-2d-rectangle-cmd draw-data handle model-mtx line-thickness color x0 y0 x1 y1)))

(defun scene-draw-2d-rectangle (scene color x0 y0 x1 y1)
  (declare (type krma-essential-scene-mixin scene))
  (let ((draw-data (rm-draw-data scene)))
    (declare (type standard-draw-data draw-data))
    (%draw-list-add-2d-rectangle
     (draw-data-3d-line-list-draw-list draw-data) color x0 y0 x1 y1)))

(defun scene-add-2d-circular-arc (scene closed? model-mtx line-thickness color
                                  center-x center-y radius start-angle end-angle
                                  &optional (number-of-segments 64))
  (declare (type krma-essential-scene-mixin scene))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-2d-circular-arc-cmd draw-data handle closed? model-mtx line-thickness color
                                        center-x center-y radius start-angle end-angle
                                        number-of-segments)))

(defun scene-add-2d-circle (scene model-mtx line-thickness color
                            center-x center-y radius
                            &optional (number-of-segments 64))
  (declare (type krma-essential-scene-mixin scene))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-2d-circle-cmd draw-data handle model-mtx line-thickness color
                                  center-x center-y radius
                                  number-of-segments)))

(defun scene-add-multicolor-3d-polyline-1 (scene closed? model-mtx line-thickness vertices)
  (declare (type krma-essential-scene-mixin scene))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-multicolor-3d-polyline-cmd-1 draw-data handle closed? model-mtx line-thickness vertices)))

(defun scene-add-3d-polyline-1 (scene closed? model-mtx line-thickness color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-3d-polyline-cmd-1 draw-data handle model-mtx closed? line-thickness color vertices)))

(defun scene-add-filled-2d-triangle-list (scene model-mtx color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-2d-triangle-list-cmd draw-data handle model-mtx color vertices)))

(defun scene-add-filled-2d-rectangle-list (scene model-mtx color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-2d-rectangle-list-cmd draw-data handle model-mtx color vertices)))

(defun scene-add-textured-2d-rectangle-list (scene model-mtx texture color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-textured-2d-rectangle-list-cmd draw-data handle model-mtx texture color vertices)))

(defun scene-add-filled-2d-polygon (scene model-mtx color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-2d-polygon-cmd draw-data handle model-mtx color vertices)))

(defun scene-add-filled-3d-triangle-list (scene model-mtx color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-3d-triangle-list-cmd draw-data handle model-mtx color vertices)))

(defun scene-add-filled-3d-triangle-strip (scene model-mtx color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-3d-triangle-strip-cmd draw-data handle model-mtx color vertices)))

(defun scene-add-filled-3d-triangle-list-with-normals (scene model-mtx color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-3d-triangle-list-with-normals-cmd draw-data handle model-mtx color vertices)))

(defun scene-add-filled-3d-triangle-strip-with-normals (scene model-mtx color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-filled-3d-triangle-strip-with-normals-cmd draw-data handle model-mtx color vertices)))

(defun scene-add-textured-3d-triangle-list (scene model-mtx texture color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-textured-3d-triangle-list-cmd draw-data handle model-mtx texture color vertices)))

(defun scene-add-textured-3d-triangle-strip (scene model-mtx texture color vertices)
  (declare (type krma-essential-scene-mixin scene))
  (rm-dispatch-to-render-thread (scene draw-data handle)
    (%draw-data-add-textured-3d-triangle-strip-cmd draw-data handle model-mtx texture color vertices)))

(defun %reinstance-cmd (cmd model-mtx line-thickness color-override
                        &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
  (let ((first-idx (cmd-first-idx cmd))
        (elem-count (cmd-elem-count cmd))
        (vtx-offset (cmd-vtx-offset cmd))
        (draw-list (cmd-draw-list cmd))
        (texture (cmd-texture cmd))
        (model-mtx (if model-mtx model-mtx (cmd-model-mtx cmd)))
        (line-thickness (if line-thickness line-thickness (cmd-line-thickness cmd)))
        (color-override (if color-override color-override (cmd-color-override cmd))))
    (let ((cmd (funcall cmd-constructor
                        draw-list
                        first-idx elem-count vtx-offset
                        model-mtx line-thickness color-override texture)))
      (vector-push-extend cmd (draw-list-cmd-vector draw-list))
      cmd)))

(defun reinstance-primitive (scene handle
                             &key (model-matrix nil)
                               (line-thickness nil)
                               (color-override nil)
                               (cmd-constructor #'make-standard-draw-indexed-cmd))
  (declare (type krma-essential-scene-mixin scene))
  (let ((draw-data (rm-draw-data scene)))
    (let ((dd0 (aref draw-data 0))
          (dd1 (aref draw-data 1)))
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
                           ;; it's silly to reinstance a point or a line segment with a new model matrix
                           ;; since it's just as easy to bake the coordinates and add new points or line segments
                           ;; of a potentially different color
                           ;; point size and line thickness changes will have to be supported through
                           ;; additional draw-lists (todo) (since I'm aiming to make point and line draw lists
                           ;; render in one drawindexed invocation)
                           ;; so there is no point in reinstancing, having separate cmds for these things would only
                           ;; slow down something that can be implemented faster by having more draw-lists
                           (warn "failed to reinstance ~S" handle)
                           (setf (gethash new-handle ht)
                                 (%reinstance-cmd cmd model-matrix line-thickness color-override
                                                  cmd-constructor)))
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
  (declare (type (unsigned-byte 32) color))
  (let ((draw-data (rm-draw-data scene)))
    (let ((dd0 (aref draw-data 0))
          (dd1 (aref draw-data 1)))
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
                                                                 index)))

                                      (setf (foreign-slot-value
                                             (inc-pointer (foreign-array-ptr vertex-array) (* vtx-offset vtx-type-size))
                                             vtx-type 'col)
                                            color)))
                           (setf (cmd-color-override cmd) color))))))
          (sb-concurrency:enqueue #'(lambda () (update-color! ht0)) wq0)
          (sb-concurrency:enqueue #'(lambda () (update-color! ht1)) wq1)
          (values))))))

;; replaces model-mtx in cmd by this matrix
(defun primitive-set-transform (scene handle matrix)
  (declare (type krma-essential-scene-mixin scene))
  (let ((draw-data (rm-draw-data scene)))
    (let ((dd0 (aref draw-data 0))
          (dd1 (aref draw-data 1)))
      (declare (type retained-mode-draw-data dd0 dd1))
      (let ((ht0 (draw-data-handle-hash-table dd0))
            (ht1 (draw-data-handle-hash-table dd1))
            (wq0 (draw-data-work-queue dd0))
            (wq1 (draw-data-work-queue dd1)))
        (flet ((update-matrix! (ht)
                 (let ((cmd (gethash handle ht)))
                   (if (listp cmd)
                       (warn "could not set transform for primitive ~S." handle)
                       (setf (cmd-model-mtx cmd) matrix)))))
          (sb-concurrency:enqueue #'(lambda () (update-matrix! ht0)) wq0)
          (sb-concurrency:enqueue #'(lambda () (update-matrix! ht1)) wq1)
          (values))))))

;; multiplies new matrix against old matrix and replaces model-mtx in cmd
(defun primitive-apply-transform (scene handle matrix)
  (declare (type krma-essential-scene-mixin scene))
  (let ((draw-data (rm-draw-data scene)))
    (let ((dd0 (aref draw-data 0))
          (dd1 (aref draw-data 1)))
      (declare (type retained-mode-draw-data dd0 dd1))
      (let ((ht0 (draw-data-handle-hash-table dd0))
            (ht1 (draw-data-handle-hash-table dd1))
            (wq0 (draw-data-work-queue dd0))
            (wq1 (draw-data-work-queue dd1)))
        (flet ((multiply-matrix! (ht)
                 (let ((cmd (gethash handle ht)))
                   (if (listp cmd)
                       (warn "could not apply transform for primitive ~S." handle)
                       (setf (cmd-model-mtx cmd) (funcall *multiply-matrix-function* matrix (cmd-model-mtx cmd)))))))
          (sb-concurrency:enqueue #'(lambda () (multiply-matrix! ht0)) wq0)
          (sb-concurrency:enqueue #'(lambda () (multiply-matrix! ht1)) wq1)
          (values))))))

(defun primitive-set-line-thickness (scene handle thickness)
  (declare (type krma-essential-scene-mixin scene))
  (declare (type real thickness))
  (let ((draw-data (rm-draw-data scene)))
    (let ((dd0 (aref draw-data 0))
          (dd1 (aref draw-data 1)))
      (declare (type retained-mode-draw-data dd0 dd1))
      (let ((ht0 (draw-data-handle-hash-table dd0))
            (ht1 (draw-data-handle-hash-table dd1))
            (wq0 (draw-data-work-queue dd0))
            (wq1 (draw-data-work-queue dd1)))
        (flet ((update-thickness! (ht)
                 (let ((cmd (gethash handle ht)))
                   (if (listp cmd)
                       (warn "could not update line thickness for primitive ~S." handle)
                       (setf (cmd-line-thickness cmd) thickness)))))
          (sb-concurrency:enqueue #'(lambda () (update-thickness! ht0)) wq0)
          (sb-concurrency:enqueue #'(lambda () (update-thickness! ht1)) wq1)
          (values))))))

(defun delete-primitive (scene handle)
  (declare (type krma-essential-scene-mixin scene))
  (let ((draw-data (rm-draw-data scene)))
    (let ((dd0 (aref draw-data 0))
          (dd1 (aref draw-data 1)))
      (declare (type retained-mode-draw-data dd0 dd1))
      (let ((ht0 (draw-data-handle-hash-table dd0))
            (ht1 (draw-data-handle-hash-table dd1))
            (wq0 (draw-data-work-queue dd0))
            (wq1 (draw-data-work-queue dd1)))
        (flet ((delete-handle-from! (ht)
                 (let ((cmd (gethash handle ht)))
                   (if cmd
                       (if (listp cmd)
                           ;; points and lines: rebuild index-array and remove handle
                           ;; todo: use memcpy if these index-arrays are big
                           (let* ((old-index-array (draw-list-index-array (car cmd)))
                                  (ptr (foreign-array-ptr old-index-array))
                                  (type (foreign-array-foreign-type old-index-array))
                                  (new-index-array (ecase type
                                                     (:unsigned-short
                                                      (make-unsigned-short-index-array
                                                       (index-array-allocated-count old-index-array)))
                                                     (:unsigned-int
                                                      (make-unsigned-int-index-array
                                                       (index-array-allocated-count old-index-array))))))
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
                                   when (eq entry cmd)
                                     do (setf (aref cmd-vector i) nil)
                                        (setf (draw-list-needs-compaction? draw-list) t)
                                        (remhash handle ht)
                                        (return))))
                       (warn "could find primitive ~S" handle)))))
          (sb-concurrency:enqueue #'(lambda ()
                                      (delete-handle-from! ht0))
                                  wq0)
          (sb-concurrency:enqueue #'(lambda ()
                                      (delete-handle-from! ht1))
                                  wq1)
          (values))))))
