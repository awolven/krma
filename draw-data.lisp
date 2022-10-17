(in-package :krma)

(defstruct (standard-draw-data
            (:conc-name "DRAW-DATA-")
            (:constructor make-standard-draw-data (name)))
  (name)
  (3d-point-list-draw-list (make-instance '3d-vertex-small-draw-list))
  (3d-line-list-draw-list (make-instance '3d-vertex-small-draw-list))
  (3d-line-strip-draw-list (make-instance '3d-vertex-small-draw-list))
  (3d-triangle-list-draw-list (make-instance '3d-vertex-small-draw-list))
  (3d-triangle-strip-draw-list (make-instance '3d-vertex-small-draw-list))
  (2d-point-list-draw-list (make-instance '2d-vertex-small-draw-list))
  (2d-line-list-draw-list (make-instance '2d-vertex-small-draw-list))
  (2d-line-strip-draw-list (make-instance '2d-vertex-small-draw-list))
  (2d-triangle-list-draw-list (make-instance '2d-vertex-small-draw-list))
  (2d-triangle-strip-draw-list (make-instance '2d-vertex-small-draw-list)))

(defstruct (retained-mode-draw-data
            (:include standard-draw-data)
            (:conc-name "DRAW-DATA-")
            (:constructor make-retained-mode-draw-data (name)))
  (handle-hash-table (make-hash-table :test #'eq))
  (work-queue (sb-concurrency:make-queue :name "draw-data-work-queue")))

(defun %draw-data-add-2d-point (draw-data handle color x y)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-2d-point-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (list draw-list (%draw-list-add-2d-point draw-list color x y)))
    (values)))

(defun %draw-data-add-3d-point (draw-data handle color x y z)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-point-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (list draw-list (%draw-list-add-3d-point draw-list color x y z)))
    (values)))

(defun %draw-data-add-2d-line (draw-data handle color x0 y0 x1 y1)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-2d-line-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (list* draw-list (%draw-list-add-2d-line draw-list color x0 y0 x1 y1)))
    (values)))

(defun %draw-data-add-3d-line (draw-data handle color x0 y0 z0 x1 y1 z1)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-line-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (list* draw-list (%draw-list-add-3d-line draw-list color x0 y0 z0 x1 y1 z1)))
    (values)))

(defun %draw-data-add-multicolor-2d-polyline-cmd-1 (draw-data handle closed? model-mtx line-thickness vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-2d-line-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-multicolor-2d-polyline-cmd-1
           draw-list
           closed? model-mtx line-thickness vertices))
    (values)))

(defun %draw-data-add-2d-polyline-cmd-1 (draw-data handle closed? model-mtx line-thickness color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-2d-line-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-2d-polyline-cmd-1
           draw-list
           closed? model-mtx line-thickness color vertices))
    (values)))

(defun %draw-data-add-2d-triangle-cmd (draw-data handle model-mtx line-thickness color x0 y0 x1 y1 x2 y2)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-2d-line-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-2d-triangle-cmd
           draw-list
           model-mtx line-thickness color x0 y0 x1 y1 x2 y2))
    (values)))

(defun %draw-data-add-2d-rectangle-cmd (draw-data handle model-mtx line-thickness color x0 y0 x1 y1)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-2d-line-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-2d-rectangle-cmd
           draw-list
           model-mtx line-thickness color x0 y0 x1 y1))
    (values)))

(defun %draw-data-add-2d-circular-arc-cmd (draw-data handle closed? model-mtx line-thickness color
                                           center-x center-y radius start-angle end-angle
                                           number-of-segments)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-2d-line-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-2d-circular-arc-cmd
           draw-list
           closed? model-mtx line-thickness color
           center-x center-y radius start-angle end-angle
           number-of-segments))
    (values)))

(defun %draw-data-add-2d-circle-cmd (draw-data handle model-mtx line-thickness color
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

(defun %draw-data-add-multicolor-3d-polyline-cmd-1 (draw-data handle closed? model-mtx line-thickness vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-line-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-multicolor-3d-polyline-cmd-1
           draw-list
           closed? model-mtx line-thickness vertices))
    (values)))

(defun %draw-data-add-3d-polyline-cmd-1 (draw-data handle model-mtx closed? line-thickness color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-line-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-3d-polyline-cmd-1
           draw-list
           model-mtx closed? line-thickness color vertices))
    (values)))

(defun %draw-data-add-filled-2d-triangle-list-cmd (draw-data handle model-mtx color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list (draw-data-2d-triangle-list-draw-list draw-data))
         (maybe-cmd (%draw-list-add-filled-2d-triangle-list-cmd
                     draw-list
                     model-mtx color vertices)))
    (when maybe-cmd
      (setf (gethash handle (draw-data-handle-hash-table draw-data))
            maybe-cmd))
    (values)))

(defun %draw-data-add-filled-2d-rectangle-list-cmd (draw-data handle model-mtx color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let* ((draw-list (draw-data-2d-triangle-list-draw-list draw-data))
         (maybe-cmd (%draw-list-add-filled-2d-rectangle-list-cmd
           draw-list
           model-mtx color vertices)))
    (when maybe-cmd
      (setf (gethash handle (draw-data-handle-hash-table draw-data))
            maybe-cmd))
    (values)))

(defun %draw-data-add-textured-2d-rectangle-list-cmd (draw-data handle model-mtx texture color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-2d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-textured-2d-rectangle-list-cmd
           draw-list
           model-mtx texture color vertices))
    (values)))

(defun %draw-data-add-filled-2d-polygon-cmd (draw-data handle model-mtx color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-2d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-2d-polygon-cmd
           draw-list
           model-mtx color vertices))
    (values)))

(defun %draw-data-add-filled-3d-triangle-list-cmd (draw-data handle model-mtx color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-3d-triangle-strip-cmd
           draw-list
           model-mtx color vertices))
    (values)))

(defun %draw-data-add-filled-3d-triangle-strip-cmd (draw-data handle model-mtx color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-triangle-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-3d-triangle-strip-cmd
           draw-list
           model-mtx color vertices))
    (values)))

(defun %draw-data-add-filled-3d-triangle-list-with-normals-cmd (draw-data handle model-mtx color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-3d-triangle-strip-with-normals-cmd
           draw-list
           model-mtx color vertices))
    (values)))

(defun %draw-data-add-filled-3d-triangle-strip-with-normals-cmd (draw-data handle model-mtx color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-triangle-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-filled-3d-triangle-strip-with-normals-cmd
           draw-list
           model-mtx color vertices))
    (values)))

(defun %draw-data-add-textured-3d-triangle-list-cmd (draw-data handle model-mtx texture color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-triangle-list-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-textured-3d-triangle-strip-cmd
           draw-list
           model-mtx texture color vertices))
    (values)))

(defun %draw-data-add-textured-3d-triangle-strip-cmd (draw-data handle model-mtx texture color vertices)
  (declare (type retained-mode-draw-data draw-data))
  (let ((draw-list (draw-data-3d-triangle-strip-draw-list draw-data)))
    (setf (gethash handle (draw-data-handle-hash-table draw-data))
          (%draw-list-add-textured-3d-triangle-strip-cmd
           draw-list
           model-mtx texture color vertices))
    (values)))
