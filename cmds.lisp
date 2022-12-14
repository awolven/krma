(in-package :krma)

(eval-when (:compile-toplevel :load-toplevel)
  (when krma::*debug*
    (declaim (optimize (safety 3) (debug 3)))))

(defstruct (essential-draw-indexed-cmd
            (:conc-name "CMD-")
            (:constructor make-essential-draw-indexed-cmd (draw-list first-idx elem-count vtx-offset)))
  (first-idx 0 :type (unsigned-byte 32))
  (elem-count 0 :type (unsigned-byte 32))
  (vtx-offset 0 :type (unsigned-byte 32))
  (draw-list nil))

(defmethod print-object ((object essential-draw-indexed-cmd) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ "first-idx: " stream)
    (princ (cmd-first-idx object) stream)
    (princ " elem-count: " stream)
    (princ (cmd-elem-count object) stream)
    (princ " vtx-offset: " stream)
    (princ (cmd-vtx-offset object) stream))
  object)

(defstruct (standard-draw-indexed-cmd
            (:include essential-draw-indexed-cmd)
            (:conc-name "CMD-")
            (:constructor make-standard-draw-indexed-cmd
                (draw-list first-idx elem-count vtx-offset
                 &optional (group nil) (model-mtx nil) (color-override nil) (texture *white-texture*)
		 (point-size nil) (line-thickness nil) (light-position nil))))
  (group)
  (model-mtx)
  (color-override)
  (texture)
  (point-size)
  (line-thickness)
  (light-position))

(defstruct (text-draw-indexed-cmd
            (:include standard-draw-indexed-cmd)
            (:conc-name "CMD-")
	    (:constructor make-text-draw-indexed-cmd
                (font draw-list first-idx elem-count vtx-offset
                 &optional (group nil) (model-mtx nil) (color-override nil) (texture *white-texture*)
                   (point-size nil) (line-thickness nil) (light-position nil))))
  (font))

(declaim (inline %resinstance-cmd-1))
(defun %reinstance-cmd-1 (cmd constructor new-draw-list first-idx elem-count vtx-offset
			  &optional group model-mtx sf-line-thickness sf-point-size ub32-color-override texture)
  (declare (type standard-draw-indexed-cmd cmd))
  (declare (type draw-list-mixin new-draw-list))
  (declare (type fixnum first-idx elem-count vtx-offset))
  (declare (type function constructor))
  (declare (type (or single-float null) sf-line-thickness sf-point-size))
  (declare (type (or (unsigned-byte 32) null) ub32-color-override))
  (declare (type (or mat4 null) model-mtx))
  (setq group (or group (cmd-group cmd)))
  (setq model-mtx (or model-mtx (cmd-model-mtx cmd)))
  (setq sf-line-thickness (or sf-line-thickness (cmd-line-thickness cmd)))
  (setq sf-point-size (or sf-point-size (cmd-point-size cmd)))
  (setq ub32-color-override (or ub32-color-override (cmd-color-override cmd)))
  (setq texture (or texture (cmd-texture cmd)))
  (let ((cmd (funcall constructor
                        new-draw-list
                        first-idx elem-count vtx-offset
                        group (when model-mtx (mcopy model-mtx))
                        ub32-color-override texture sf-line-thickness sf-point-size)))
    (vector-push-extend cmd (draw-list-cmd-vector new-draw-list))
    cmd))
