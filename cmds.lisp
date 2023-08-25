(in-package :krma)

(eval-when (:compile-toplevel :load-toplevel)
  (when krma::*debug*
    (declaim (optimize (safety 3) (debug 3))))
  (unless krma::*debug*
    (declaim (optimize (speed 3) (safety 0) (debug 0)))
    (declaim (inline %resinstance-cmd-1))))

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
		   (point-size nil) (line-thickness nil) (material *default-material*)
		   (elevation 0))))
  (group)
  (model-mtx)
  (color-override)
  (texture)
  (point-size)
  (line-thickness)
  (material)
  (elevation))

(defstruct (text-draw-indexed-cmd
	     (:include standard-draw-indexed-cmd)
	     (:conc-name "CMD-")
	     (:constructor make-text-draw-indexed-cmd
			   (font draw-list first-idx elem-count vtx-offset
				 &optional (group nil) (model-mtx nil) (color-override nil) (texture *white-texture*)
				 (point-size nil) (line-thickness nil) (material nil) (elevation 0))))
  (font))

(defun %reinstance-cmd-1 (cmd new-draw-list first-idx elem-count vtx-offset
			  &key
			    (group (cmd-group cmd))
			    (model-mtx (cmd-model-mtx cmd))
			    (ub32-color-override (cmd-color-override cmd))
			    (texture (cmd-texture cmd))
			    (material (cmd-material cmd))
			    (sf-point-size (cmd-point-size cmd))
			    (sf-line-thickness (cmd-line-thickness cmd))
			    (elevation (cmd-elevation cmd)))
  (let ((cmd (apply (etypecase cmd
		      (text-draw-indexed-cmd #'make-text-draw-indexed-cmd)
		      (standard-draw-indexed-cmd #'make-standard-draw-indexed-cmd))
		    (append (when (typep cmd 'text-draw-indexed-cmd)
			      (list (cmd-font cmd)))
			    (list new-draw-list
				  first-idx elem-count vtx-offset
				  group (when model-mtx (mcopy model-mtx))
				  ub32-color-override
				  texture
				  sf-point-size
				  sf-line-thickness
				  material
				  elevation)))))
    (vector-push-extend cmd (draw-list-cmd-vector new-draw-list))
    cmd))
