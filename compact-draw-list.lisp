(in-package :krma)

(eval-when (:compile-toplevel :load-toplevel)
  (when krma::*debug*
    (declaim (optimize (safety 3) (debug 3)))))

(defun compact-draw-lists (dpy rm-draw-data)
  (let ((pipeline-store (krma-pipeline-store dpy)))
    (loop for (p dl) on (3d-cmd-oriented-combinations pipeline-store rm-draw-data) by #'cddr
	  do (compact-draw-list dl))
    (loop for (p dl) on (2d-cmd-oriented-combinations pipeline-store rm-draw-data) by #'cddr
	  do (compact-draw-list dl))))

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
               (vertex-type-size (foreign-array-foreign-type-size old-vertex-array)))

	  (format *debug-io* "~%info: compacting draw-list")
	  (force-output *debug-io*)

          (%prim-reserve new-draw-list
			 (foreign-array-fill-pointer old-vertex-array)
			 (foreign-array-fill-pointer old-index-array)
			 (foreign-array-foreign-type-size old-vertex-array)
			 (foreign-array-foreign-type-size old-index-array))

          (cond ((zerop (fill-pointer cmd-vector))
                 ;; case #1: it's either a primitive draw-list with no cmds, which doesn't need to be compacted
		 ;; or its a group draw-list which has no commands and therefore doesn't need to be compacted
		 ;; group draw lists are deleted in their entirety
                 draw-list)

                (t ;; case #2: primitive draw-list with cmds
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
                                  do (setq orig-idx (aref (foreign-array-bytes old-index-array) i))
                                  when (setq translation (gethash orig-idx translation-table))
                                    do ;; if there is a translation of index, add it to index-array at fill-pointer
                                       (index-array-push-extend new-index-array translation)
                                  unless translation
                                    do ;; copy one vertex into new vertex array at index (1+ new-vtx-offset)
                                       ;; no vertex should be copied twice, but some may not be copied at all
                                       ;; it doesn't matter what the vertex-array fill-pointer is. we'll adjust
                                       ;; that at the end
				       (let ((vertex-size-in-uints (ash vertex-type-size -2))
					     (new-lisp-array (foreign-array-bytes new-vertex-array))
					     (old-lisp-array (foreign-array-bytes old-vertex-array)))
					 #+sbcl(sb-sys:with-pinned-objects (new-lisp-array old-lisp-array)
					     (let ((new-lisp-array-ptr (sb-sys:vector-sap new-lisp-array))
						   (old-lisp-array-ptr (sb-sys:vector-sap old-lisp-array)))
					       (vk::memcpy (inc-pointer new-lisp-array-ptr (* (incf new-vtx-offset) vertex-size-in-uints))
							   (inc-pointer old-lisp-array-ptr (* (+ cmd-vtx-offset orig-idx) vertex-size-in-uints))
							   (* 1 vertex-type-size))))
				       #+CCL
					 (ccl::%copy-ivector-to-ivector old-lisp-array (* (+ cmd-vtx-offset orig-idx) vertex-size-in-uints)
									new-lisp-array (* (incf new-vtx-offset) vertex-size-in-uints)
									(* 1 vertex-type-size)))
                                       ;; put new index into new index array at fill-pointer
                                       (index-array-push-extend new-index-array new-vtx-offset)
                                       ;; record the translation
                                       (setf (gethash orig-idx translation-table) new-vtx-offset)
                                  finally ;; copy the cmd, except update the first-index and vtx-offset
					  (%reinstance-cmd-1 cmd 
							     cmd-constructor
							     new-draw-list
							     (foreign-array-fill-pointer new-index-array)
							     (cmd-elem-count cmd)
							     ;; the vertex array fp hasn't been updated yet
							     ;; and we want to use it as is anyway
							     ;; we'll update it below for use in the next cmd
							     (foreign-array-fill-pointer new-vertex-array))
                                          ;; finally update the vertex array fp so that vertex-pushes work in the future
                                          (setf (foreign-array-fill-pointer new-vertex-array) (1+ new-vtx-offset)))
                       finally (setf (draw-list-needs-compaction? draw-list) nil)
			       (return new-draw-list))))))))
