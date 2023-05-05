(in-package :krma)

(eval-when (:compile-toplevel :load-toplevel)
  (when krma::*debug*
    (declaim (optimize (safety 3) (debug 3)))))

(defun compact-draw-lists (dpy rm-draw-data)
  (declare (ignore dpy))
  (with-slots (2d-point-list-draw-list
	       2d-line-list-draw-list
	       2d-triangle-list-draw-list
	       2d-triangle-list-draw-list-for-text
	       3d-point-list-draw-list
	       3d-line-list-draw-list
	       3d-triangle-list-draw-list
	       3d-triangle-list-with-normals-draw-list
	       2d-line-strip-draw-list
	       2d-triangle-strip-draw-list
	       3d-line-strip-draw-list
	       3d-triangle-strip-draw-list
	       3d-triangle-strip-with-normals-draw-list) rm-draw-data
    
    (setf 2d-point-list-draw-list (compact-draw-list 2d-point-list-draw-list)
	  2d-line-list-draw-list (compact-draw-list 2d-line-list-draw-list)
	  2d-triangle-list-draw-list (compact-draw-list 2d-triangle-list-draw-list)
	  2d-triangle-list-draw-list-for-text (compact-draw-list 2d-triangle-list-draw-list-for-text)
	  3d-point-list-draw-list (compact-draw-list 3d-point-list-draw-list)
	  3d-line-list-draw-list (compact-draw-list 3d-line-list-draw-list)
	  3d-triangle-list-draw-list (compact-draw-list 3d-triangle-list-draw-list)
	  3d-triangle-list-with-normals-draw-list (compact-draw-list 3d-triangle-list-with-normals-draw-list)
	  2d-line-strip-draw-list (compact-draw-list 2d-line-strip-draw-list)
	  2d-triangle-strip-draw-list (compact-draw-list 2d-triangle-strip-draw-list)
	  3d-line-strip-draw-list (compact-draw-list 3d-line-strip-draw-list)
	  3d-triangle-strip-draw-list (compact-draw-list 3d-triangle-strip-draw-list)
	  3d-triangle-strip-with-normals-draw-list (compact-draw-list 3d-triangle-strip-with-normals-draw-list))
    
    (values)))

(defun copy-vertex (old-vertex-array old-vertex-offset new-vertex-array new-vertex-offset vertex-type-size-bytes)
  (let ((vertex-size-in-uints (ash vertex-type-size-bytes -2))
	(new-lisp-array (foreign-array-bytes new-vertex-array))
	(old-lisp-array (foreign-array-bytes old-vertex-array)))
    ;;(print "-----------")
    (loop for i from (* new-vertex-offset vertex-size-in-uints)
       for j from (* old-vertex-offset vertex-size-in-uints)
       repeat vertex-size-in-uints
       do (setf (aref new-lisp-array i) (aref old-lisp-array j))))
  ;;(print "-----------")
  ;;(finish-output)
  (values))

(defun compact-draw-list (draw-list)
  
  (unless (draw-list-needs-compaction? draw-list)
    (return-from compact-draw-list draw-list))

  (let ((cmd-vector (draw-list-cmd-vector draw-list)))
      
    (when (zerop (fill-pointer cmd-vector))
      ;; it a primitive draw-list with no cmds, which doesn't need to be compacted
      (setf (draw-list-needs-compaction? draw-list) nil)
      (return-from compact-draw-list draw-list))
	      
    (format *debug-io* "~%info: compacting draw-list")
    (force-output *debug-io*)
      
    (let* ((old-index-array (draw-list-index-array draw-list))
	   (old-vertex-array (draw-list-vertex-array draw-list))
	   (new-draw-list
	    (make-instance (class-of draw-list)
			   :index-array (make-index-array (foreign-array-fill-pointer old-index-array)
							  :unsigned-short)
			   :vertex-array
			   (funcall (intern (concatenate 'string "MAKE-"
							 (symbol-name (type-of old-vertex-array)))
					    (symbol-package (type-of old-vertex-array)))
				    (foreign-array-fill-pointer old-vertex-array))
			   :index-memory (draw-list-index-memory draw-list)
			   :index-size-aligned (draw-list-index-size-aligned draw-list)
			   :vertex-memory (draw-list-vertex-memory draw-list)
			   :vertex-size-aligned (draw-list-vertex-size-aligned draw-list)))
	   (new-index-array (draw-list-index-array new-draw-list))
	   (new-vertex-array (draw-list-vertex-array new-draw-list))
	   (vertex-type-size (foreign-array-foreign-type-size old-vertex-array)))
						    
      (loop for cmd across cmd-vector
	 ;;do (print "-")
	 when cmd
	 do ;;(print "----")
	   (loop repeat (cmd-elem-count cmd)
	      for i from (cmd-first-idx cmd)
	      with old-vtx-offset = (cmd-vtx-offset cmd)
	      with new-first-idx = (foreign-array-fill-pointer new-index-array)
	      with new-vtx-offset = (foreign-array-fill-pointer new-vertex-array)
	      with max-local-offset = 0
	      with seen = ()
	      do (let ((local-offset (aref (foreign-array-bytes old-index-array) i)))

		   ;;(print local-offset)
		   
		   (index-array-push-extend new-index-array local-offset)

		   (setq max-local-offset (max local-offset max-local-offset))
			    
		   ;; no vertex should be copied twice
		   (unless (member local-offset seen :test #'=)
		     ;; copy one vertex into new vertex array
		     (copy-vertex old-vertex-array (+ old-vtx-offset local-offset)
				  new-vertex-array (+ new-vtx-offset local-offset)
				  vertex-type-size)
			      
		     ;; we've now seen this vertex
		     (push local-offset seen)))
			 
	      finally ;; update the cmd with the new-draw-list, new-first-idx and new-vtx-offset
	      ;; we recycle cmd objects because they exist in the primitive handle hash table
	      ;; and we don't want to have to fix those relationships
		(setf (cmd-draw-list cmd) new-draw-list)
		(setf (cmd-first-idx cmd) new-first-idx)
		(setf (cmd-vtx-offset cmd) new-vtx-offset)
		(vector-push-extend cmd (draw-list-cmd-vector new-draw-list))

		;;(finish-output)

		;;(print ".")
		(incf (foreign-array-fill-pointer new-vertex-array) (1+ max-local-offset)))
			
	 finally (return new-draw-list)))))
