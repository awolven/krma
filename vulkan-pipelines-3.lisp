(in-package :krma)

(defun ubershader-render-draw-list-cmds (pipeline draw-data draw-list dpy device command-buffer scene window view proj x y width height near far releaseme-queue)
  
  (declare (type draw-indexed-pipeline-mixin pipeline))
  (declare (type draw-list-mixin draw-list))
  (declare (type standard-draw-data draw-data))
  (declare (ignorable device))

  (let* ((pipeline-layout (pipeline-layout pipeline))
         (command-buffer-handle (h command-buffer))
	 (mm))
	    
    (cmd-bind-pipeline command-buffer (device-pipeline pipeline) :bind-point :graphics)

    ;; apparently, updating uniform buffers have no effect if done before cmd-bind-pipeline
    (update-vertex-uniform-buffer pipeline view proj width height near far)

    (update-fragment-uniform-buffer pipeline scene window (car (current-frame-cons dpy)))

    (cmd-set-viewport command-buffer :x x :y y :width width :height height
				     :min-depth 0.0 :max-depth 1.0)
    (cmd-set-scissor command-buffer :x x :y y :width width :height height)

    (with-foreign-objects ((p-descriptor-sets :pointer 1))
      (setf (mem-aref p-descriptor-sets :pointer 0) (h (global-descriptor-set pipeline)))
      (vkCmdBindDescriptorSets (h command-buffer)
                               VK_PIPELINE_BIND_POINT_GRAPHICS
                               (h pipeline-layout)
                               0 1
                               p-descriptor-sets
                               0 +nullptr+))
	    
    (with-foreign-objects ((p-descriptor-sets :pointer 1))
      (setf (mem-aref p-descriptor-sets :pointer 0) (h (aref (krma-select-boxes-descriptor-sets window) (car (current-frame-cons dpy)))))
      (vkCmdBindDescriptorSets (h command-buffer)
                               VK_PIPELINE_BIND_POINT_GRAPHICS
                               (h pipeline-layout)
                               1 1
                               p-descriptor-sets
                               0 +nullptr+))

    

    (flet ((render-standard-draw-indexed-cmd (cmd &aux (pipeline-default-font nil))
	     (declare (type standard-draw-indexed-cmd cmd))
	     
	     (let* ((descriptor-set (texture-image-descriptor-set
				     (or (cmd-texture cmd)
					 (draw-list-texture draw-list)
					 (and (setq pipeline-default-font
						    (pipeline-default-font pipeline))
					      (font-atlas pipeline-default-font))
					 *white-texture*)))
		    (group (when (cmd-group cmd) (gethash (cmd-group cmd) (draw-data-group-hash-table draw-data))))
		    (group-model-matrix (and group (group-model-matrix group)))
		    (group-color-override (and group (group-color-override group))))
		       
	       (with-foreign-objects ((p-descriptor-sets :pointer 1))
                 (setf (mem-aref p-descriptor-sets :pointer 0) (h descriptor-set))
                 (vkCmdBindDescriptorSets (h command-buffer)
                                          VK_PIPELINE_BIND_POINT_GRAPHICS
                                          (h pipeline-layout)
                                          2 1
                                          p-descriptor-sets
                                          0 +nullptr+))

	       (with-foreign-object (pvalues :uint32 +uber-vertex-shader-pc-size+)

		 (let ((cmd-model-matrix (cmd-model-mtx cmd))
		       (cmd-color-override (cmd-color-override cmd)))

		   (if cmd-model-matrix
		       (setq mm cmd-model-matrix)
		       (if group-model-matrix
			   (setq mm group-model-matrix)
			   (setq mm *identity-matrix*))
		       #+NIL
		       (if group-model-matrix
			   (setq mm (m* cmd-model-matrix group-model-matrix))
			   (setq mm (m* cmd-model-matrix)))
		       #+NIL
		       (if group-model-matrix
			   (setq mm (m* group-model-matrix))
			   (setq mm *identity-matrix*)))

		   ;;(print mm)

		   (copy-matrix-to-foreign mm pvalues)
			   
		   (if cmd-color-override
		       (let ((pcol (mem-aptr pvalues :uint32 +uber-vertex-shader-color-override-offset+)))
			 (setf (mem-aref pcol :uint32 0) cmd-color-override)
			 (setf (mem-aref pvalues :uint32 +uber-vertex-shader-override-color-p-offset+) 1))
			       
		       (if group-color-override
			   (let ((pcol (mem-aptr pvalues :uint32 +uber-vertex-shader-color-override-offset+)))
			     (setf (mem-aref pcol :uint32 0) group-color-override)
			     (setf (mem-aref pvalues :uint32 +uber-vertex-shader-override-color-p-offset+) 1))
			   (setf (mem-aref pvalues :uint32 +uber-vertex-shader-override-color-p-offset+) 0)))

		   (cond ((or (typep pipeline 'point-list-pipeline-mixin)
			      (typep pipeline '2d-instanced-line-pipeline)
			      (typep pipeline '3d-instanced-tube-pipeline)
			      (typep pipeline 'foreground-3d-instanced-line-pipeline))
			  (setf (mem-aref pvalues :uint32 +uber-vertex-shader-primitive-type-offset+) 0)
			  (let ((psize (mem-aptr pvalues :uint32 +uber-vertex-shader-point-size-offset+)))
			    (let ((cmd-point-size (cmd-point-size cmd)))
			      (if cmd-point-size
				  (setf (mem-aref psize :float) cmd-point-size)
				  (let ((pipeline-point-size (pipeline-point-size pipeline)))
				    (if pipeline-point-size
					(setf (mem-aref psize :float) pipeline-point-size)
					(setf (mem-aref psize :float) *default-point-size*)))))))

			 ((typep pipeline 'line-pipeline-mixin)
			  (setf (mem-aref pvalues :uint32 +uber-vertex-shader-primitive-type-offset+) 1)
			  #-darwin
			  (let ((cmd-line-width (cmd-line-thickness cmd)))
			    (if cmd-line-width
				(vkCmdSetLineWidth command-buffer-handle cmd-line-width)
				(let ((pipeline-line-width (pipeline-line-width pipeline)))
				  (if pipeline-line-width
				      (vkCmdSetLineWidth command-buffer-handle pipeline-line-width)
				      (vkCmdSetLineWidth command-buffer-handle *default-line-thickness*))))))
			 ((typep cmd 'text-draw-indexed-cmd)
			  (setf (mem-aref pvalues :uint32 +uber-vertex-shader-primitive-type-offset+) 3))
				  
			 (t (setf (mem-aref pvalues :uint32 +uber-vertex-shader-primitive-type-offset+) 2)))

		   (let ((cmd-instance-array (cmd-instance-array cmd)))
		     (if (and cmd-instance-array
			      (not (zerop (foreign-array-fill-pointer
					   (instance-list-array cmd-instance-array)))))
			 (progn
			   (initialize-instance-list-buffer device cmd-instance-array)
			   ;;(print (foreign-array-bytes (instance-list-array cmd-instance-array)))
			   (let* ((memory-block (instance-list-memory cmd-instance-array))
				  (mrb (memory-block-buffer memory-block))
				  (mro (memory-block-offset memory-block)))
			     (%vk::with-vkBufferDeviceAddressInfo (p-info)
			       (setf %vk::buffer (h mrb))
			       (setf (mem-ref pvalues :uint64 (* +uber-vertex-shader-instance-array-pointer-offset+ (foreign-type-size :uint32)))
				     (+ mro (%vk::vkGetBufferDeviceAddress (h (default-logical-device dpy)) p-info))))))
			 (setf (mem-ref pvalues :uint64 (* +uber-vertex-shader-instance-array-pointer-offset+ (foreign-type-size :uint32))) 0)))
			   
		   (vkCmdPushConstants command-buffer-handle
				       (h pipeline-layout)
				       VK_SHADER_STAGE_VERTEX_BIT
				       0
				       (load-time-value (* +uber-vertex-shader-pc-size+
							   (foreign-type-size :uint32)))
				       pvalues)))


	       (with-foreign-object (pvalues2 :float +fragment-shader-pc-size+)
		 (when (typep cmd 'text-draw-indexed-cmd)
		   (let ((font (or (text-cmd-font cmd) pipeline-default-font)))
		     (when font
		       (let ((px-range (font-px-range font)))
			 (if px-range
			     (setf (mem-aref pvalues2 :float +text-fragment-shader-px-range-offset+) (clampf px-range))
			     (setf (mem-aref pvalues2 :float +text-fragment-shader-px-range-offset+) 32.0f0))))))

		 (setf (mem-aref pvalues2 :float +fragment-shader-select-box-min-offset+) (clampf (krma-select-box-x0 window))
		       (mem-aref pvalues2 :float (1+ +fragment-shader-select-box-min-offset+)) (clampf (krma-select-box-y0 window))
		       (mem-aref pvalues2 :float +fragment-shader-select-box-max-offset+) (clampf (krma-select-box-x1 window))
		       (mem-aref pvalues2 :float (1+ +fragment-shader-select-box-max-offset+)) (clampf (krma-select-box-y1 window)))

		 (when (typep pipeline '3d-texture-with-normals-pipeline-mixin)
		   (let* ((cmd-material (cmd-material cmd))
			  (material (if cmd-material
					cmd-material
					(if group
					    (group-material group)
					    *default-material*)))
			  (ambient (material-ambient material))
			  (diffuse (material-diffuse material))
			  (specular (material-specular material))
			  (shininess (material-shininess material)))

		     (setf (mem-aref pvalues2 :unsigned-int +lighting-fragment-shader-ambient-offset+) (canonicalize-color ambient)
			   (mem-aref pvalues2 :unsigned-int +lighting-fragment-shader-diffuse-offset+) (canonicalize-color diffuse)
			   (mem-aref pvalues2 :unsigned-int +lighting-fragment-shader-specular-offset+) (canonicalize-color specular)
			   (mem-ref pvalues2 :float (load-time-value
						     (* +lighting-fragment-shader-shininess-offset+ (foreign-type-size :float))))
			   (clampf shininess))))				 

		 ;; make sure msdf-texture fragment shader can get px-range from font
		 (vkCmdPushConstants command-buffer-handle
				     (h pipeline-layout)
				     VK_SHADER_STAGE_FRAGMENT_BIT
				     (load-time-value (* +uber-vertex-shader-pc-size+
							 (foreign-type-size :uint32)))
				     (load-time-value (* +fragment-shader-pc-size+
							 (foreign-type-size :float)))
				     pvalues2))

	       (vkCmdDrawIndexed command-buffer-handle
				 (cmd-elem-count cmd) (if (cmd-instance-array cmd)
							  (max
							   (instance-list-count
							    (cmd-instance-array cmd))
							   0)
							  1)
				 (cmd-first-idx cmd) (cmd-vtx-offset cmd)
				 0))))

      (do ((draw-list draw-list (draw-list-prev draw-list)))
	  ((null draw-list))
	
	(let ((index-array (draw-list-index-array draw-list)))
	  (declare (type foreign-adjustable-array index-array))
	
	  (unless (= 0 (foreign-array-fill-pointer index-array))
	    
	    (let ((cmd-vector (draw-list-cmd-vector draw-list)))
	      (declare (type (vector t) cmd-vector))
	    
	      (unless (= 0 (fill-pointer cmd-vector))
	      
		(if (typep draw-data 'immediate-mode-draw-data)
		    (initialize-draw-list-vram-quick device draw-list releaseme-queue)
		    (maybe-initialize-draw-list-vram device draw-list window releaseme-queue))

		(unless (or (draw-list-vertex-memory draw-list)
			    (draw-list-index-memory draw-list))
		  (print "missing memory block")
		  (finish-output))

		(when (and (draw-list-vertex-memory draw-list)
			   (draw-list-index-memory draw-list))
		  
		  (cmd-bind-vertex-buffers
		   command-buffer (list (memory-block-buffer (draw-list-vertex-memory draw-list)))
		   (list (memory-block-offset (draw-list-vertex-memory draw-list))))
		  (cmd-bind-index-buffer
		   command-buffer (memory-block-buffer (draw-list-index-memory draw-list))
		   (memory-block-offset (draw-list-index-memory draw-list))
		   (foreign-array-foreign-type index-array))
		  
		  (loop for cmd across cmd-vector
			when cmd
			  do (render-standard-draw-indexed-cmd cmd))
		  ))))))
      t)))

(defun ubershader-render-draw-list (pipeline draw-data draw-list dpy device command-buffer scene window view proj x y width height near far releaseme-queue)
  
  (declare (type draw-indexed-pipeline-mixin))
  (declare (type draw-list-mixin draw-list))
  (declare (ignorable device))

  (let ((index-array (draw-list-index-array draw-list)))
    (declare (type foreign-adjustable-array index-array))

    (unless (= 0 (foreign-array-fill-pointer index-array))

      (if (typep draw-data 'immediate-mode-draw-data)
	  (initialize-draw-list-vram-quick device draw-list releaseme-queue)
	  (maybe-initialize-draw-list-vram device draw-list window releaseme-queue))
      
      (let* ((command-buffer-handle (h command-buffer))
             (pipeline-layout (pipeline-layout pipeline))
	     (group (draw-list-group draw-list))
             (mm))

	(declare (type (or group null) group))

	(cmd-bind-pipeline command-buffer (device-pipeline pipeline) :bind-point :graphics)

	(update-vertex-uniform-buffer pipeline view proj width height near far)

	(update-fragment-uniform-buffer pipeline scene window (car (current-frame-cons dpy)))

	(cmd-set-viewport command-buffer :x x :y y :width width :height height
					 :min-depth 0.0 :max-depth 1.0)
	
	(cmd-set-scissor command-buffer :x x :y y :width width :height height)

	(with-foreign-objects ((p-descriptor-sets :pointer 1))
          (setf (mem-aref p-descriptor-sets :pointer 0) (h (global-descriptor-set pipeline)))
          (vkCmdBindDescriptorSets (h command-buffer)
                                   VK_PIPELINE_BIND_POINT_GRAPHICS
                                   (h pipeline-layout)
                                   0 1
                                   p-descriptor-sets
                                   0 +nullptr+))

	(with-foreign-objects ((p-descriptor-sets :pointer 1))
          (setf (mem-aref p-descriptor-sets :pointer 0) (h (aref (krma-select-boxes-descriptor-sets window) (car (current-frame-cons dpy)))))
          (vkCmdBindDescriptorSets (h command-buffer)
                                   VK_PIPELINE_BIND_POINT_GRAPHICS
                                   (h pipeline-layout)
                                   1 1
                                   p-descriptor-sets
                                   0 +nullptr+))
	
        (cmd-bind-vertex-buffers command-buffer (list (memory-block-buffer (draw-list-vertex-memory draw-list)))
                                 (list (memory-block-offset (draw-list-vertex-memory draw-list))))
        (cmd-bind-index-buffer command-buffer (memory-block-buffer (draw-list-index-memory draw-list))
                               (memory-block-offset (draw-list-index-memory draw-list)) (foreign-array-foreign-type index-array))

	(let ((descriptor-set (texture-image-descriptor-set (or (draw-list-texture draw-list)
								(if (pipeline-default-font pipeline)
                                                                    (font-atlas (pipeline-default-font pipeline))
                                                                    *white-texture*)))))
          (with-foreign-objects ((p-descriptor-sets :pointer 1))
            (setf (mem-aref p-descriptor-sets :pointer 0) (h descriptor-set))
            (vkCmdBindDescriptorSets (h command-buffer)
                                     VK_PIPELINE_BIND_POINT_GRAPHICS
                                     (h pipeline-layout)
                                     2 1
                                     p-descriptor-sets
                                     0 +nullptr+)))

	(with-foreign-object (pvalues :uint32 +uber-vertex-shader-pc-size+)

	  (if group
	      (progn
		(let ((group-model-matrix (group-model-matrix group)))
		  
		  (if group-model-matrix
		      (setq mm group-model-matrix)
		      (setq mm *identity-matrix*))
		  
		  (copy-matrix-to-foreign mm pvalues))

		(let ((group-color-override (group-color-override group)))
		  (if group-color-override
		      (let ((pcol (mem-aptr pvalues :uint32 +uber-vertex-shader-color-override-offset+)))
			(setf (mem-aref pcol :uint32 0) group-color-override)
			(setf (mem-aref pvalues :uint32 +uber-vertex-shader-override-color-p-offset+) 1))
		      (setf (mem-aref pvalues :uint32 +uber-vertex-shader-override-color-p-offset+) 0))))
	      (progn
		(setf (mem-aref pvalues :uint32 +uber-vertex-shader-override-color-p-offset+) 0)
		(copy-matrix-to-foreign *identity-matrix* pvalues)))

	  (cond ((typep pipeline 'point-list-pipeline-mixin)
		 (setf (mem-aref pvalues :uint32 +uber-vertex-shader-primitive-type-offset+) 0)
                 (let ((psize (mem-aptr pvalues :uint32 +uber-vertex-shader-point-size-offset+)))
		   (let ((draw-list-point-size (draw-list-point-size draw-list)))
		     (if draw-list-point-size
			 (setf (mem-aref psize :float) (draw-list-point-size draw-list))
			 (let ((pipeline-point-size (pipeline-point-size pipeline)))
			   (if pipeline-point-size
			       (setf (mem-aref psize :float) pipeline-point-size)
			       (setf (mem-aref psize :float) *default-point-size*)))))))
		
		((typep pipeline 'line-pipeline-mixin)
		 (setf (mem-aref pvalues :uint32 +uber-vertex-shader-primitive-type-offset+) 1)

		 #-darwin
		 (let ((draw-list-line-width (draw-list-line-thickness draw-list)))
		   (if draw-list-line-width
		       (vkCmdSetLineWidth command-buffer-handle draw-list-line-width)
		       (let ((pipeline-line-width (pipeline-line-width pipeline)))
			 (if pipeline-line-width
			     (vkCmdSetLineWidth command-buffer-handle pipeline-line-width)
			     (vkCmdSetLineWidth command-buffer-handle *default-line-thickness*))))))

		((draw-list-font draw-list)
		 (setf (mem-aref pvalues :uint32 +uber-vertex-shader-primitive-type-offset+) 3))
		
		(t (setf (mem-aref pvalues :uint32 +uber-vertex-shader-primitive-type-offset+) 2)))

          ;; apperently the call to vkCmdPushConstants for vertex-shader must happen
          ;; before the call to vkCmdPushConstants for fragment-shader even though
          ;; the locations/positions are right
	  (vkCmdPushConstants command-buffer-handle
			      (h pipeline-layout)
			      VK_SHADER_STAGE_VERTEX_BIT
			      0
			      (load-time-value (* +uber-vertex-shader-pc-size+
						  (foreign-type-size :uint32)))
			      pvalues)

          
	  (with-foreign-object (pvalues2 :float +fragment-shader-pc-size+)
	    (let ((font (draw-list-font draw-list)))
	      (when font
		(let ((px-range (font-px-range font)))
		  (if px-range
		      (setf (mem-aref pvalues2 :float +text-fragment-shader-px-range-offset+) (clampf px-range))
		      (setf (mem-aref pvalues2 :float +text-fragment-shader-px-range-offset+) 32.0f0)))))

	    (setf (mem-aref pvalues2 :float +fragment-shader-select-box-min-offset+) (clampf (krma-select-box-x0 window))
		  (mem-aref pvalues2 :float (1+ +fragment-shader-select-box-min-offset+)) (clampf (krma-select-box-y0 window))
		  (mem-aref pvalues2 :float +fragment-shader-select-box-max-offset+) (clampf (krma-select-box-x1 window))
		  (mem-aref pvalues2 :float (1+ +fragment-shader-select-box-max-offset+)) (clampf (krma-select-box-y1 window)))
	    

	    (when (typep pipeline '3d-texture-with-normals-pipeline-mixin)
	      (let* ((material (if group (group-material group) (make-material "default")))
		     (ambient (material-ambient material))
		     (diffuse (material-diffuse material))
		     (specular (material-specular material))
		     (shininess (material-shininess material)))

		(setf (mem-aref pvalues2 :unsigned-int +lighting-fragment-shader-ambient-offset+) (canonicalize-color ambient)
		      (mem-aref pvalues2 :unsigned-int +lighting-fragment-shader-diffuse-offset+) (canonicalize-color diffuse)
		      (mem-aref pvalues2 :unsigned-int +lighting-fragment-shader-specular-offset+) (canonicalize-color specular)
		      (mem-ref pvalues2 :float (load-time-value
						(* +lighting-fragment-shader-shininess-offset+ (foreign-type-size :float))))
		      (clampf shininess))))
	    
	    ;; make sure msdf-texture fragment shader can get px-range from font
	    (vkCmdPushConstants command-buffer-handle
				(h pipeline-layout)
				VK_SHADER_STAGE_FRAGMENT_BIT
				(load-time-value (* +uber-vertex-shader-pc-size+
						    (foreign-type-size :uint32)))
				(load-time-value (* +fragment-shader-pc-size+
						    (foreign-type-size :uint32)))
				pvalues2))

	  ;;(print (foreign-array-fill-pointer index-array))

          ;; draw the whole draw list in one command
	  (vkCmdDrawIndexed command-buffer-handle
			    (foreign-array-fill-pointer index-array)
			    1 0 0 0))))))

(defmethod render-draw-list-cmds ((pipeline draw-indexed-pipeline-mixin) draw-data draw-list
				  dpy device command-buffer scene window view proj viewport near far
				  releaseme-queue)

  (ubershader-render-draw-list-cmds pipeline draw-data draw-list dpy device command-buffer scene window view proj
				    (viewport-x viewport) (viewport-y viewport)
				    (viewport-width viewport) (viewport-height viewport) near far releaseme-queue))

(defmethod render-draw-list ((pipeline draw-indexed-pipeline-mixin) draw-data draw-list dpy device command-buffer scene window view proj viewport near far releaseme-queue)
  (ubershader-render-draw-list pipeline draw-data draw-list dpy device command-buffer scene window view proj
			       (viewport-x viewport) (viewport-y viewport)
			       (viewport-width viewport) (viewport-height viewport) near far releaseme-queue))
