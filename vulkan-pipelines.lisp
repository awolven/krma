(in-package :krma)

(eval-when (:compile-toplevel :load-toplevel)
  (when *muffle-compilation-notes*
    #+sbcl(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))))

(eval-when (:compile-toplevel :load-toplevel)
  (when krma::*debug*
    (declaim (optimize (safety 3) (debug 3))))
  (unless krma::*debug*
    (declaim (optimize (speed 3) (safety 0) (debug 0)))))

(defgeneric pipeline-topology (pipeline))

(defgeneric vertex-shader-pathname (pipeline))

(defgeneric fragment-shader-pathname (pipeline))

;;(defgeneric create-device-objects (pipeline &key dpy))

(defconstant +uber-vertex-shader-model-matrix-offset+ 0)
(defconstant +uber-vertex-shader-point-size-offset+ 16)
(defconstant +uber-vertex-shader-primitive-type-offset+ 17)
(defconstant +uber-vertex-shader-color-override-offset+  18)
(defconstant +uber-vertex-shader-override-color-p-offset+ 19)

(defconstant +uber-vertex-shader-pc-size+ 20)

(defconstant +fragment-shader-select-box-min-offset+ 0)
(defconstant +fragment-shader-select-box-max-offset+ (+ +fragment-shader-select-box-min-offset+ 2))
(defconstant +text-fragment-shader-px-range-offset+ (+ +fragment-shader-select-box-max-offset+ 2))
(defconstant +lighting-fragment-shader-ambient-offset+ (1+ +text-fragment-shader-px-range-offset+))
(defconstant +lighting-fragment-shader-diffuse-offset+ (1+ +lighting-fragment-shader-ambient-offset+))
(defconstant +lighting-fragment-shader-specular-offset+ (1+ +lighting-fragment-shader-diffuse-offset+))
(defconstant +lighting-fragment-shader-shininess-offset+ (1+ +lighting-fragment-shader-specular-offset+))
							
(defconstant +fragment-shader-pc-size+ (1+ +lighting-fragment-shader-shininess-offset+))

(defclass pipeline-mixin ()
  ((display :reader pipeline-display :initarg :dpy)
   (name :initarg :name :reader pipeline-name :initform nil)
   (pipeline-layout :accessor pipeline-layout)
   (device-pipeline :accessor device-pipeline)
   
   (global-descriptor-set-layout :initform nil)
   (global-descriptor-set :initform nil)
   (scene-descriptor-set-layout :initform nil)
   (per-instance-descriptor-set-layout :initform nil)
   
   (vertex-uniform-buffer :initform nil :accessor pipeline-vertex-uniform-buffer)
   (fragment-uniform-buffer :initform nil :accessor pipeline-fragment-uniform-buffer)))

(defun update-vertex-uniform-buffer (pipeline view proj)
  (with-foreign-object (p-stage '(:struct vertex-uniform-buffer))
    (copy-matrix-to-foreign view (foreign-slot-pointer p-stage '(:struct vertex-uniform-buffer) 'view))
    (copy-matrix-to-foreign proj (foreign-slot-pointer p-stage '(:struct vertex-uniform-buffer) 'proj))
    (copy-matrix-to-foreign (m* proj view) (foreign-slot-pointer p-stage '(:struct vertex-uniform-buffer) 'vproj))
    (copy-uniform-buffer-memory (default-logical-device pipeline)
				p-stage
				(allocated-memory (pipeline-vertex-uniform-buffer pipeline))
				(load-time-value (foreign-type-size '(:struct vertex-uniform-buffer))))))



(defmethod make-global-descriptor-set-layout-bindings ((pipeline pipeline-mixin))
  nil)

(defmethod global-descriptor-set-layout ((pipeline pipeline-mixin))
  (if (slot-value pipeline 'global-descriptor-set-layout)
      (slot-value pipeline 'global-descriptor-set-layout)
      (setf (slot-value pipeline 'global-descriptor-set-layout)
	    (create-descriptor-set-layout (default-logical-device (pipeline-display pipeline)) :bindings (make-global-descriptor-set-layout-bindings pipeline)))))

(defmethod make-scene-descriptor-set-layout-bindings ((pipeline pipeline-mixin))
  nil)

(defmethod scene-descriptor-set-layout ((pipeline pipeline-mixin))
  (if (slot-value pipeline 'scene-descriptor-set-layout)
      (slot-value pipeline 'scene-descriptor-set-layout)
      (setf (slot-value pipeline 'scene-descriptor-set-layout)
	    (create-descriptor-set-layout (default-logical-device (pipeline-display pipeline)) :bindings (make-scene-descriptor-set-layout-bindings pipeline)))))

(defmethod make-per-instance-descriptor-set-layout-bindings ((pipeline pipeline-mixin))
  nil)

(defmethod per-instance-descriptor-set-layout ((pipeline pipeline-mixin))
  (if (slot-value pipeline 'per-instance-descriptor-set-layout)
      (slot-value pipeline 'per-instance-descriptor-set-layout)
      (setf (slot-value pipeline 'per-instance-descriptor-set-layout)
	    (create-descriptor-set-layout (default-logical-device (pipeline-display pipeline)) :bindings (make-per-instance-descriptor-set-layout-bindings pipeline)))))

(defmethod print-object ((object pipeline-mixin) stream)
  (print-unreadable-object (object stream)
    (if (pipeline-name object)
        (princ (pipeline-name object) stream)
        (princ (class-name (class-of object)) stream))))

(defmethod allocator ((pipeline pipeline-mixin))
  (with-slots (display) pipeline
    (allocator display)))

(defmethod default-logical-device ((pipeline pipeline-mixin))
  (with-slots (display) pipeline
    (default-logical-device display)))

(defmethod pipeline-cache ((pipeline pipeline-mixin))
  (with-slots (display) pipeline
    (pipeline-cache display)))

(defmethod descriptor-pool ((pipeline pipeline-mixin))
  (with-slots (display) pipeline
    (default-descriptor-pool display)))

(defmethod pipeline-front-face-orientation ((pipeline pipeline-mixin))
  VK_FRONT_FACE_COUNTER_CLOCKWISE)

(defmethod pipeline-cull-mode ((pipeline pipeline-mixin))
  VK_CULL_MODE_BACK_BIT)

(defmethod vk::make-descriptor-image-info ((pipeline pipeline-mixin))
  nil)

(defmethod pipeline-line-width ((pipeline pipeline-mixin))
  #+(or windows linux) 2.0f0
  #+darwin 1.0f0) ;; because of MoltenVK limitations

(defmethod pipeline-point-size ((pipeline pipeline-mixin))
  5.0f0)

(defmethod pipeline-default-font ((pipeline pipeline-mixin))
  nil)

(defmethod create-device-objects ((pipeline pipeline-mixin) device render-pass)
  (create-standard-pipeline-device-objects pipeline device render-pass))

(defmethod initialize-instance :after ((pipeline pipeline-mixin) &rest initargs
				&key dpy)
  (declare (ignore initargs))
  (let* ((window (clui::helper-window dpy))
	 (surface (render-surface window))
	 (device (default-logical-device dpy)))

    (unless (vk::paired-gpu surface)
      ;; helper window surface has not been initialized yet
      ;; because we didn't have logical device when it was created.
      ;; so initialize it so that we can get the surface-format to
      ;; create the render pass properly
      (let* ((gpu (physical-device device))
	     (index (get-queue-family-index-with-wsi-support gpu surface)))
	(initialize-window-surface surface gpu index)))
    
    (unless (display-stock-render-pass dpy)
      (setf (display-stock-render-pass dpy)
	    (create-render-pass device (vk::surface-format-format (find-supported-format surface)))))
    
    (create-device-objects pipeline device (display-stock-render-pass dpy))
    (values)))

(defmethod make-push-constant-ranges ((pipeline pipeline-mixin))
  nil)

(defmethod pipeline-depth-test-enable? ((pipeline pipeline-mixin))
  t)

(defmethod pipeline-depth-write-enable? ((pipeline pipeline-mixin))
  t)

(defmethod pipeline-depth-compare-op ((pipeline pipeline-mixin))
  VK_COMPARE_OP_LESS)

(defmethod pipeline-logic-op ((pipeline pipeline-mixin))
  VK_LOGIC_OP_COPY)

(defmethod pipeline-blend-enable? ((pipeline pipeline-mixin))
  t)

(defmethod pipeline-depth-clamp-enable? ((pipeline pipeline-mixin))
  nil)

(defmethod pipeline-src-alpha-blend-factor ((pipeline pipeline-mixin))
  VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA)

(defmethod pipeline-min-sample-shading ((pipeline pipeline-mixin))
  1.0f0)


(defclass draw-indexed-pipeline-mixin (pipeline-mixin)
  ())

(defclass ubershader-pipeline-mixin (draw-indexed-pipeline-mixin)

  ())

(defmethod initialize-instance :after ((instance ubershader-pipeline-mixin) &rest initargs)
  (declare (ignore initargs))
  (setf (pipeline-vertex-uniform-buffer instance)
	(create-uniform-buffer (default-logical-device instance)
			       (load-time-value (foreign-type-size '(:struct vertex-uniform-buffer)))))
  (setf (pipeline-fragment-uniform-buffer instance)
	(create-uniform-buffer (default-logical-device instance)
			       (load-time-value (foreign-type-size '(:struct fragment-uniform-buffer))))))

(defmethod global-descriptor-set ((pipeline ubershader-pipeline-mixin))
  ;; apparently each pipeline needs it's own uniform buffer
  ;; you can't have a [vertex] uniform buffer for the entire application
  ;; because it doesn't update when switching from 2d to 3d
  ;; so we have to have a separate global-descriptor-set for each pipeline too
  ;; but maybe the fragment-uniform-buffer could be a slot on the app
  ;;   
  (if (slot-value pipeline 'global-descriptor-set)
      (slot-value pipeline 'global-descriptor-set)
      (setf (slot-value pipeline 'global-descriptor-set)
	    (create-descriptor-set
	     (default-logical-device pipeline)
	     (list (global-descriptor-set-layout pipeline))
	     (descriptor-pool pipeline)
	     :descriptor-buffer-info (list (make-instance 'descriptor-uniform-buffer-info
							  :buffer (pipeline-vertex-uniform-buffer pipeline)
							  :offset 0
							  :range (load-time-value (foreign-type-size '(:struct vertex-uniform-buffer))))
					   (make-instance 'descriptor-uniform-buffer-info
							  :buffer (pipeline-fragment-uniform-buffer pipeline)
							  :offset 0
							  :range (load-time-value (foreign-type-size '(:struct fragment-uniform-buffer)))))))))

(defmethod make-global-descriptor-set-layout-bindings ((pipeline ubershader-pipeline-mixin))
 ;; set 0
  (list (make-instance 'uniform-buffer-for-vertex-shader-dsl-binding)
	(make-instance 'vk::uniform-buffer-for-fragment-shader-dsl-binding
		       :binding 1)))

(defmethod scene-descriptor-set-layout ((pipeline ubershader-pipeline-mixin))
  (krma-select-boxes-descriptor-set-layout (pipeline-display pipeline)))

(defmethod make-scene-descriptor-set-layout-bindings ((pipeline ubershader-pipeline-mixin))
  (make-select-boxes-descriptor-set-layout-bindings (pipeline-display pipeline)))

(defmethod make-per-instance-descriptor-set-layout-bindings ((pipeline ubershader-pipeline-mixin))
  (make-per-instance-descriptor-set-layout-bindings (pipeline-display pipeline)))

(defmethod per-instance-descriptor-set-layout ((pipeline ubershader-pipeline-mixin))
  (krma-ubershader-per-instance-descriptor-set-layout (pipeline-display pipeline)))

(defmethod make-push-constant-ranges ((pipeline draw-indexed-pipeline-mixin))
  (list (make-instance 'push-constant-range
                       :stage-flags VK_SHADER_STAGE_VERTEX_BIT
                       :offset 0
                       :size (load-time-value (* +uber-vertex-shader-pc-size+
                                                 (foreign-type-size :uint32))))))

(defmethod make-push-constant-ranges ((pipeline ubershader-pipeline-mixin))
  (append (call-next-method)
	  (list (make-instance 'push-constant-range
                               :stage-flags VK_SHADER_STAGE_FRAGMENT_BIT
                               :offset (load-time-value (* +uber-vertex-shader-pc-size+
                                                           (foreign-type-size :uint32)))
                               :size (load-time-value (* +fragment-shader-pc-size+
							 (foreign-type-size :float)))))))

(defclass texture-pipeline-mixin (ubershader-pipeline-mixin)
  ())

(defmethod fragment-shader-pathname ((pipeline texture-pipeline-mixin))
  (asdf/system:system-relative-pathname :krma "submodules/krma-shader-bin/texture.frag.spv"))

(defmethod pipeline-min-sample-shading ((pipeline texture-pipeline-mixin))
  0.0f0)

(defmethod make-per-instance-descriptor-set-layout-bindings ((pipeline texture-pipeline-mixin))
  (list (make-instance 'descriptor-set-layout-binding
                       :type VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
                       :count 1
                       :flags VK_SHADER_STAGE_FRAGMENT_BIT
                       :samplers (list (krma-texture-sampler (pipeline-display pipeline))))))


(defclass 2d-pipeline-mixin ()
  ())

(defmethod vertex-shader-pathname ((pipeline 2d-pipeline-mixin))
  (asdf/system:system-relative-pathname :krma "submodules/krma-shader-bin/standard-2d.vert.spv"))

(defclass 2d-texture-pipeline-mixin (2d-pipeline-mixin texture-pipeline-mixin)
  ())

(defmethod pipeline-vertex-type ((pipeline 2d-texture-pipeline-mixin))
  '(:struct textured-3d-vertex))

(defmethod pipeline-depth-test-enable? ((pipeline 2d-pipeline-mixin))
  t)

(defmethod pipeline-depth-write-enable? ((pipeline 2d-pipeline-mixin))
  t)

(defmethod pipeline-depth-compare-op ((pipeline 2d-pipeline-mixin))
  VK_COMPARE_OP_LESS_OR_EQUAL)

(defmethod pipeline-logic-op ((pipeline 2d-pipeline-mixin))
  VK_LOGIC_OP_COPY)

(defmethod pipeline-cull-mode ((pipeline 2d-pipeline-mixin))
  VK_CULL_MODE_NONE)


(defclass 3d-pipeline-mixin ()
  ())

(defmethod vertex-shader-pathname ((pipeline 3d-pipeline-mixin))
  (asdf/system:system-relative-pathname :krma "submodules/krma-shader-bin/standard-3d.vert.spv"))

(defclass 3d-texture-pipeline-mixin (3d-pipeline-mixin texture-pipeline-mixin)
  ())

(defmethod pipeline-vertex-type ((pipeline 3d-texture-pipeline-mixin))
  '(:struct textured-3d-vertex))

(defmethod pipeline-cull-mode ((pipeline 3d-texture-pipeline-mixin))
  VK_CULL_MODE_BACK_BIT)


(defclass 3d-texture-with-normals-pipeline-mixin (3d-texture-pipeline-mixin)
  ())

(defmethod pipeline-vertex-type ((pipeline 3d-texture-with-normals-pipeline-mixin))
  '(:struct textured-3d-vertex-with-normal))

(defmethod fragment-shader-pathname ((pipeline 3d-texture-with-normals-pipeline-mixin))
  (asdf/system:system-relative-pathname :krma "submodules/krma-shader-bin/lighting+texture.frag.spv"))

(defmethod vertex-shader-pathname ((pipeline 3d-texture-with-normals-pipeline-mixin))
  (asdf/system:system-relative-pathname :krma "submodules/krma-shader-bin/3d-with-normal.vert.spv"))

(defclass texture-image (vk::image)
  ((descriptor-set :accessor texture-image-descriptor-set)))

(defun aligned-size (size)
  (* (1+ (ceiling (/ (1- size) +buffer-alignment+))) +buffer-alignment+))

(defun make-vulkan-texture (device queue sampler descriptor-set-layout descriptor-pool command-buffer bpp bitmap width height
                            &key (allocator (allocator device)))

  (declare (type fixnum width height bpp))
  (let* ((descriptor-set (allocate-descriptor-set device (list descriptor-set-layout) descriptor-pool))
         (texture-image (create-image device width height :image-class 'texture-image :allocator allocator))
         (texture-image-view (create-image-view device texture-image :allocator allocator))
         (upload-size (* width height bpp #.(foreign-type-size :unsigned-char)))
         (upload-size-aligned (aligned-size upload-size)))

    (setf (texture-image-descriptor-set texture-image) descriptor-set)

    ;; update the descriptor set:
    (with-vk-struct (p-desc-image VkDescriptorImageInfo)
      (with-foreign-slots ((%vk::sampler
                            %vk::imageView
                            %vk::imageLayout)
                           p-desc-image (:struct VkDescriptorImageInfo))
        (setf %vk::sampler (h sampler)
              %vk::imageView (h texture-image-view)
              %vk::imageLayout VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)
        (with-vk-struct (p-write-desc VkWriteDescriptorSet)
          (with-foreign-slots ((%vk::dstSet
                                %vk::descriptorCount
                                %vk::descriptorType
                                %vk::pImageInfo)
                               p-write-desc (:struct VkWriteDescriptorSet))
            (setf %vk::dstSet (h descriptor-set)
                  %vk::descriptorCount 1
                  %vk::descriptorType VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
                  %vk::pImageInfo p-desc-image))
          (vkUpdateDescriptorSets (h device) 1 p-write-desc 0 +nullptr+)

          ;; create the upload-buffer
          (let* ((upload-buffer (create-buffer-1 device upload-size-aligned
                                                 VK_BUFFER_USAGE_TRANSFER_SRC_BIT
                                                 :allocator allocator))
                 (upload-buffer-memory (allocate-buffer-memory device upload-buffer
                                                               VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
                                                               :allocator allocator)))

            (vkBindBufferMemory (h device) (h upload-buffer) (h upload-buffer-memory) 0)

            ;; upload to buffer
            (with-foreign-object (p-map :pointer)

              (check-vk-result
               (vkMapMemory (h device) (h upload-buffer-memory) 0 upload-size-aligned 0 p-map))

              (if (typep bitmap 'vector)
		  #+CCL
		  (ccl::%copy-ivector-to-ptr bitmap 0 (mem-aref p-map :pointer) 0 upload-size)
		  #+sbcl
                  (sb-sys:with-pinned-objects (bitmap)
                    (vk::memcpy (mem-aref p-map :pointer) (sb-sys:vector-sap bitmap) upload-size))
                  (vk::memcpy (mem-aref p-map :pointer) bitmap upload-size))

              (with-vk-struct (p-range VkMappedMemoryRange)
                (with-foreign-slots ((%vk::memory
                                      %vk::size)
                                     p-range (:struct VkMappedMemoryRange))
                  (setf %vk::memory (h upload-buffer-memory)
                        %vk::size upload-size-aligned))
                (check-vk-result
                 (vkFlushMappedMemoryRanges (h device) 1 p-range))

                (vkUnmapMemory (h device) (h upload-buffer-memory))))

            ;; copy to image
            (with-vk-struct (p-copy-barrier VkImageMemoryBarrier)
              (with-foreign-slots ((%vk::dstAccessMask
                                    %vk::oldLayout
                                    %vk::newLayout
                                    %vk::srcQueueFamilyIndex
                                    %vk::dstQueueFamilyIndex
                                    %vk::image)
                                   p-copy-barrier (:struct VkImageMemoryBarrier))
                (setf %vk::dstAccessMask VK_ACCESS_TRANSFER_WRITE_BIT
                      %vk::oldLayout VK_IMAGE_LAYOUT_UNDEFINED
                      %vk::newLayout VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                      %vk::srcQueueFamilyIndex vk::VK_QUEUE_FAMILY_IGNORED
                      %vk::dstQueueFamilyIndex vk::VK_QUEUE_FAMILY_IGNORED
                      %vk::image (h texture-image))

                (let ((p-subresource-range
		       (foreign-slot-pointer p-copy-barrier '(:struct VkImageMemoryBarrier) '%vk::subresourceRange)))
                  (with-foreign-slots ((%vk::aspectMask
                                        %vk::levelCount
                                        %vk::layerCount)
                                       p-subresource-range
                                       (:struct VkImageSubresourceRange))
                    (setf %vk::aspectMask VK_IMAGE_ASPECT_COLOR_BIT
                          %vk::levelCount 1
                          %vk::layerCount 1)))

                (begin-command-buffer command-buffer)

                (vkCmdPipelineBarrier (h command-buffer) VK_PIPELINE_STAGE_HOST_BIT
                                      VK_PIPELINE_STAGE_TRANSFER_BIT
                                      0 0 +nullptr+ 0 +nullptr+ 1 p-copy-barrier)

                (with-vk-struct (p-region VkBufferImageCopy)
                  (let ((p-image-subresource
			 (foreign-slot-pointer p-region '(:struct VkBufferImageCopy) '%vk::imageSubresource))
                        (p-image-extent
			 (foreign-slot-pointer p-region '(:struct VkBufferImageCopy) '%vk::imageExtent)))
                    (with-foreign-slots ((%vk::aspectMask
                                          %vk::layerCount)
                                         p-image-subresource (:struct VkImageSubresourceLayers))
                      (setf %vk::aspectMask VK_IMAGE_ASPECT_COLOR_BIT
                            %vk::layerCount 1))
                    (with-foreign-slots ((%vk::width
                                          %vk::height
                                          %vk::depth)
                                         p-image-extent (:struct VkExtent3D))
                      (setf %vk::width width
                            %vk::height height
                            %vk::depth 1)))
                  (vkCmdCopyBufferToImage (h command-buffer) (h upload-buffer)
                                          (h texture-image)
                                          VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL 1 p-region))

                (with-vk-struct (p-use-barrier VkImageMemoryBarrier)
		  (with-foreign-slots ((%vk::srcAccessMask
					%vk::dstAccessMask
					%vk::oldLayout
					%vk::newLayout
					%vk::srcQueueFamilyIndex
					%vk::dstQueueFamilyIndex
					%vk::image)
				       p-use-barrier (:struct VkImageMemoryBarrier))
		    (setf %vk::srcAccessMask VK_ACCESS_TRANSFER_WRITE_BIT
			  %vk::dstAccessMask VK_ACCESS_SHADER_READ_BIT
			  %vk::oldLayout VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
			  %vk::newLayout VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
			  %vk::srcQueueFamilyIndex vk::VK_QUEUE_FAMILY_IGNORED
			  %vk::dstQueueFamilyIndex vk::VK_QUEUE_FAMILY_IGNORED
			  %vk::image (h texture-image)))
		  (let ((p-subresource-range
			 (foreign-slot-pointer p-use-barrier '(:struct VkImageMemoryBarrier) '%vk::subresourceRange)))
		    (with-foreign-slots ((%vk::aspectMask
					  %vk::levelCount
					  %vk::layerCount)
					 p-subresource-range
					 (:struct VkImageSubresourceRange))
		      (setf %vk::aspectMask VK_IMAGE_ASPECT_COLOR_BIT
			    %vk::levelCount 1
			    %vk::layerCount 1)))

		  (vkCmdPipelineBarrier (h command-buffer) VK_PIPELINE_STAGE_TRANSFER_BIT
					VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT
					0 0 +nullptr+ 0 +nullptr+ 1 p-use-barrier))

                (end-command-buffer command-buffer)


                (queue-submit1 queue command-buffer)

                (device-wait-idle device)

                (vkDestroyBuffer (h device) (h upload-buffer) (h allocator))
                (vkFreeMemory (h device) (h upload-buffer-memory) (h allocator))

                texture-image))))))))

(defun initialize-buffers (dpy draw-list)
  (let ((vertex-array (draw-list-vertex-array draw-list))
        (index-array (draw-list-index-array draw-list)))

    (let ((index-size (* (foreign-array-fill-pointer index-array)
                         (foreign-array-foreign-type-size index-array)))
          (vertex-size (* (foreign-array-fill-pointer vertex-array)
                          (foreign-array-foreign-type-size vertex-array))))

      (flet ((mmap-buffer (buffer lisp-array size memory-resource aligned-size)
               (let ((memory (allocated-memory buffer))
                     (offset (vk::memory-resource-offset memory-resource))
                     (device (vk::device buffer)))
                 (with-foreign-object (pp-dst :pointer)

                   (check-vk-result (vkMapMemory (h device) (h memory) offset aligned-size 0 pp-dst))

                   (let ((p-dst (mem-aref pp-dst :pointer)))
		     #+sbcl
		     (sb-sys:with-pinned-objects (lisp-array)
		       (vk::memcpy p-dst (sb-sys:vector-sap lisp-array) size))
		     #+ccl
		     (ccl::%copy-ivector-to-ptr lisp-array 0 p-dst 0 size)

	             (with-foreign-object (p-range '(:struct VkMappedMemoryRange))
	               (zero-struct p-range '(:struct VkMappedMemoryRange))

	               (with-foreign-slots ((%vk::sType
				             %vk::memory
				             %vk::size
                                             %vk::offset)
                                            p-range (:struct VkMappedMemoryRange))

	                 (setf %vk::sType VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
		               %vk::memory (h memory)
		               %vk::size aligned-size
                               %vk::offset offset))

	               (check-vk-result (vkFlushMappedMemoryRanges (h device) 1 p-range))

	               (vkUnmapMemory (h device) (h memory))

	               (values)))))))

        (unless (zerop vertex-size)
          (let* ((old-size-aligned (draw-list-vertex-size-aligned draw-list))
                 (new-size-aligned (* (1+ (ceiling (/ (1- vertex-size) +buffer-alignment+))) +buffer-alignment+))
                 (memory-resource))
            (setq memory-resource
		  (if (> new-size-aligned old-size-aligned)
		      (prog1
			  (if (draw-list-vertex-memory draw-list)
			      (progn (vk::release-vertex-memory dpy (draw-list-vertex-memory draw-list))
				     (setf (draw-list-vertex-memory draw-list)
					   (vk::acquire-vertex-memory-sized dpy new-size-aligned :host-visible)))
			      (setf (draw-list-vertex-memory draw-list)
				    (vk::acquire-vertex-memory-sized dpy new-size-aligned :host-visible)))
			(setf (draw-list-vertex-size-aligned draw-list) new-size-aligned))
		      (if (draw-list-vertex-memory draw-list)
			  (draw-list-vertex-memory draw-list)
			  (prog1
			      (setf (draw-list-vertex-memory draw-list)
				    (vk::acquire-vertex-memory-sized dpy new-size-aligned :host-visible))
			    (setf (draw-list-vertex-size-aligned draw-list) new-size-aligned)))))
            (mmap-buffer (vk::memory-pool-buffer (vk::memory-resource-memory-pool memory-resource))
                         (foreign-array-bytes vertex-array) vertex-size memory-resource
                         new-size-aligned)))

        (unless (zerop index-size)
          (let* ((new-size-aligned (* (1+ (ceiling (/ (1- index-size) +buffer-alignment+))) +buffer-alignment+))
                 (old-size-aligned (draw-list-index-size-aligned draw-list))
                 (memory-resource))
            (setq memory-resource
		  (if (> new-size-aligned old-size-aligned)
		      (prog1 
			  (if (draw-list-index-memory draw-list)
			      (progn (vk::release-index-memory dpy (draw-list-index-memory draw-list))
				     (setf (draw-list-index-memory draw-list)
					   (vk::acquire-index-memory-sized dpy new-size-aligned :host-visible)))
			      (setf (draw-list-index-memory draw-list)
				    (vk::acquire-index-memory-sized dpy new-size-aligned :host-visible)))
			(setf (draw-list-index-size-aligned draw-list) new-size-aligned))
		      (if (draw-list-index-memory draw-list)
			  (draw-list-index-memory draw-list)
			  (prog1
			      (setf (draw-list-index-memory draw-list)
				    (vk::acquire-index-memory-sized dpy new-size-aligned :host-visible))
			    (setf (draw-list-index-size-aligned draw-list) new-size-aligned)))))
	    (mmap-buffer (vk::memory-pool-buffer (vk::memory-resource-memory-pool memory-resource))
                         (foreign-array-bytes index-array) index-size memory-resource
                         new-size-aligned))))))
  
  (values))

(defun create-standard-pipeline-device-objects (pipeline
						device
						render-pass
						&key
						  (push-constant-ranges (make-push-constant-ranges pipeline))
						  (line-width (pipeline-line-width pipeline))
						  (vertex-type (pipeline-vertex-type pipeline))
						  (vertex-input-attribute-descriptions
						   (make-vertex-input-attribute-descriptions pipeline))
						  (topology (pipeline-topology pipeline))
						  (min-sample-shading (pipeline-min-sample-shading pipeline))
						  (depth-test-enable (pipeline-depth-test-enable? pipeline))
						  (depth-write-enable (pipeline-depth-write-enable? pipeline))
						  (depth-compare-op (pipeline-depth-compare-op pipeline))
						  (logic-op (pipeline-logic-op pipeline))
						  (blend-enable (pipeline-blend-enable? pipeline))
						  (depth-clamp-enable (pipeline-depth-clamp-enable? pipeline))
						  (src-alpha-blend-factor (pipeline-src-alpha-blend-factor pipeline))
						  (stippled-line-enable nil)
						  (additional-pipeline-creation-args nil)
						  (cull-mode (pipeline-cull-mode pipeline))
						  (front-face (pipeline-front-face-orientation pipeline)))
  (let ()
    (let ((vtx-shader (create-shader-module-from-file device (vertex-shader-pathname pipeline)))
	  (frg-shader (create-shader-module-from-file device (fragment-shader-pathname pipeline))))

      (setf (pipeline-layout pipeline)
	    (create-pipeline-layout device (list (global-descriptor-set-layout pipeline)
                                                 (scene-descriptor-set-layout pipeline)
                                                 (per-instance-descriptor-set-layout pipeline))
				    :push-constant-ranges push-constant-ranges)
	    (device-pipeline pipeline)
	    (apply #'create-graphics-pipeline device (pipeline-cache pipeline) (pipeline-layout pipeline)
		   render-pass nil vtx-shader frg-shader
		   :cull-mode cull-mode
		   :front-face front-face
		   :line-width line-width
		   :vertex-type vertex-type
		   :vertex-input-attribute-descriptions vertex-input-attribute-descriptions
		   :topology topology
		   :min-sample-shading min-sample-shading
		   :depth-test-enable (if depth-test-enable (if (eq depth-test-enable VK_FALSE)
								VK_FALSE VK_TRUE)
					  VK_FALSE)
		   :depth-write-enable (if depth-write-enable (if (eq depth-write-enable VK_FALSE)
								  VK_FALSE VK_TRUE)
					   VK_FALSE)
		   :depth-compare-op depth-compare-op
		   :logic-op logic-op
		   :blend-enable (if blend-enable (if (eq blend-enable VK_FALSE) VK_FALSE VK_TRUE) VK_FALSE)
		   :depth-clamp-enable (if depth-clamp-enable (if (eq depth-clamp-enable VK_FALSE)
								  VK_FALSE VK_TRUE)
					   VK_FALSE)
		   :src-alpha-blend-factor src-alpha-blend-factor
		   :stippled-line-enable (if stippled-line-enable
					     VK_TRUE VK_FALSE)
		   :allocator (allocator pipeline)
		   additional-pipeline-creation-args))

      (destroy-shader-module vtx-shader)
      (destroy-shader-module frg-shader)

      pipeline)))

(defmethod make-vertex-input-attribute-descriptions ((pipeline 2d-texture-pipeline-mixin))
  (list (make-instance 'vertex-input-attribute-description
		       :location 0
		       :format VK_FORMAT_R32_UINT
		       :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'oid))
	(make-instance 'vertex-input-attribute-description
		       :location 1
		       :format VK_FORMAT_R32G32B32_SFLOAT ;; <--updated for layer feature
		       :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'x))
	(make-instance 'vertex-input-attribute-description
		       :location 2
		       :format VK_FORMAT_R32G32_SFLOAT
		       :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'u))
        (make-instance 'vertex-input-attribute-description
                       :location 3
                       :format VK_FORMAT_R32_UINT
                       :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'col))))

(defmethod make-vertex-input-attribute-descriptions ((pipeline 3d-texture-pipeline-mixin))
  (list (make-instance 'vertex-input-attribute-description
		       :location 0
		       :format VK_FORMAT_R32_UINT
		       :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'oid))
	(make-instance 'vertex-input-attribute-description
                       :location 1
                       :format VK_FORMAT_R32G32B32_SFLOAT
		       :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'x))
	(make-instance 'vertex-input-attribute-description
		       :location 2
		       :format VK_FORMAT_R32G32_SFLOAT
		       :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'u))
        (make-instance 'vertex-input-attribute-description
                       :location 3
                       :format VK_FORMAT_R32_UINT
                       :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'col))))

(defmethod make-vertex-input-attribute-descriptions ((pipeline 3d-texture-with-normals-pipeline-mixin))
  (list (make-instance 'vertex-input-attribute-description
		       :location 0
		       :format VK_FORMAT_R32_UINT
		       :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'oid))
	(make-instance 'vertex-input-attribute-description
                       :location 1
                       :format VK_FORMAT_R32G32B32_SFLOAT
		       :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'x))
	(make-instance 'vertex-input-attribute-description
		       :location 2
		       :format VK_FORMAT_R32G32_SFLOAT
		       :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'u))
        (make-instance 'vertex-input-attribute-description
                       :location 3
                       :format VK_FORMAT_R32_UINT
                       :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'col))
        (make-instance 'vertex-input-attribute-description
                       :location 4
                       :format VK_FORMAT_R32G32B32_SFLOAT
		       :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'nx))))


(defclass point-list-pipeline-mixin () ())

(defmethod pipeline-topology ((pipeline point-list-pipeline-mixin))
  VK_PRIMITIVE_TOPOLOGY_POINT_LIST)

(defclass 2d-point-list-pipeline (point-list-pipeline-mixin
                                  2d-texture-pipeline-mixin)
  ())

(defclass 3d-point-list-pipeline (point-list-pipeline-mixin
                                  3d-texture-pipeline-mixin)
  ())

(defclass line-pipeline-mixin () ())

(defclass 2d-line-pipeline-mixin (line-pipeline-mixin 2d-texture-pipeline-mixin) ())


(defclass 3d-line-pipeline-mixin (line-pipeline-mixin 3d-texture-pipeline-mixin) ())


(defclass line-list-pipeline-mixin () ())

(defmethod pipeline-topology ((pipeline line-list-pipeline-mixin))
  VK_PRIMITIVE_TOPOLOGY_LINE_LIST)

(defclass line-strip-pipeline-mixin () ())

(defmethod pipeline-topology ((pipeline line-strip-pipeline-mixin))
  VK_PRIMITIVE_TOPOLOGY_LINE_STRIP)

(defclass 2d-line-list-pipeline (line-list-pipeline-mixin
                                 2d-line-pipeline-mixin)
  ())

(defclass 2d-line-strip-pipeline (line-strip-pipeline-mixin
                                  2d-line-pipeline-mixin)
  ())

(defclass 3d-line-list-pipeline (line-list-pipeline-mixin
                                 3d-line-pipeline-mixin)
  ())

(defclass 3d-line-strip-pipeline (line-strip-pipeline-mixin
                                  3d-line-pipeline-mixin)
  ())

(defclass triangle-list-pipeline-mixin () ())

(defmethod pipeline-topology ((pipeline triangle-list-pipeline-mixin))
  VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST)

(defclass triangle-strip-pipeline-mixin () ())

(defmethod pipeline-topology ((pipeline triangle-strip-pipeline-mixin))
  VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP)

(defclass 2d-triangle-list-pipeline-mixin (triangle-list-pipeline-mixin
                                           2d-texture-pipeline-mixin)
  ())

(defclass 2d-triangle-list-pipeline (2d-triangle-list-pipeline-mixin)
  ())

(defclass msdf-text-pipeline (2d-triangle-list-pipeline-mixin)
  ())

(defmethod pipeline-default-font ((pipeline msdf-text-pipeline))
  (display-system-font (pipeline-display pipeline)))

(defmethod fragment-shader-pathname ((pipeline msdf-text-pipeline)) 
  (asdf/system:system-relative-pathname :krma "submodules/krma-shader-bin/msdf-texture.frag.spv"))

(defclass 2d-triangle-strip-pipeline (triangle-strip-pipeline-mixin
                                      2d-texture-pipeline-mixin)
  ())

(defclass 3d-triangle-list-pipeline (triangle-list-pipeline-mixin
                                     3d-texture-pipeline-mixin)
  ())

(defclass 3d-triangle-list-with-normals-pipeline (triangle-list-pipeline-mixin
						  3d-texture-with-normals-pipeline-mixin)
  ())

(defclass 3d-triangle-strip-pipeline (triangle-strip-pipeline-mixin
                                      3d-texture-pipeline-mixin)
  ())

(defclass 3d-triangle-strip-with-normals-pipeline (triangle-strip-pipeline-mixin
						   3d-texture-with-normals-pipeline-mixin)
  ())


;;------

(defun ubershader-render-draw-list-cmds (pipeline draw-data draw-list dpy device command-buffer scene view proj x y width height)
  
  (declare (type draw-indexed-pipeline-mixin pipeline))
  (declare (type draw-list-mixin draw-list))
  (declare (type standard-draw-data draw-data))

  (let ((index-array (draw-list-index-array draw-list)))
    (declare (type foreign-adjustable-array index-array))
    
    (unless (= 0 (foreign-array-fill-pointer index-array))

      (initialize-buffers dpy draw-list)
      
      (let ((cmd-vector (draw-list-cmd-vector draw-list)))
	(declare (type (vector t) cmd-vector))
	
	(unless (= 0 (fill-pointer cmd-vector))
	  
	  (let* ((pipeline-layout (pipeline-layout pipeline))
                 (command-buffer-handle (h command-buffer))
		 (mm))
	    
            (cmd-bind-pipeline command-buffer (device-pipeline pipeline) :bind-point :graphics)

	    ;; apparently, updating uniform buffers have no effect if done before cmd-bind-pipeline
	    (update-vertex-uniform-buffer pipeline view proj)

	    (update-fragment-uniform-buffer pipeline scene)

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
	      (setf (mem-aref p-descriptor-sets :pointer 0) (h (aref (krma-select-boxes-descriptor-sets dpy) (car (current-frame-cons dpy)))))
              (vkCmdBindDescriptorSets (h command-buffer)
                                       VK_PIPELINE_BIND_POINT_GRAPHICS
                                       (h pipeline-layout)
                                       1 1
                                       p-descriptor-sets
                                       0 +nullptr+))

            (cmd-bind-vertex-buffers command-buffer (list (vk::memory-pool-buffer (vk::memory-resource-memory-pool (draw-list-vertex-memory draw-list))))
                                     (list (vk::memory-resource-offset (draw-list-vertex-memory draw-list))))
            (cmd-bind-index-buffer command-buffer (vk::memory-pool-buffer (vk::memory-resource-memory-pool (draw-list-index-memory draw-list)))
                                   (vk::memory-resource-offset (draw-list-index-memory draw-list)) (foreign-array-foreign-type index-array))

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
			       (if group-model-matrix
				   (setq mm (m* cmd-model-matrix group-model-matrix))
				   (setq mm (m* cmd-model-matrix)))
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

			   (cond ((typep pipeline 'point-list-pipeline-mixin)
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

			   (vkCmdPushConstants command-buffer-handle
					       (h pipeline-layout)
					       VK_SHADER_STAGE_VERTEX_BIT
					       0
					       (load-time-value (* +uber-vertex-shader-pc-size+
								   (foreign-type-size :uint32)))
					       pvalues)))


		       (with-foreign-object (pvalues2 :float +fragment-shader-pc-size+)
			 (when (typep cmd 'text-draw-indexed-cmd)
			   (let ((font (or (cmd-font cmd) pipeline-default-font)))
			     (when font
			       (let ((px-range (font-px-range font)))
				 (if px-range
				     (setf (mem-aref pvalues2 :float +text-fragment-shader-px-range-offset+) (clampf px-range))
				     (setf (mem-aref pvalues2 :float +text-fragment-shader-px-range-offset+) 32.0f0))))))
			 
			 (let ((select-box-coords (krma-select-box-coords dpy)))
			   (setf (mem-aref pvalues2 :float +fragment-shader-select-box-min-offset+) (clampf (vx select-box-coords))
				 (mem-aref pvalues2 :float (1+ +fragment-shader-select-box-min-offset+)) (clampf (vy select-box-coords))
				 (mem-aref pvalues2 :float +fragment-shader-select-box-max-offset+) (clampf (vz select-box-coords))
				 (mem-aref pvalues2 :float (1+ +fragment-shader-select-box-max-offset+)) (clampf (vw select-box-coords))))

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
					 (cmd-elem-count cmd) 1 (cmd-first-idx cmd) (cmd-vtx-offset cmd)
					 0))))

	      (loop for cmd across cmd-vector
		    when cmd
		      do (render-standard-draw-indexed-cmd cmd))
              t)))))))

(defmethod render-draw-list-cmds ((pipeline draw-indexed-pipeline-mixin) draw-data draw-list
				  dpy device command-buffer scene view proj viewport)

  (ubershader-render-draw-list-cmds pipeline draw-data draw-list dpy device command-buffer scene view proj
				    (viewport-x viewport) (viewport-y viewport)
				    (viewport-width viewport) (viewport-height viewport)))

(defun ubershader-render-draw-list (pipeline draw-data draw-list dpy device command-buffer scene view proj x y width height)
  
  (declare (type draw-indexed-pipeline-mixin))
  (declare (type draw-list-mixin draw-list))
  (declare (ignore draw-data))

  (let ((index-array (draw-list-index-array draw-list)))
    (declare (type foreign-adjustable-array index-array))

    (unless (= 0 (foreign-array-fill-pointer index-array))

      (initialize-buffers dpy draw-list)
      
      (let* ((command-buffer-handle (h command-buffer))
             (pipeline-layout (pipeline-layout pipeline))
	     (group (draw-list-group draw-list))
             (mm))

	(declare (type (or group null) group)) ;; group is null for im-draw-list

	(cmd-bind-pipeline command-buffer (device-pipeline pipeline) :bind-point :graphics)

	(update-vertex-uniform-buffer pipeline view proj)

	(update-fragment-uniform-buffer pipeline scene)

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
          (setf (mem-aref p-descriptor-sets :pointer 0) (h (aref (krma-select-boxes-descriptor-sets dpy) (car (current-frame-cons dpy)))))
          (vkCmdBindDescriptorSets (h command-buffer)
                                   VK_PIPELINE_BIND_POINT_GRAPHICS
                                   (h pipeline-layout)
                                   1 1
                                   p-descriptor-sets
                                   0 +nullptr+))
	
        (cmd-bind-vertex-buffers command-buffer (list (vk::memory-pool-buffer (vk::memory-resource-memory-pool (draw-list-vertex-memory draw-list))))
                                 (list (vk::memory-resource-offset (draw-list-vertex-memory draw-list))))
        (cmd-bind-index-buffer command-buffer (vk::memory-pool-buffer (vk::memory-resource-memory-pool (draw-list-index-memory draw-list)))
                               (vk::memory-resource-offset (draw-list-index-memory draw-list)) (foreign-array-foreign-type index-array))

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
	    
	    (let ((select-box-coords (krma-select-box-coords dpy)))
	      (setf (mem-aref pvalues2 :float +fragment-shader-select-box-min-offset+) (clampf (vx select-box-coords))
		    (mem-aref pvalues2 :float (1+ +fragment-shader-select-box-min-offset+)) (clampf (vy select-box-coords))
		    (mem-aref pvalues2 :float +fragment-shader-select-box-max-offset+) (clampf (vz select-box-coords))
		    (mem-aref pvalues2 :float (1+ +fragment-shader-select-box-max-offset+)) (clampf (vw select-box-coords))))

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

(defmethod render-draw-list ((pipeline draw-indexed-pipeline-mixin) draw-data draw-list dpy device command-buffer scene view proj viewport)
  (ubershader-render-draw-list pipeline draw-data draw-list dpy device command-buffer scene view proj
			       (viewport-x viewport) (viewport-y viewport)
			       (viewport-width viewport) (viewport-height viewport)))

(defun read-buffer (buffer lisp-array size memory-resource aligned-size)
  (let ((memory (allocated-memory buffer))
        (offset (vk::memory-resource-offset memory-resource))
        (device (vk::device buffer)))
    (with-foreign-object (pp-src :pointer)

      (check-vk-result (vkMapMemory (h device) (h memory) offset aligned-size 0 pp-src))

      (let ((p-src (mem-aref pp-src :pointer)))
	#+sbcl
	(sb-sys:with-pinned-objects (lisp-array)
	  (vk::memcpy (sb-sys:vector-sap lisp-array) p-src size))

	#+ccl
	(ccl::%copy-ptr-to-ivector p-src 0 lisp-array 0 size)
	
	(with-foreign-object (p-range '(:struct VkMappedMemoryRange))
	  (zero-struct p-range '(:struct VkMappedMemoryRange))

	  (with-foreign-slots ((%vk::sType
				%vk::memory
				%vk::size
                                %vk::offset)
                               p-range (:struct VkMappedMemoryRange))

	    (setf %vk::sType VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
		  %vk::memory (h memory)
		  %vk::size aligned-size
                  %vk::offset offset))

	  (check-vk-result (vkFlushMappedMemoryRanges (h device) 1 p-range))

	  (vkUnmapMemory (h device) (h memory))

	  (values))))))

(defun read-select-boxes (dpy frame-to-read)
  (let* ((coords (krma-select-box-coords dpy))
	 (cols (floor (- (vz coords) (vx coords))))
	 (rows (floor (- (vw coords) (vy coords)))))
    
    (when (aref (krma-select-box-2d-memory-resources dpy) frame-to-read)
    
      (let* ((size (* cols rows +select-box-2d-depth+))
	     (size-in-bytes (* size (foreign-type-size :unsigned-int)))
	     (aligned-size (aligned-size size-in-bytes)))

	(read-buffer (vk::memory-pool-buffer
		      (vk::storage-buffer-memory-pool dpy))
		     (array-displacement (krma-select-box-2d dpy)) size-in-bytes
		     (aref (krma-select-box-2d-memory-resources dpy) frame-to-read)
		     aligned-size)
      
	(clear-buffer (vk::memory-pool-buffer (vk::storage-buffer-memory-pool dpy)) 0 aligned-size
		      (aref (krma-select-box-2d-memory-resources dpy) frame-to-read))))

    (when (aref (krma-select-box-3d-memory-resources dpy) frame-to-read)

      (let* ((size (* cols rows +select-box-3d-depth+))
	     (size-in-bytes (* size (foreign-type-size :unsigned-int)))
	     (aligned-size (aligned-size size-in-bytes)))
	
	(read-buffer (vk::memory-pool-buffer
		      (vk::storage-buffer-memory-pool dpy))
		     (array-displacement (krma-select-box-3d dpy)) size-in-bytes
		     (aref (krma-select-box-3d-memory-resources dpy) frame-to-read)
		     aligned-size)

	(clear-buffer (vk::memory-pool-buffer (vk::storage-buffer-memory-pool dpy)) 0 aligned-size
		      (aref (krma-select-box-3d-memory-resources dpy) frame-to-read))))))
      
