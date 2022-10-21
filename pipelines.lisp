(in-package :krma)

(defgeneric pipeline-topology (pipeline))

(defgeneric vertex-shader-pathname (pipeline))

(defgeneric fragment-shader-pathname (pipeline))

(defgeneric create-device-objects (pipeline &key app))

(defclass pipeline-mixin ()
  ((application :reader application :initarg :app)
   (name :initarg :name :reader pipeline-name :initform nil)

   ;; set 0, i.e. camera data (uniform buffer)
   (global-descriptor-set-layout :accessor global-descriptor-set-layout)
   (global-descriptor-set :accessor global-descriptor-set)

   ;; set 1, scene data (storage buffer)
   (scene-descriptor-set-layout :accessor scene-descriptor-set-layout)
   (scene-descriptor-set :accessor scene-descriptor-set)

   ;; set 2, per-cmd instance data (such as texture) (and maybe model mtx)
   (per-instance-descriptor-set-layout :accessor per-instance-descriptor-set-layout)

   (pipeline-layout :accessor pipeline-layout)
   (device-pipeline :accessor device-pipeline)
   (uniform-buffer :accessor uniform-buffer)
   (uniform-buffer-stage :accessor uniform-buffer-stage)))

(defmethod print-object ((object pipeline-mixin) stream)
  (print-unreadable-object (object stream)
    (if (pipeline-name object)
        (princ (pipeline-name object) stream)
        (princ (class-name (class-of object)) stream))))

(defmethod main-window ((pipeline pipeline-mixin))
  (with-slots (application) pipeline
    (main-window application)))

(defmethod allocator ((pipeline pipeline-mixin))
  (with-slots (application) pipeline
    (allocator application)))

(defmethod default-logical-device ((pipeline pipeline-mixin))
  (with-slots (application) pipeline
    (default-logical-device application)))

(defmethod pipeline-cache ((pipeline pipeline-mixin))
  (with-slots (application) pipeline
    (pipeline-cache application)))

(defmethod descriptor-pool ((pipeline pipeline-mixin))
  (with-slots (application) pipeline
    (default-descriptor-pool application)))

(defmethod render-pass ((pipeline pipeline-mixin))
  ;; currently all pipelines share the same render-pass
  ;; may change in the future.
  (render-pass (swapchain (main-window pipeline))))

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

(defmethod create-device-objects ((pipeline pipeline-mixin) &key app)
  (create-standard-pipeline-device-objects pipeline :app app))

(defmethod initialize-instance :after ((pipeline pipeline-mixin) &rest initargs
								       &key app)
  (declare (ignore initargs))
  (create-device-objects pipeline :app app)
  (values))

(defmethod make-global-descriptor-set-layout-bindings ((pipeline pipeline-mixin))
  (list (make-instance 'uniform-buffer-for-vertex-shader-dsl-binding)))

(defmethod make-scene-descriptor-set-layout-bindings ((pipeline pipeline-mixin))
  nil)

(defmethod make-per-instance-descriptor-set-layout-bindings ((pipeline pipeline-mixin))
  nil)

(defmethod make-push-constant-ranges ((pipeline pipeline-mixin))
  nil)

(defmethod pipeline-vertex-type ((pipeline pipeline-mixin))
  '(:struct textured-3d-vertex))

(defmethod pipeline-depth-test-enable? ((pipeline pipeline-mixin))
  t)

(defmethod pipeline-depth-write-enable? ((pipeline pipeline-mixin))
  t)

(defmethod pipeline-depth-compare-op ((pipeline pipeline-mixin))
  VK_COMPARE_OP_LESS_OR_EQUAL)

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

(defmethod make-global-descriptor-buffer-info ((pipeline pipeline-mixin))
  (list (make-instance 'descriptor-uniform-buffer-info
			           :buffer (uniform-buffer pipeline)
			           :range (foreign-type-size (uniform-buffer-type pipeline)))))

(defmethod make-scene-descriptor-buffer-info ((pipeline pipeline-mixin))
  nil)





(defclass standard-frame-resource (frame-specific-mixin)
  ;; there is one frame-resource object per frame per pipeline
  (;; index-buffers and vertex-buffers may appear again in the same array or in other pipelines
   ;; (but must be for the same frame-index)
   ;; consider edges to faces which use the same vertex-buffer, but different index-buffer
   (3d-point-list-draw-list :initform (make-instance '3d-vertex-small-draw-list))
   (3d-line-list-draw-list :initform (make-instance '3d-vertex-small-draw-list))
   (3d-line-strip-draw-list :initform (make-instance '3d-vertex-small-draw-list))
   (3d-triangle-list-draw-list :initform (make-instance '3d-vertex-small-draw-list))
   (3d-triangle-strip-draw-list :initform (make-instance '3d-vertex-small-draw-list))
   (2d-point-list-draw-list :initform (make-instance '2d-vertex-small-draw-list))
   (2d-line-list-draw-list :initform (make-instance '2d-vertex-small-draw-list))
   (2d-line-strip-draw-list :initform (make-instance '2d-vertex-small-draw-list))
   (2d-triangle-list-draw-list :initform (make-instance '2d-vertex-small-draw-list))
   (2d-triangle-strip-draw-list :initform (make-instance '2d-vertex-small-draw-list))
   (frame-work-queue :initform (sb-concurrency:make-queue) :reader frame-work-queue)))

(defmethod frame-resource-type ((scene standard-scene))
  'standard-frame-resource)

(defmethod get-frame-resource ((scene standard-scene) frame-index frame-count)
  (with-slots (frame-resources) scene
    (if frame-resources
        (if (elt frame-resources frame-index)
            (elt frame-resources frame-index)
            (setf (elt frame-resources frame-index)
                  (make-instance (frame-resource-type scene) :frame-number frame-index)))
        (progn (setf frame-resources (make-array frame-count :initial-element nil))
               (setf (elt frame-resources frame-index)
                     (make-instance (frame-resource-type scene) :frame-number frame-index))))))



(defclass draw-indexed-pipeline-mixin (pipeline-mixin)
  ())

(defmethod make-push-constant-ranges ((pipeline draw-indexed-pipeline-mixin))
  (list (make-instance 'push-constant-range
                       :stage-flags VK_SHADER_STAGE_VERTEX_BIT
                       :offset 0
                       :size (load-time-value (* 23 (foreign-type-size :uint32))))))

(defcstruct 3DMatrix
  (m00 :float)
  (m10 :float)
  (m20 :float)
  (m30 :float)

  (m01 :float)
  (m11 :float)
  (m21 :float)
  (m31 :float)

  (m02 :float)
  (m12 :float)
  (m22 :float)
  (m32 :float)

  (m03 :float)
  (m13 :float)
  (m23 :float)
  (m33 :float))

(defmethod uniform-buffer-type ((pipeline pipeline-mixin))
  '(:struct 3DMatrix))

(defclass texture-pipeline-mixin (draw-indexed-pipeline-mixin)
  ())

(defmethod fragment-shader-pathname ((pipeline texture-pipeline-mixin))
  (asdf/system:system-relative-pathname :krma "texture.frag.spv"))

(defmethod pipeline-min-sample-shading ((pipeline texture-pipeline-mixin))
  0.0f0)

(defmethod make-per-instance-descriptor-set-layout-bindings ((pipeline texture-pipeline-mixin))
  (list (make-instance 'descriptor-set-layout-binding
                       :type VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
                       :count 1
                       :flags VK_SHADER_STAGE_FRAGMENT_BIT
                       :samplers (list *sampler*))))


(defmethod create-device-objects :around ((pipeline texture-pipeline-mixin) &key app)
  (setf *sampler*
        (create-sampler (default-logical-device app) :allocator (allocator pipeline)))
  (call-next-method))

(defclass 2d-pipeline-mixin ()
  ())

(defmethod pipeline-vertex-type ((pipeline 2d-pipeline-mixin))
  '(:struct textured-2d-vertex))

(defmethod make-vertex-input-attribute-descriptions ((pipeline 2d-pipeline-mixin))
  (list (make-instance 'vertex-input-attribute-description
		               :location 0
		               :format VK_FORMAT_R32G32_SFLOAT
		               :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'x))
        (make-instance 'vertex-input-attribute-description
                       :location 1
                       :format VK_FORMAT_R32_UINT
                       :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'col))))

(defmethod pipeline-depth-test-enable? ((pipeline 2d-pipeline-mixin))
  nil)

(defmethod pipeline-depth-write-enable? ((pipeline 2d-pipeline-mixin))
  nil)

(defmethod pipeline-depth-compare-op ((pipeline 2d-pipeline-mixin))
  VK_COMPARE_OP_NEVER)

(defmethod pipeline-logic-op ((pipeline 2d-pipeline-mixin))
  VK_LOGIC_OP_CLEAR)

(defmethod pipeline-cull-mode ((pipeline 2d-pipeline-mixin))
  VK_CULL_MODE_NONE)

(defclass 2d-texture-pipeline-mixin (2d-pipeline-mixin texture-pipeline-mixin)
  ())

(defmethod vertex-shader-pathname ((pipeline 2d-texture-pipeline-mixin))
  (asdf/system:system-relative-pathname :krma "standard-2d.vert.spv"))

(defclass 3d-pipeline-mixin ()
  ())

(defmethod pipeline-cull-mode ((pipeline 3d-pipeline-mixin))
  VK_CULL_MODE_NONE)

(defmethod pipeline-vertex-type ((pipeline 3d-pipeline-mixin))
  '(:struct textured-3d-vertex))

(defmethod make-vertex-input-attribute-descriptions ((pipeline 3d-pipeline-mixin))
  (list (make-instance 'vertex-input-attribute-description
		               :location 0
		               :format VK_FORMAT_R32G32B32_SFLOAT
		               :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'x))
        (make-instance 'vertex-input-attribute-description
                       :location 1
                       :format VK_FORMAT_R32_UINT
                       :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'col))))

(defclass 3d-texture-pipeline-mixin (3d-pipeline-mixin texture-pipeline-mixin)
  ())

(defmethod vertex-shader-pathname ((pipeline 3d-texture-pipeline-mixin))
  (asdf/system:system-relative-pathname :krma "standard-3d.vert.spv"))

(defmethod pipeline-cull-mode ((pipeline 3d-texture-pipeline-mixin))
  VK_CULL_MODE_BACK_BIT)


(defclass 3d-texture-with-normal-pipeline-mixin (3d-texture-pipeline-mixin)
  ())

(defmethod pipeline-vertex-type ((pipeline 3d-texture-with-normal-pipeline-mixin))
  '(:struct textured-3d-vertex-with-normal))

(defmethod fragment-shader-pathname ((pipeline 3d-texture-with-normal-pipeline-mixin))
  (asdf/system:system-relative-pathname :krma "diffuse+texture.frag.spv"))

(defmethod vertex-shader-pathname ((pipeline 3d-texture-with-normal-pipeline-mixin))
  (asdf/system:system-relative-pathname :krma "3d-with-normal.vert.spv"))

(defclass texture-image (vk::image)
  ((descriptor-set :accessor texture-image-descriptor-set)))

(defun make-vulkan-texture (device queue sampler descriptor-set-layout descriptor-pool command-buffer bpp bitmap width height
                            &key (allocator (allocator device)))
  (let* ((descriptor-set (allocate-descriptor-set device (list descriptor-set-layout) descriptor-pool))
         (texture-image (create-image device width height :image-class 'texture-image :allocator allocator))
         (texture-image-view (create-image-view device texture-image :allocator allocator))
         (upload-size (* width height bpp #.(foreign-type-size :unsigned-char)))
         (upload-size-aligned (* (1+ (ceiling (/ (1- upload-size) +buffer-alignment+))) +buffer-alignment+)))

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

(defun cmd-set-line-width (command-buffer line-width)
  (vkCmdSetLineWidth (h command-buffer) (clampf line-width)))

;; initialize buffers for immediate mode should happen right after draw lists
;;                    are built in secondary render thread
;; initialize buffers for retained mode should happen in tertiary render thread
;;                    and amended in secondary render thread
(defun mmap-buffer (buffer array size
                    &optional
                      (aligned-size (* (1+ (ceiling (/ (1- size) +buffer-alignment+))) +buffer-alignment+)))
  (let ((memory (allocated-memory buffer))
	    (device (vk::device buffer)))
    (with-foreign-object (pp-dst :pointer)

      (check-vk-result (vkMapMemory (h device) (h memory) 0 aligned-size 0 pp-dst))

      (let ((p-dst (mem-aref pp-dst :pointer)))
	    (vk::memcpy p-dst array size)

	    (with-foreign-object (p-range '(:struct VkMappedMemoryRange))
	      (zero-struct p-range '(:struct VkMappedMemoryRange))

	      (with-foreign-slots ((%vk::sType
				                %vk::memory
				                %vk::size)
			                   p-range (:struct VkMappedMemoryRange))

	        (setf %vk::sType VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
		          %vk::memory (h memory)
		          %vk::size VK_WHOLE_SIZE))

	      (check-vk-result (vkFlushMappedMemoryRanges (h device) 1 p-range))

	      (vkUnmapMemory (h device) (h memory))

	      (values))))))

(defun initialize-buffers (device draw-list)
  (let ((vertex-array (draw-list-vertex-array draw-list))
        (index-array (draw-list-index-array draw-list))
        (vertex-buffer (draw-list-vertex-buffer draw-list))
        (index-buffer (draw-list-index-buffer draw-list)))

    (let ((index-size (* (foreign-array-fill-pointer index-array)
                         (foreign-array-foreign-type-size index-array)))
          (vertex-size (* (foreign-array-fill-pointer vertex-array)
                          (foreign-array-foreign-type-size vertex-array))))

	  (labels ((new-buffer (class usage size)
			     (let ((size-aligned (* (1+ (ceiling (/ (1- size) +buffer-alignment+))) +buffer-alignment+)))

                   (let ((buffer
                           (create-buffer-1 device size-aligned usage
                                            :buffer-class class :allocator (allocator device))))

                     (new-memory buffer)
                     buffer)))

		       (new-memory (buffer)
                 (let ((buffer-memory
				         (allocate-buffer-memory device buffer VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
							                     :allocator (allocator device))))

			       (setf (allocated-memory buffer) buffer-memory)

			       (bind-buffer-memory device buffer buffer-memory)))

		       (free-buffer (buffer)
			     (vkDestroyBuffer (h device) (h buffer) (h (allocator buffer)))

			     (when (allocated-memory buffer)
			       (free-memory buffer)))

		       (free-memory (buffer)
			     (vkFreeMemory (h device) (h (allocated-memory buffer)) (h (allocator (allocated-memory buffer))))))

        (cond ((null vertex-buffer)
               (setf (draw-list-vertex-buffer draw-list)
                     (new-buffer 'vertex-buffer VK_BUFFER_USAGE_VERTEX_BUFFER_BIT vertex-size)))
		      ((or (null (allocated-memory vertex-buffer))
			       (< (vk::size vertex-buffer) vertex-size))
		       (free-buffer vertex-buffer)
               (setf (draw-list-vertex-buffer draw-list)
                     (new-buffer 'vertex-buffer VK_BUFFER_USAGE_VERTEX_BUFFER_BIT vertex-size))))

        (cond ((null index-buffer)
               (setf (draw-list-index-buffer draw-list)
                     (new-buffer 'index-buffer VK_BUFFER_USAGE_INDEX_BUFFER_BIT index-size)))
		      ((or (null (allocated-memory index-buffer))
			       (< (vk::size index-buffer) index-size))
		       (free-buffer index-buffer)
               (setf (draw-list-index-buffer draw-list)
                     (new-buffer 'index-buffer VK_BUFFER_USAGE_INDEX_BUFFER_BIT index-size))))

        (let ((vertex-buffer (draw-list-vertex-buffer draw-list))
              (index-buffer (draw-list-index-buffer draw-list)))

          (unless (zerop vertex-size)
            (mmap-buffer vertex-buffer
                         (foreign-array-ptr vertex-array) vertex-size (vk::size vertex-buffer)))

          (unless (zerop index-size)
            (mmap-buffer index-buffer
                         (foreign-array-ptr index-array) index-size (vk::size index-buffer))))

        (values)))))

(defun copy-matrix-to-foreign (lisp-matrix p-matrix)
  ;; glsl expects transpose of what is in marr of mat4
  (let ((array (marr lisp-matrix)))
    (loop for i from 0 below 4
       do (loop for j from 0 below 4
	     do (setf (mem-aref p-matrix :float (+ j (* i 4)))
		      (clampf (aref array (+ i (* j 4)))))))
    (values)))

(defmethod update-uniform-buffer ((pipeline pipeline-mixin) model-matrix view-matrix projection-matrix)
  (with-slots (uniform-buffer) pipeline
    (let ((type (uniform-buffer-type pipeline))
	      (p-stage (uniform-buffer-stage pipeline)))
      (copy-matrix-to-foreign model-matrix (foreign-slot-pointer p-stage type 'model))
      (copy-matrix-to-foreign view-matrix (foreign-slot-pointer p-stage type 'view))
      (copy-matrix-to-foreign projection-matrix (foreign-slot-pointer p-stage type 'proj))
      (copy-uniform-buffer-memory (default-logical-device pipeline)
				                  p-stage
				                  (allocated-memory uniform-buffer)
				                  (foreign-type-size type)))
    (values)))

(defmethod update-uniform-buffer-2 ((pipeline pipeline-mixin) vproj)
  (with-slots (uniform-buffer) pipeline
    (let ((type (uniform-buffer-type pipeline))
	      (p-stage (uniform-buffer-stage pipeline)))
      (copy-matrix-to-foreign vproj p-stage)
      (copy-uniform-buffer-memory (default-logical-device pipeline)
				                  p-stage
				                  (allocated-memory uniform-buffer)
				                  (foreign-type-size type)))
    (values)))

(defun create-standard-pipeline-device-objects (pipeline
						                        &key
                                                  app
						                          (render-pass (render-pass pipeline))

                                                  ;; set 0
						                          (global-bindings ;; i.e. camera uniform buffer
                                                   (make-global-descriptor-set-layout-bindings pipeline))

                                                  ;; set 1
                                                  (scene-bindings ;; i.e. storage buffer with all object data for scene
                                                   (make-scene-descriptor-set-layout-bindings pipeline))

                                                  ;; set 2
                                                  (instance-bindings ;; i.e. texture image or model matrix
                                                   (make-per-instance-descriptor-set-layout-bindings pipeline))

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
  (let ((device (default-logical-device app)))
    (let ((vtx-shader (create-shader-module-from-file device (vertex-shader-pathname pipeline)))
	      (frg-shader (create-shader-module-from-file device (fragment-shader-pathname pipeline))))

	  (setf (global-descriptor-set-layout pipeline)
            (create-descriptor-set-layout device :bindings global-bindings))

      (setf (scene-descriptor-set-layout pipeline)
            (create-descriptor-set-layout device :bindings scene-bindings))

      (setf (per-instance-descriptor-set-layout pipeline)
            (create-descriptor-set-layout device :bindings instance-bindings))

	  (setf (pipeline-layout pipeline)
	        (create-pipeline-layout device (list (global-descriptor-set-layout pipeline)
                                                 (scene-descriptor-set-layout pipeline)
                                                 (per-instance-descriptor-set-layout pipeline))
				                    :push-constant-ranges push-constant-ranges)
	        (device-pipeline pipeline)
	        (apply #'create-graphics-pipeline device (pipeline-cache pipeline) (pipeline-layout pipeline)
		           render-pass 3 vtx-shader frg-shader
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

	  (setf (uniform-buffer pipeline)
	        (create-uniform-buffer device (foreign-type-size (uniform-buffer-type pipeline))))

	  (setf (uniform-buffer-stage pipeline)
	        (foreign-alloc (uniform-buffer-type pipeline)))

      (setf (global-descriptor-set pipeline)
	        (create-descriptor-set
	         device
	         (list (global-descriptor-set-layout pipeline))
	         (descriptor-pool pipeline)
	         :descriptor-buffer-info (make-global-descriptor-buffer-info pipeline)))

      (setf (scene-descriptor-set pipeline)
	        (create-descriptor-set
	         device
	         (list (scene-descriptor-set-layout pipeline))
	         (descriptor-pool pipeline)
	         :descriptor-buffer-info (make-scene-descriptor-buffer-info pipeline)))

	  pipeline)))

(defmethod pipeline-vertex-type ((pipeline 2d-texture-pipeline-mixin))
  '(:struct textured-2d-vertex))

(defmethod make-vertex-input-attribute-descriptions ((pipeline 2d-texture-pipeline-mixin))
  (list (make-instance 'vertex-input-attribute-description
		               :location 0
		               :format VK_FORMAT_R32G32_SFLOAT
		               :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'x))
	    (make-instance 'vertex-input-attribute-description
		               :location 1
		               :format VK_FORMAT_R32G32_SFLOAT
		               :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'u))
        (make-instance 'vertex-input-attribute-description
                       :location 2
                       :format VK_FORMAT_R32_UINT
                       :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'col))))

(defmethod pipeline-vertex-type ((pipeline 3d-texture-pipeline-mixin))
  '(:struct textured-3d-vertex))

(defmethod make-vertex-input-attribute-descriptions ((pipeline 3d-texture-pipeline-mixin))
  (list (make-instance 'vertex-input-attribute-description
                       :location 0
                       :format VK_FORMAT_R32G32B32_SFLOAT
		               :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'x))
	    (make-instance 'vertex-input-attribute-description
		               :location 1
		               :format VK_FORMAT_R32G32_SFLOAT
		               :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'u))
        (make-instance 'vertex-input-attribute-description
                       :location 2
                       :format VK_FORMAT_R32_UINT
                       :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'col))))

(defmethod make-vertex-input-attribute-descriptions ((pipeline 3d-texture-with-normal-pipeline-mixin))
  (list (make-instance 'vertex-input-attribute-description
                       :location 0
                       :format VK_FORMAT_R32G32B32_SFLOAT
		               :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'x))
	    (make-instance 'vertex-input-attribute-description
		               :location 1
		               :format VK_FORMAT_R32G32_SFLOAT
		               :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'u))
        (make-instance 'vertex-input-attribute-description
                       :location 2
                       :format VK_FORMAT_R32_UINT
                       :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'col))
        (make-instance 'vertex-input-attribute-description
                       :location 3
                       :format VK_FORMAT_R32G32B32_SFLOAT
		               :offset (foreign-slot-offset (pipeline-vertex-type pipeline) 'nx))))


(defclass point-list-pipeline-mixin () ())

(defmethod pipeline-topology ((pipeline point-list-pipeline-mixin))
  VK_PRIMITIVE_TOPOLOGY_POINT_LIST)

(defmethod make-push-constant-ranges ((pipeline point-list-pipeline-mixin))
  (list (make-instance 'push-constant-range
                       :stage-flags VK_SHADER_STAGE_VERTEX_BIT
                       :offset 0
                       :size (load-time-value (* 1 (foreign-type-size :float))))))

(defmethod make-per-instance-descriptor-set-layout-bindings ((pipeline point-list-pipeline-mixin))
  nil)

(defmethod fragment-shader-pathname ((pipeline point-list-pipeline-mixin))
  (asdf/system:system-relative-pathname :krma "notexture.frag.spv"))

(defclass 2d-point-list-pipeline (point-list-pipeline-mixin
                                  2d-pipeline-mixin
                                  draw-indexed-pipeline-mixin)
  ())

(defmethod vertex-shader-pathname ((pipeline 2d-point-list-pipeline))
  (asdf/system:system-relative-pathname :krma "standard-2d-point.vert.spv"))

(defclass 3d-point-list-pipeline (point-list-pipeline-mixin
                                  3d-pipeline-mixin
                                  draw-indexed-pipeline-mixin)
  ())

(defmethod vertex-shader-pathname ((pipeline 3d-point-list-pipeline))
  (asdf/system:system-relative-pathname :krma "standard-3d-point.vert.spv"))

(progn
(defmethod pipeline-depth-test-enable? ((pipeline 3d-point-list-pipeline))
  nil)

(defmethod pipeline-depth-write-enable? ((pipeline 3d-point-list-pipeline))
  nil)

(defmethod pipeline-depth-compare-op ((pipeline 3d-point-list-pipeline))
  VK_COMPARE_OP_NEVER)

(defmethod pipeline-logic-op ((pipeline 3d-point-list-pipeline))
  VK_LOGIC_OP_CLEAR)

(defmethod pipeline-cull-mode ((pipeline 3d-point-list-pipeline))
  VK_CULL_MODE_NONE)
)

(defclass line-pipeline-mixin () ())

(defmethod fragment-shader-pathname ((pipeline line-pipeline-mixin))
  (asdf/system:system-relative-pathname :krma "notexture.frag.spv"))

(defclass 2d-line-pipeline-mixin (line-pipeline-mixin 2d-pipeline-mixin) ())


(defclass 3d-line-pipeline-mixin (line-pipeline-mixin 3d-pipeline-mixin) ())


(defclass line-list-pipeline-mixin () ())

(defmethod pipeline-topology ((pipeline line-list-pipeline-mixin))
  VK_PRIMITIVE_TOPOLOGY_LINE_LIST)

(defclass line-strip-pipeline-mixin () ())

(defmethod make-push-constant-ranges ((pipeline line-strip-pipeline-mixin))
  (list (make-instance 'push-constant-range
                       :stage-flags VK_SHADER_STAGE_VERTEX_BIT
                       :offset 0
                       :size (load-time-value (* 18 (foreign-type-size :uint32))))))

(defmethod pipeline-topology ((pipeline line-strip-pipeline-mixin))
  VK_PRIMITIVE_TOPOLOGY_LINE_STRIP)

(defclass 2d-line-list-pipeline (line-list-pipeline-mixin
                                 2d-line-pipeline-mixin
                                 draw-indexed-pipeline-mixin)
  ())

(defmethod vertex-shader-pathname ((pipeline 2d-line-list-pipeline))
  (asdf/system:system-relative-pathname :krma "standard-2d-line-list.vert.spv"))

(defclass 2d-line-strip-pipeline (line-strip-pipeline-mixin
                                  2d-line-pipeline-mixin
                                  draw-indexed-pipeline-mixin)
  ())

(defmethod vertex-shader-pathname ((pipeline 2d-line-strip-pipeline))
  (asdf/system:system-relative-pathname :krma "standard-2d-line-strip.vert.spv"))

(defclass 3d-line-list-pipeline (line-list-pipeline-mixin
                                 3d-line-pipeline-mixin
                                 draw-indexed-pipeline-mixin)
  ())

(defmethod vertex-shader-pathname ((pipeline 3d-line-list-pipeline))
  (asdf/system:system-relative-pathname :krma "standard-3d-line-list.vert.spv"))

(defclass 3d-line-strip-pipeline (line-strip-pipeline-mixin
                                  3d-line-pipeline-mixin
                                  draw-indexed-pipeline-mixin)
  ())

(defmethod vertex-shader-pathname ((pipeline 3d-line-strip-pipeline))
  (asdf/system:system-relative-pathname :krma "standard-3d-line-strip.vert.spv"))

(defclass triangle-list-pipeline-mixin () ())

(defmethod pipeline-topology ((pipeline triangle-list-pipeline-mixin))
  VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST)

(defclass triangle-strip-pipeline-mixin () ())

(defmethod pipeline-topology ((pipeline triangle-strip-pipeline-mixin))
  VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP)



(defclass 2d-triangle-list-pipeline-mixin (triangle-list-pipeline-mixin
                                           2d-texture-pipeline-mixin
                                           draw-indexed-pipeline-mixin)
  ())

(defmethod fragment-shader-pathname ((pipeline 2d-triangle-list-pipeline-mixin))
  (asdf/system:system-relative-pathname :krma "texture.frag.spv"))

(defmethod vertex-shader-pathname ((pipeline 2d-triangle-list-pipeline-mixin))
  (asdf/system:system-relative-pathname :krma "standard-2d.vert.spv"))

(defclass 2d-triangle-list-pipeline (2d-triangle-list-pipeline-mixin)
  ())

(defclass msdf-text-pipeline (2d-triangle-list-pipeline-mixin)
  ())

(defmethod fragment-shader-pathname ((pipeline msdf-text-pipeline))
  (asdf/system:system-relative-pathname :krma "msdf-texture.frag.spv"))

(defmethod make-push-constant-ranges ((pipeline msdf-text-pipeline))
  (append (call-next-method)
          (list (make-instance 'push-constant-range
                               :stage-flags VK_SHADER_STAGE_FRAGMENT_BIT
                               :offset (load-time-value (* 23 (foreign-type-size :uint32)))
                               :size (load-time-value (* 5 (foreign-type-size :float)))))))

(defclass 2d-triangle-strip-pipeline (triangle-strip-pipeline-mixin
                                      2d-texture-pipeline-mixin
                                      draw-indexed-pipeline-mixin)
  ())

(defclass 3d-triangle-list-pipeline (triangle-list-pipeline-mixin
                                     3d-texture-pipeline-mixin
                                     draw-indexed-pipeline-mixin)
  ())

(defmethod fragment-shader-pathname ((pipeline 3d-triangle-list-pipeline))
  (asdf/system:system-relative-pathname :krma "texture.frag.spv"))

(defmethod vertex-shader-pathname ((pipeline 3d-triangle-list-pipeline))
  (asdf/system:system-relative-pathname :krma "standard-3d.vert.spv"))

(defclass 3d-triangle-list-with-normal-pipeline (triangle-list-pipeline-mixin
                                                 3d-texture-with-normal-pipeline-mixin
                                                 draw-indexed-pipeline-mixin)
  ())

(defclass 3d-triangle-strip-pipeline (triangle-strip-pipeline-mixin
                                      3d-texture-pipeline-mixin
                                      draw-indexed-pipeline-mixin)
  ())


;;------

(defun render-standard-draw-indexed-cmd (cmd command-buffer pipeline-layout index-buffer
                                         &optional (default-line-width *default-line-thickness*)
                                           (default-point-size *default-point-size*))
  ;; so we have such a thing as a "standard-draw-indexed-cmd".
  ;; this standard cmd is used any type of primitive: points, line-lists, line-strips, triangle-lists, triangle-strips
  ;; because we may use the same cmd object for different primitives/pipelines/pipelines
  ;; for example, we may want to render something which is essentially a triangle list as a line list
  ;; to show edges, or another example would be to show endpoints to lines
  ;; so the standard-draw-indexed-cmd carries line-thickness and point-size regardless of if it is a triangle primitive
  ;; the standard-2d and standard-3d vertex shader will pass them through or use them per primitive type
  ;; standard-draw-indexed-cmd also has a slot for texture

  ;; vertex buffers have been bound by the time we reach this function
  ;; so has pipeline and descriptor sets
  (let ((command-buffer-handle (h command-buffer)))

    (let ((line-width (cmd-line-thickness cmd))
          (color-override (cmd-color-override cmd))
          (model-mtx (cmd-model-mtx cmd))
          (point-size (cmd-point-size cmd))
          (texture (cmd-texture cmd)))

      (let ((descriptor-set (texture-image-descriptor-set texture)))
        (with-foreign-objects ((p-descriptor-sets :pointer 1))
          (setf (mem-aref p-descriptor-sets :pointer 0) (h descriptor-set))
          (vkCmdBindDescriptorSets (h command-buffer)
                                   VK_PIPELINE_BIND_POINT_GRAPHICS
                                   (h pipeline-layout)
                                   2 1
                                   p-descriptor-sets
                                   0 +nullptr+)))

      ;;(VkCmdBindIndexBuffer command-buffer-handle (h index-buffer) 0 (foreign-type-enum index-buffer))
      (cmd-bind-index-buffer command-buffer index-buffer 0 :unsigned-short)

      #-darwin
      (if line-width
          (vkCmdSetLineWidth command-buffer-handle (clampf line-width))
          (vkCmdSetLineWidth command-buffer-handle (clampf default-line-width)))

      (with-foreign-object (pvalues :uint32 23)
        ;; push constant is model-matrix followed by color, followed by point size
        ;; followed by bool if to use color-override
        ;; this is so every command can set the model-matrix
        ;; and override the color of the vertices
        (if model-mtx
            (copy-matrix-to-foreign model-mtx pvalues)
            (copy-matrix-to-foreign *identity-matrix* pvalues))

        (if color-override
            (let ((pcol (mem-aptr pvalues :uint32 16)))
              (setf (mem-aref pcol :uint32 0) color-override))
            (let ((pcol (mem-aptr pvalues :uint32 16)))
              (setf (mem-aref pcol :uint32 0) #xff0000ff)))

        (let ((psize (mem-aptr pvalues :uint32 17)))
          (if point-size
              (setf (mem-aref psize :float) (clampf point-size))
              (setf (mem-aref psize :float) (clampf default-point-size))))

        (setf (mem-aref pvalues :uint32 18) (if color-override 1 0))

        (let ((plp (mem-aptr pvalues :uint32 20))
              (light-pos (cmd-light-position cmd)))
          (if light-pos
              (setf (mem-aref plp :float 0) (clampf (vx light-pos))
                    (mem-aref plp :float 1) (clampf (vy light-pos))
                    (mem-aref plp :float 2) (clampf (vz light-pos)))
              (setf (mem-aref plp :float 0) 10000.0f0
                    (mem-aref plp :float 1) 10000.0f0
                    (mem-aref plp :float 2) 10000.0f0)))

        (vkCmdPushConstants command-buffer-handle
                            (h pipeline-layout)
                            VK_SHADER_STAGE_VERTEX_BIT
                            0
                            (load-time-value (* 23 (foreign-type-size :uint32)))
                            pvalues))

      (when (typep cmd 'text-draw-indexed-cmd)
        (with-foreign-object (pvalues :float 5)
          (let ((px-range (font-px-range (cmd-font cmd))))
            (if px-range
                (setf (mem-aref pvalues :float 0) (clampf px-range))
                (setf (mem-aref pvalues :float 0) 32.0f0))
            (setf (mem-aref pvalues :float 1) 0.0f0
                  (mem-aref pvalues :float 2) 0.0f0
                  (mem-aref pvalues :float 3) 0.0f0
                  (mem-aref pvalues :float 4) 1.0f0))

          (vkCmdPushConstants command-buffer-handle
                              (h pipeline-layout)
                              VK_SHADER_STAGE_FRAGMENT_BIT
                              (load-time-value (* 23 (foreign-type-size :uint32)))
                              (load-time-value (* 5 (foreign-type-size :float)))
                              pvalues)))

      (vkCmdDrawIndexed command-buffer-handle (cmd-elem-count cmd) 1 (cmd-first-idx cmd) (cmd-vtx-offset cmd) 0))))


(defun render-draw-indexed-buffers (pipeline
                                    draw-list
                                    command-buffer
                                    model-matrix view-matrix projection-matrix
                                    width height)
  (let ((cmd-vector (draw-list-cmd-vector draw-list)))
    (unless (zerop (fill-pointer cmd-vector))
      (let* ((pipeline-layout (pipeline-layout pipeline))
             (index-buffer (draw-list-index-buffer draw-list))
             (line-width (pipeline-line-width pipeline))
             (point-size (pipeline-point-size pipeline)))
        (cmd-bind-pipeline command-buffer (device-pipeline pipeline) :bind-point :graphics)
        (update-uniform-buffer-2 pipeline (m* projection-matrix view-matrix model-matrix))
        (cmd-set-viewport command-buffer :width width :height height :min-depth 0.0 :max-depth 1.0)
        (cmd-set-scissor command-buffer :width width :height height)

        (cmd-bind-descriptor-sets command-buffer pipeline-layout (list (global-descriptor-set pipeline)))

        (with-foreign-objects ((p-descriptor-sets :pointer 1))
          (setf (mem-aref p-descriptor-sets :pointer 0) (h (global-descriptor-set pipeline)))
          (vkCmdBindDescriptorSets (h command-buffer)
                                   VK_PIPELINE_BIND_POINT_GRAPHICS
                                   (h pipeline-layout)
                                   0 1
                                   p-descriptor-sets
                                   0 +nullptr+))

        (with-foreign-objects ((p-descriptor-sets :pointer 1))
          (setf (mem-aref p-descriptor-sets :pointer 0) (h (scene-descriptor-set pipeline)))
          (vkCmdBindDescriptorSets (h command-buffer)
                                   VK_PIPELINE_BIND_POINT_GRAPHICS
                                   (h pipeline-layout)
                                   1 1
                                   p-descriptor-sets
                                   0 +nullptr+))

        (cmd-bind-vertex-buffers command-buffer (list (draw-list-vertex-buffer draw-list)))
        (loop for cmd across cmd-vector
              when cmd
                do (render-standard-draw-indexed-cmd cmd command-buffer pipeline-layout
                                                     index-buffer line-width point-size)))
      t)))

(defmethod render ((pipeline draw-indexed-pipeline-mixin) draw-list device command-buffer
                   model-matrix view-matrix projection-matrix
                   width height)
  (unless (zerop (foreign-array-fill-pointer (draw-list-index-array draw-list)))
    (initialize-buffers device draw-list)
    (render-draw-indexed-buffers pipeline draw-list command-buffer
                                 model-matrix view-matrix projection-matrix
                                 width height))
  (values))

(defmethod render ((pipeline point-list-pipeline-mixin)
                   draw-list device command-buffer
                   model-matrix view-matrix projection-matrix
                   width height)

  (let ((index-array (draw-list-index-array draw-list)))

    (unless (zerop (foreign-array-fill-pointer index-array))

      (initialize-buffers device draw-list)

      (let* ((pipeline-layout (pipeline-layout pipeline))
             (index-buffer (draw-list-index-buffer draw-list))
             (point-size (pipeline-point-size pipeline))
             (command-buffer-handle (h command-buffer)))

        (cmd-bind-pipeline command-buffer (device-pipeline pipeline) :bind-point :graphics)

        (update-uniform-buffer-2 pipeline (m* projection-matrix view-matrix model-matrix))

        (cmd-set-viewport command-buffer :width width :height height :min-depth 0.0 :max-depth 1.0)

        (cmd-set-scissor command-buffer :width width :height height)

        (with-foreign-objects ((p-descriptor-sets :pointer 1))
          (setf (mem-aref p-descriptor-sets :pointer 0) (h (global-descriptor-set pipeline)))
          (vkCmdBindDescriptorSets command-buffer-handle
                                   VK_PIPELINE_BIND_POINT_GRAPHICS
                                   (h pipeline-layout)
                                   0 1
                                   p-descriptor-sets
                                   0 +nullptr+))

        (with-foreign-objects ((p-descriptor-sets :pointer 1))
          (setf (mem-aref p-descriptor-sets :pointer 0) (h (scene-descriptor-set pipeline)))
          (vkCmdBindDescriptorSets command-buffer-handle
                                   VK_PIPELINE_BIND_POINT_GRAPHICS
                                   (h pipeline-layout)
                                   1 1
                                   p-descriptor-sets
                                   0 +nullptr+))

        (cmd-bind-vertex-buffers command-buffer (list (draw-list-vertex-buffer draw-list)))

        (cmd-bind-index-buffer command-buffer index-buffer 0 (foreign-array-foreign-type index-array))

        (with-foreign-object (pvalues :float 1)
          (setf (mem-aref pvalues :float) (clampf point-size))

          (vkCmdPushConstants command-buffer-handle
                              (h pipeline-layout)
                              VK_SHADER_STAGE_VERTEX_BIT
                              0
                              (load-time-value (* 1 (foreign-type-size :float)))
                              pvalues))

        (vkCmdDrawIndexed command-buffer-handle
                          (foreign-array-fill-pointer index-array)
                          1 0 0 0))))
  (values))

(defmethod render ((pipeline line-list-pipeline-mixin)
                   draw-list device command-buffer
                   model-matrix view-matrix projection-matrix
                   width height)

  (let ((index-array (draw-list-index-array draw-list)))

    (unless (zerop (foreign-array-fill-pointer index-array))

      (initialize-buffers device draw-list)

      (let* ((pipeline-layout (pipeline-layout pipeline))
             (index-buffer (draw-list-index-buffer draw-list))
             (line-width (pipeline-line-width pipeline))
             (command-buffer-handle (h command-buffer)))

        (cmd-bind-pipeline command-buffer (device-pipeline pipeline) :bind-point :graphics)

        (update-uniform-buffer-2 pipeline (m* projection-matrix view-matrix model-matrix))

        (cmd-set-viewport command-buffer :width width :height height :min-depth 0.0 :max-depth 1.0)

        (cmd-set-scissor command-buffer :width width :height height)

        (with-foreign-objects ((p-descriptor-sets :pointer 1))
          (setf (mem-aref p-descriptor-sets :pointer 0) (h (global-descriptor-set pipeline)))
          (vkCmdBindDescriptorSets command-buffer-handle
                                   VK_PIPELINE_BIND_POINT_GRAPHICS
                                   (h pipeline-layout)
                                   0 1
                                   p-descriptor-sets
                                   0 +nullptr+))

        (with-foreign-objects ((p-descriptor-sets :pointer 1))
          (setf (mem-aref p-descriptor-sets :pointer 0) (h (scene-descriptor-set pipeline)))
          (vkCmdBindDescriptorSets command-buffer-handle
                                   VK_PIPELINE_BIND_POINT_GRAPHICS
                                   (h pipeline-layout)
                                   1 1
                                   p-descriptor-sets
                                   0 +nullptr+))

        (cmd-bind-vertex-buffers command-buffer (list (draw-list-vertex-buffer draw-list)))

        (cmd-bind-index-buffer command-buffer index-buffer 0 (foreign-array-foreign-type index-array))

        #-darwin
        (if line-width
            (vkCmdSetLineWidth command-buffer-handle (clampf line-width))
            (vkCmdSetLineWidth command-buffer-handle (clampf *default-line-thickness*)))

        (vkCmdDrawIndexed command-buffer-handle
                          (foreign-array-fill-pointer index-array)
                          1 0 0 0))))
  (values))

(defmethod render ((pipeline line-strip-pipeline-mixin)
                   draw-list device command-buffer
                   model-matrix view-matrix projection-matrix
                   width height)

  (unless (zerop (foreign-array-fill-pointer (draw-list-index-array draw-list)))
    (initialize-buffers device draw-list)
    (let ((cmd-vector (draw-list-cmd-vector draw-list)))
      (unless (zerop (fill-pointer cmd-vector))
        (let* ((pipeline-layout (pipeline-layout pipeline))
               (index-buffer (draw-list-index-buffer draw-list))
               (pipeline-line-width (pipeline-line-width pipeline)))

          (cmd-bind-pipeline command-buffer (device-pipeline pipeline) :bind-point :graphics)

          (update-uniform-buffer-2 pipeline (m* projection-matrix view-matrix model-matrix))

          (cmd-set-viewport command-buffer :width width :height height :min-depth 0.0 :max-depth 1.0)

          (cmd-set-scissor command-buffer :width width :height height)

          (with-foreign-objects ((p-descriptor-sets :pointer 1))
            (setf (mem-aref p-descriptor-sets :pointer 0) (h (global-descriptor-set pipeline)))
            (vkCmdBindDescriptorSets (h command-buffer)
                                     VK_PIPELINE_BIND_POINT_GRAPHICS
                                     (h pipeline-layout)
                                     0 1
                                     p-descriptor-sets
                                     0 +nullptr+))

          (with-foreign-objects ((p-descriptor-sets :pointer 1))
            (setf (mem-aref p-descriptor-sets :pointer 0) (h (scene-descriptor-set pipeline)))
            (vkCmdBindDescriptorSets (h command-buffer)
                                     VK_PIPELINE_BIND_POINT_GRAPHICS
                                     (h pipeline-layout)
                                     1 1
                                     p-descriptor-sets
                                     0 +nullptr+))

          (cmd-bind-vertex-buffers command-buffer (list (draw-list-vertex-buffer draw-list)))

          (loop for cmd across cmd-vector
                when cmd
                  do (let ((command-buffer-handle (h command-buffer)))

                       (let ((line-width (cmd-line-thickness cmd))
                             (color-override (cmd-color-override cmd))
                             (model-mtx (cmd-model-mtx cmd)))

                         ;;(VkCmdBindIndexBuffer command-buffer-handle (h index-buffer) 0 (foreign-type-enum index-buffer))
                         (cmd-bind-index-buffer command-buffer index-buffer 0 :unsigned-short)

                         #-darwin
                         (if line-width
                             (vkCmdSetLineWidth command-buffer-handle (clampf line-width))
                             (vkCmdSetLineWidth command-buffer-handle (clampf pipeline-line-width)))

                         (with-foreign-object (pvalues :uint32 18)
                           ;; push constant is model-matrix followed by color, followed by point size
                           ;; followed by bool if to use color-override
                           ;; this is so every command can set the model-matrix
                           ;; and override the color of the vertices
                           (if model-mtx
                               (copy-matrix-to-foreign model-mtx pvalues)
                               (copy-matrix-to-foreign *identity-matrix* pvalues))

                           (if color-override
                               (let ((pcol (mem-aptr pvalues :uint32 16)))
                                 (setf (mem-aref pcol :uint32 0) color-override))
                               (let ((pcol (mem-aptr pvalues :uint32 16)))
                                 (setf (mem-aref pcol :uint32 0) #xff0000ff)))

                           (setf (mem-aref pvalues :uint32 17) (if color-override 1 0))

                           (vkCmdPushConstants command-buffer-handle
                                               (h pipeline-layout)
                                               VK_SHADER_STAGE_VERTEX_BIT
                                               0
                                               (load-time-value (* 18 (foreign-type-size :uint32)))
                                               pvalues))

                         (vkCmdDrawIndexed command-buffer-handle (cmd-elem-count cmd) 1 (cmd-first-idx cmd) (cmd-vtx-offset cmd) 0)))))
        t))))
