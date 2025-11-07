(in-package :krma)

(defconstant +uber-vertex-shader-model-matrix-offset+ 0)
(defconstant +uber-vertex-shader-point-size-offset+ 16)
(defconstant +uber-vertex-shader-primitive-type-offset+ 17)
(defconstant +uber-vertex-shader-color-override-offset+  18)
(defconstant +uber-vertex-shader-override-color-p-offset+ 19)
(defconstant +uber-vertex-shader-instance-array-pointer-offset+ 20)

(defconstant +uber-vertex-shader-pc-size+ 22)

(defconstant +text-fragment-shader-px-range-offset+ 0)
(defconstant +lighting-fragment-shader-ambient-offset+ (1+ +text-fragment-shader-px-range-offset+))
(defconstant +lighting-fragment-shader-diffuse-offset+ (1+ +lighting-fragment-shader-ambient-offset+))
(defconstant +lighting-fragment-shader-specular-offset+ (1+ +lighting-fragment-shader-diffuse-offset+))
(defconstant +lighting-fragment-shader-shininess-offset+ (1+ +lighting-fragment-shader-specular-offset+))
(defconstant +lighting-fragment-shader-unused-offset+ (1+ +lighting-fragment-shader-shininess-offset+))
(defconstant +fragment-shader-select-box-min-offset+ (1+ +lighting-fragment-shader-unused-offset+))
(defconstant +fragment-shader-select-box-max-offset+ (+ +fragment-shader-select-box-min-offset+ 2))
							
(defconstant +fragment-shader-pc-size+ (+ +fragment-shader-select-box-max-offset+ 2))

(defcstruct %vk::vkBufferDeviceAddressInfo
  (%vk::sType %vk::VkStructureType)
  (%vk::pNext :pointer)
  (%vk::buffer %vk::VkBuffer))

(defconstant %vk::VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO 1000244001)

(defcfun (%vk::vkGetBufferOpaqueCaptureAddress "vkGetBufferOpaqueCaptureAddress")
    :uint64
  (device %vk::VkDevice)
  (pInfo :pointer))

(defctype %vk::VkDeviceAddress :uint64)

(defcfun (%vk::vkGetBufferDeviceAddress "vkGetBufferDeviceAddress")
    %vk::VkDeviceAddress
  (device %vk::VkDevice)
  (pInfo :pointer))

(defmacro %vk::with-vkBufferDeviceAddressInfo ((var) &body body)
  `(with-foreign-object (,var '(:struct %vk::vkBufferDeviceAddressInfo))
     (with-foreign-slots ((%vk::sType
			   %vk::pNext
			   %vk::buffer)
			  ,var (:struct %vk::vkBufferDeviceAddressInfo))
       (setf %vk::sType %vk::VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO
	     %vk::pNext (null-pointer))
       ,@body)))

(defclass pipeline-mixin ()
  ((display :reader pipeline-display :initarg :dpy)
   (name :initarg :name :reader pipeline-name :initform nil)
   (subpass :initarg :subpass)
   (pipeline-layout :accessor pipeline-layout)
   (device-pipeline :accessor device-pipeline)
   
   (global-descriptor-set-layout :initform nil)
   (global-descriptor-set :initform nil)
   (scene-descriptor-set-layout :initform nil)
   (per-instance-descriptor-set-layout :initform nil)
   
   (vertex-uniform-buffer :initform nil :accessor pipeline-vertex-uniform-buffer)
   (fragment-uniform-buffer :initform nil :accessor pipeline-fragment-uniform-buffer)))

(defclass draw-indexed-pipeline-mixin (pipeline-mixin)
  ())

(defclass ubershader-pipeline-mixin (draw-indexed-pipeline-mixin)

  ())

(defclass texture-pipeline-mixin (ubershader-pipeline-mixin)
  ())

(defclass 2d-pipeline-mixin ()
  ())

(defclass 2d-texture-pipeline-mixin (2d-pipeline-mixin texture-pipeline-mixin)
  ())

(defclass 3d-pipeline-mixin ()
  ())

(defclass 3d-texture-pipeline-mixin (3d-pipeline-mixin texture-pipeline-mixin)
  ())

(defclass 3d-texture-with-normals-pipeline-mixin (3d-texture-pipeline-mixin)
  ())

(defclass texture-image (vk::image)
  ((descriptor-set :accessor texture-image-descriptor-set)))

(defclass point-list-pipeline-mixin () ())

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

(defclass line-strip-pipeline-mixin () ())

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

(defclass triangle-strip-pipeline-mixin () ())

(defclass 2d-triangle-list-pipeline-mixin (triangle-list-pipeline-mixin
                                           2d-texture-pipeline-mixin)
  ())

(defclass 2d-instanced-line-pipeline (triangle-list-pipeline-mixin
				      2d-texture-pipeline-mixin)
  ())

(defclass 2d-triangle-list-pipeline (2d-triangle-list-pipeline-mixin)
  ())

(defclass 2d-triangle-strip-pipeline (triangle-strip-pipeline-mixin
                                      2d-texture-pipeline-mixin)
  ())

(defclass 3d-triangle-list-pipeline (triangle-list-pipeline-mixin
                                     3d-texture-pipeline-mixin)
  ())

(defclass 3d-instanced-tube-pipeline (triangle-list-pipeline-mixin
				      3d-texture-pipeline-mixin)
  ())

(defclass foreground-3d-instanced-line-pipeline (triangle-list-pipeline-mixin
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


