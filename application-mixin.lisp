(in-package :krma)

(defclass pipeline-store ()
  ;; singleton object
  ((3d-point-list-pipeline)
   (3d-line-list-pipeline)
   (3d-line-strip-pipeline)
   (3d-triangle-list-pipeline)
   (3d-triangle-strip-pipeline)
   (2d-point-list-pipeline)
   (2d-line-list-pipeline)
   (2d-line-strip-pipeline)
   (2d-triangle-list-pipeline)
   (2d-triangle-strip-pipeline)))

(defmethod initialize-instance :after ((instance pipeline-store) &rest initargs &key app)
  (declare (ignore initargs))
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
      instance

    (setf 3d-point-list-pipeline (make-instance '3d-point-list-pipeline
                                                :app app
                                                :name :3d-point-list-pipeline)
          3d-line-list-pipeline (make-instance '3d-line-list-pipeline
                                               :app app
                                               :name :3d-line-list-pipeline)
          3d-line-strip-pipeline (make-instance '3d-line-strip-pipeline
                                                :app app
                                                :name :3d-line-strip-pipeline)
          3d-triangle-list-pipeline (make-instance '3d-triangle-list-pipeline
                                                   :app app
                                                   :name :3d-triangle-list-pipeline)
          ;;3d-triangle-strip-pipeline (make-instance '3d-triangle-strip-pipeline
            ;;                                        :app app
              ;;                                      :name :3d-triangle-strip-pipeline)
          2d-point-list-pipeline (make-instance '2d-point-list-pipeline
                                                :app app
                                                :name :2d-point-list-pipeline)
          2d-line-list-pipeline (make-instance '2d-line-list-pipeline
                                               :app app
                                               :name :2d-line-list-pipeline)
          2d-line-strip-pipeline (make-instance '2d-line-strip-pipeline
                                                :app app
                                                :name :2d-line-strip-pipeline)
          2d-triangle-list-pipeline (make-instance '2d-triangle-list-pipeline
                                                   :app app
                                                   :name :2d-triangle-list-pipeline)
          ;;2d-triangle-strip-pipeline (make-instance '2d-triangle-strip-pipeline
            ;;                                        :app app
          ;;                                      :name :2d-triangle-strip-pipeline)
          )

    (values)))

(defclass krma-application-mixin ()
  ((scene :initform nil :accessor application-scene)
   (pipeline-store :accessor application-pipeline-store)
   (exit? :initform nil :accessor application-exit?)
   (current-frame-cons :initform (list 0) :accessor current-frame-cons)
   (current-draw-data-cons :initform (list 0) :accessor current-draw-data-cons)
   (retained-mode-handle-count-cons :initform (list -1) :accessor retained-mode-handle-count-cons)))

(defmethod initialize-instance :after ((instance krma-application-mixin) &rest initargs)
  (declare (ignore initargs))
  (setf (application-pipeline-store instance) (make-instance 'pipeline-store :app instance))
  (setf (application-scene instance) (make-instance (scene-class instance)))
  (values))

(defclass krma-test-application (krma-application-mixin #-darwin vulkan-application-mixin
                                                        #+darwin metal-application-mixin)
  ())

(defmethod scene-class ((application krma-test-application))
  'standard-scene)

(cffi:defcfun ("memcpy" memcpy) :pointer
  (dst :pointer)
  (src :pointer)
  (count size-t))


(defun add-2d-point (x y &key (color *default-color*))
  (scene-add-2d-point (application-scene vk:*app*) color x y))

(defun add-3d-point (x y z &key (color *default-color*))
  (scene-add-3d-point (application-scene vk:*app*) color x y z))

(defun draw-3d-point (x y z &key (color *default-color*))
  (scene-add-3d-point (application-scene vk:*app*) color x y z))

(defun add-2d-line (x0 y0 x1 y1 &key (color *default-color*))
  (scene-add-2d-line (application-scene vk:*app*) color x0 y0 x1 y1))

(defun add-3d-line (x0 y0 z0 x1 y1 z1 &key (color *default-color*))
  (scene-add-3d-line (application-scene vk:*app*) color x0 y0 z0 x1 y1 z1))

(defun add-multicolor-2d-polyline (vertices &key (closed? nil)
                                              (model-mtx *identity-matrix*)
                                              (line-thickness *default-line-thickness*))
  (scene-add-multicolor-2d-polyline-1 (application-scene vk:*app*) closed? model-mtx line-thickness vertices))

(defun draw-multicolor-2d-polyline (vertices &key (closed? nil))
  (scene-draw-multicolor-2d-polyline-1 (application-scene vk:*app*) closed? vertices))

(defun add-2d-polyline (vertices &key (closed? nil)
                                   (color *default-color*)
                                   (model-mtx *identity-matrix*)
                                   (line-thickness *default-line-thickness*))
  (scene-add-2d-polyline-1 (application-scene vk:*app*) closed? model-mtx line-thickness color vertices))

(defun add-2d-triangle (x0 y0 x1 y1 x2 y2 &key
                                            (color *default-color*)
                                            (model-mtx *identity-matrix*)
                                            (line-thickness *default-line-thickness*))
  (scene-add-2d-triangle (application-scene vk:*app*) model-mtx line-thickness color x0 y0 x1 y1 x2 y2))

(defun draw-2d-triangle (x0 y0 x1 y1 x2 y2 &key (color *default-color*))
  (scene-draw-2d-triangle (application-scene vk:*app*) color x0 y0 x1 y1 x2 y2))

(defun add-2d-rectangle (x0 y0 x1 y1
                         &key (color *default-color*)
                           (model-mtx *identity-matrix*)
                           (line-thickness *default-line-thickness*))
  (scene-add-2d-rectangle (application-scene vk:*app*) model-mtx line-thickness color x0 y0 x1 y1))

(defun draw-2d-rectangle (x0 y0 x1 y1 &key (color *default-color*))
  (scene-draw-2d-rectangle (application-scene vk:*app*) color x0 y0 x1 y1))

(defun add-2d-circular-arc (center-x center-y radius start-angle end-angle
                            &key (closed? nil)
                              (color *default-color*)
                              (model-mtx *identity-matrix*)
                              (line-thickness *default-line-thickness*)
                              (number-of-segments *default-number-of-segments*))
  (scene-add-2d-circular-arc (application-scene vk:*app*) closed? model-mtx line-thickness color
                             center-x center-y radius start-angle end-angle number-of-segments))

(defun add-2d-circle (center-x center-y radius
                      &key (color *default-color*)
                        (model-mtx *identity-matrix*)
                        (line-thickness *default-line-thickness*)
                        (number-of-segments *default-number-of-segments*))
  (scene-add-2d-circle (application-scene vk:*app*)
                       model-mtx line-thickness color
                       center-x center-y radius number-of-segments))

(defun add-multicolor-3d-polyline (vertices &key (closed? nil)
                                              (model-mtx *identity-matrix*)
                                              (line-thickness *default-line-thickness*))
  (scene-add-multicolor-3d-polyline-1 (application-scene vk:*app*) closed? model-mtx line-thickness vertices))

(defun add-3d-polyline (vertices &key (color *default-color*)
                                   (closed? nil)
                                   (model-mtx *identity-matrix*)
                                   (line-thickness *default-line-thickness*))
  (scene-add-3d-polyline-1 (application-scene vk:*app*) closed? model-mtx line-thickness color vertices))

(defun add-filled-2d-triangle-list (vertices &key (color *default-color*)
                                               (model-mtx *identity-matrix*))
  (scene-add-filled-2d-triangle-list (application-scene vk:*app*) model-mtx color vertices))

(defun add-filled-2d-rectangle-list (vertices &key (color *default-color*)
                                                (model-mtx *identity-matrix*))
  (scene-add-filled-2d-rectangle-list (application-scene vk:*app*) model-mtx color vertices))

(defun add-textured-2d-rectangle-list (vertices &key (color *default-color*)
                                                  (model-mtx *identity-matrix*)
                                                  (texture *white-texture*))
  (scene-add-textured-2d-rectangle-list (application-scene vk::*app*) model-mtx texture color vertices))

(defun add-filled-2d-polygon (vertices &key
                                         (color *default-color*)
                                         (model-mtx *identity-matrix*))
  (scene-add-filled-2d-polygon (application-scene vk:*app*) model-mtx color vertices))

(defun add-filled-3d-triangle-list (vertices &key
                                               (color *default-color*)
                                               (model-mtx *identity-matrix*))
  (scene-add-filled-3d-triangle-list (application-scene vk:*app*) model-mtx color vertices))

(defun add-filled-3d-triangle-strip (vertices &key
                                               (color *default-color*)
                                               (model-mtx *identity-matrix*))
  (scene-add-filled-3d-triangle-strip (application-scene vk:*app*) model-mtx color vertices))

(defun add-filled-3d-triangle-list-with-normals (vertices &key
                                                            (color *default-color*)
                                                            (model-mtx *identity-matrix*))
  (scene-add-filled-3d-triangle-list-with-normals (application-scene vk:*app*) model-mtx color vertices))

(defun add-filled-3d-triangle-strip-with-normals (vertices &key
                                                             (color *default-color*)
                                                             (model-mtx *identity-matrix*))
  (scene-add-filled-3d-triangle-strip-with-normals (application-scene vk:*app*) model-mtx color vertices))

(defun add-textured-3d-triangle-list (vertices &key
                                                 (color *default-color*)
                                                 (model-mtx *identity-matrix*)
                                                 (texture *white-texture*))
  (scene-add-textured-3d-triangle-list (application-scene vk:*app*) model-mtx texture color vertices))

(defun add-textured-3d-triangle-strip (vertices &key
                                                 (color *default-color*)
                                                 (model-mtx *identity-matrix*)
                                                 (texture *white-texture*))
  (scene-add-textured-3d-triangle-strip (application-scene vk:*app*) model-mtx texture color vertices))

(defmethod main ((app krma-test-application))
  (let* ((device (default-logical-device app))
         (main-window (main-window app))
         (index (queue-family-index (render-surface main-window)))
         (queue (find-queue device index))
         (command-pool (find-command-pool device index))
         (command-buffer (elt (command-buffers command-pool) 0))
         (descriptor-pool (default-descriptor-pool app))
         (sampler (create-sampler device :allocator (allocator app)))
         (texture-dsl (create-descriptor-set-layout
                       device
                       :bindings (list (make-instance 'descriptor-set-layout-binding
                                                      :type VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
                                                      :count 1
                                                      :flags VK_SHADER_STAGE_FRAGMENT_BIT
                                                      :samplers (list sampler))))))
    (setf *sampler* sampler)

    (device-wait-idle device)

    (reset-command-pool device command-pool)

    ;; one time commands here.
    (let* ((bpp 4)
           (bitmap (make-array bpp :element-type '(unsigned-byte 8) :initial-element #xff)))
      (setq *white-texture*
            (make-vulkan-texture device queue sampler texture-dsl descriptor-pool command-buffer bpp bitmap 1 1)))



    (let ((image-index)
          (work-queue)
          (current-frame-cons (current-frame-cons app))
          (current-draw-data-cons (current-draw-data-cons app)))

      (with-slots (exit?) app

        (loop while (zerop (glfwWindowShouldClose (h main-window)))
              do
                 (glfwPollEvents)

                 (when (recreate-swapchain? main-window)
                   (multiple-value-bind (width height) (get-framebuffer-size main-window)
                     (recreate-swapchain main-window (swapchain main-window) width height)
                     (setf (recreate-swapchain? main-window) nil)))

                 (let* ((swapchain (swapchain main-window))
                        (frame-count (number-of-images swapchain))
                        (current-frame (car current-frame-cons))
                        (current-draw-data (car current-draw-data-cons))
                        (scene (application-scene app))
                        (frame-resource0 (elt (frame-resources swapchain) current-frame))
                        (command-buffer (frame-command-buffer frame-resource0))
                        (draw-data (aref (rm-draw-data scene) current-draw-data)))

                   (setq work-queue (draw-data-work-queue draw-data))

                   (loop with work = nil
                         while (setq work (sb-concurrency:dequeue work-queue))
                         do (funcall work))


                   (setq image-index
                         (frame-begin swapchain (render-pass swapchain)
                                      current-frame (clear-value main-window)
                                      command-pool))

                   ;; render here.
                   (multiple-value-bind (width height) (get-framebuffer-size main-window)
                     (render-scene app command-buffer draw-data width height))


                   (frame-end swapchain queue current-frame)

                   (frame-present swapchain queue current-frame image-index main-window)

                   ;; this needs to be the only thread that modifies current-frame
                   (sb-ext:atomic-update (car current-frame-cons)
                                         #'(lambda (cf) (mod (1+ cf) frame-count)))
                   (sb-ext:atomic-update (car current-draw-data-cons)
                                         #'(lambda (cdd) (mod (1+ cdd) 2)))))

        (shutdown-application app)))))
