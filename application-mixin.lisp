(in-package :krma)

(eval-when (:compile-toplevel :load-toplevel)
  (when krma::*debug*
    (declaim (optimize (safety 3) (debug 3)))))

(defclass pipeline-store-mixin ()
  ((2d-point-list-pipeline :accessor pipeline-store-2d-point-list-pipeline)
   (2d-line-list-pipeline :accessor pipeline-store-2d-line-list-pipeline)
   (2d-line-strip-pipeline :accessor pipeline-store-2d-line-strip-pipeline)
   (2d-triangle-list-pipeline :accessor pipeline-store-2d-triangle-list-pipeline)
   #+NOMORE(msdf-text-pipeline :accessor pipeline-store-msdf-text-pipeline)
   (2d-triangle-strip-pipeline :accessor pipeline-store-2d-triangle-strip-pipeline)
   (3d-point-list-pipeline :accessor pipeline-store-3d-point-list-pipeline)
   (3d-line-list-pipeline :accessor pipeline-store-3d-line-list-pipeline)
   (3d-line-strip-pipeline :accessor pipeline-store-3d-line-strip-pipeline)
   (3d-triangle-list-pipeline :accessor pipeline-store-3d-triangle-list-pipeline)
   (3d-triangle-list-with-normals-pipeline :accessor pipeline-store-3d-triangle-list-with-normals-pipeline)
   (3d-triangle-strip-pipeline :accessor pipeline-store-3d-triangle-strip-pipeline)
   (3d-triangle-strip-with-normals-pipeline :accessor pipeline-store-3d-triangle-strip-with-normals-pipeline))
  (:documentation "Abstract superclass of a krma pipeline store. Define your own concrete type with new pipelines based on this mixin.  Pipeline store is used with pipeline combination generic functions to pair pipelines with draw lists to render in generic function `render-scene."))

(defclass standard-pipeline-store (pipeline-store-mixin)
  ()
  (:documentation "An concrete class based on pipeline-store-mixin used in krma-test-application."))

(defmethod initialize-instance :after ((instance pipeline-store-mixin) &rest initargs &key dpy)
  "Define your own method for initialize-instance for pipeline store to instantiate your own custom pipelines."
  (declare (ignore initargs))
  (with-slots (3d-point-list-pipeline
               3d-line-list-pipeline
               3d-line-strip-pipeline
               3d-triangle-list-pipeline
               3d-triangle-list-with-normals-pipeline
               3d-triangle-strip-with-normals-pipeline	       
               3d-triangle-strip-pipeline
               2d-point-list-pipeline
               2d-line-list-pipeline
               2d-line-strip-pipeline
               2d-triangle-list-pipeline
               #+NOMORE msdf-text-pipeline
               2d-triangle-strip-pipeline)
      instance

    (setf 2d-point-list-pipeline
	  (make-instance '2d-point-list-pipeline
			 :dpy dpy
			 :name :2d-point-list-pipeline)

	  2d-line-list-pipeline
	  (make-instance '2d-line-list-pipeline
			 :dpy dpy
			 :name :2d-line-list-pipeline)

	  2d-line-strip-pipeline
	  (make-instance '2d-line-strip-pipeline
			 :dpy dpy
			 :name :2d-line-strip-pipeline)

	  2d-triangle-list-pipeline
	  (make-instance '2d-triangle-list-pipeline
			 :dpy dpy
			 :name :2d-triangle-list-pipeline)

	  #+NOMORE #+NOMORE
	  msdf-text-pipeline
	  (make-instance 'msdf-text-pipeline
			 :dpy dpy
			 :name :msdf-text-pipeline)
	  
          2d-triangle-strip-pipeline
	  (make-instance '2d-triangle-strip-pipeline
			 :dpy dpy
			 :name :2d-triangle-strip-pipeline)

	  3d-point-list-pipeline
	  (make-instance '3d-point-list-pipeline
			 :dpy dpy
			 :name :3d-point-list-pipeline)
	  
	  3d-line-list-pipeline
	  (make-instance '3d-line-list-pipeline
			 :dpy dpy
			 :name :3d-line-list-pipeline)
	  
          3d-line-strip-pipeline
	  (make-instance '3d-line-strip-pipeline
			 :dpy dpy
			 :name :3d-line-strip-pipeline)
	  
          3d-triangle-list-pipeline
	  (make-instance '3d-triangle-list-pipeline
			 :dpy dpy
			 :name :3d-triangle-list-pipeline)
	  
          3d-triangle-list-with-normals-pipeline
	  (make-instance '3d-triangle-list-with-normals-pipeline
			 :dpy dpy
			 :name :3d-triangle-list-with-normals-pipeline)

	  3d-triangle-strip-pipeline
	  (make-instance '3d-triangle-strip-pipeline
			 :dpy dpy
			 :name :3d-triangle-strip-pipeline)
	  
	  3d-triangle-strip-with-normals-pipeline
	  (make-instance '3d-triangle-strip-with-normals-pipeline
			 :dpy dpy
			 :name :3d-triangle-strip-with-normals-pipeline)))
  (values))

(defclass window-frame-rate-mixin ()
  ((show-frame-rate? :initform t :accessor window-show-frame-rate?)
   (frame-rate :initform 0 :accessor window-frame-rate)
   (frames :initform 0 :accessor %window-frames)
   (delta-time :initform 0 :accessor %window-delta-time)
   (time :initform (/ (get-internal-real-time) internal-time-units-per-second) :accessor %window-time)
   (base-time :initform 0 :accessor %window-base-time)))

(defstruct viewport
  (x) ;; in framebuffer coordinates
  (y) ;; in framebuffer coordinates
  (width) ;; in framebuffer scale
  (height) ;; in framebuffer scale
  (2d-camera)
  (3d-camera)
  (scene)) ;; only one scene per viewport

(defstruct (camera
	    (:conc-name %CAMERA-))
  (proj-matrix)
  (view-matrix))

(defmethod camera-proj-matrix ((camera camera))
  (%camera-proj-matrix camera))

(defmethod camera-view-matrix ((camera camera))
  (%camera-view-matrix camera))

(defmethod (setf camera-proj-matrix) (value (camera camera))
  (setf (%camera-proj-matrix camera) value))

(defmethod (setf camera-view-matrix) (value (camera camera))
  (setf (%camera-view-matrix camera) value))

(defmethod set-camera-width-height ((camera camera) new-width new-height camera-type)
  (ecase camera-type
    (:2d
     (setf (camera-proj-matrix camera) (mortho-vulkan 0 new-width new-height 0 0 1024))
     (setf (camera-view-matrix camera) (mlookat (vec3 0 0 +select-box-2d-depth+) (vec3 0 0 0) (vec3 0 1 0))))
    (:3d
     (setf (camera-proj-matrix camera) (mperspective-vulkan 45 (/ new-width new-height)
							       0.1 3000
							       ;;*default-znear* *default-zfar*
							       ))
     (setf (camera-view-matrix camera) (mlookat (vec3 0 0 1500) (vec3 0 0 0) (vec3 0 1 0)))))
  (values))     

(defclass krma-window-mixin (window-frame-rate-mixin vk:vulkan-window-mixin)
  ((queue :accessor window-queue)
   (command-pool :accessor window-command-pool)
   (viewports :accessor window-viewports)))



(defclass krma-window (krma-window-mixin)
  ())

(defmethod initialize-instance :after ((window krma-window) &rest initargs)
  (declare (ignore initargs))
  (multiple-value-bind (width height) (window-framebuffer-size window)
    (setf (krma::window-viewports window)
	  (list (krma::make-viewport
		 :x 0 :y 0
		 :width width
		 :height height
		 :2d-camera (make-camera
			     :proj-matrix (mortho-vulkan 0 width height 0 0 +select-box-2d-depth+)
			     :view-matrix (mlookat (vec3 0 0 +select-box-2d-depth+) (vec3 0 0 0) (vec3 0 1 0)))
		 :3d-camera (make-camera
			     :proj-matrix (mperspective-vulkan
					   45 (/ width height)
					   *default-znear* *default-zfar*)
			     :view-matrix (mlookat (vec3 0 0 1500) (vec3 0 0 0) (vec3 0 1 0)))))))
  (values))

(defun update-frame-rate (window)
  (let ((app (clui::window-display window)))
    (with-slots (frame-rate frames delta-time time base-time) window
      (maybe-defer-debug (app)
	(incf frames)
	(setq time (/ (get-internal-real-time) internal-time-units-per-second))
	(setq delta-time (- time base-time))
	(when (>= delta-time 1)
	  (setf frame-rate (float (/ frames delta-time)))
	  (setq base-time time)
	  (setq frames 0))))))

(defmethod clui:handle-event :after ((window krma-window-mixin) (event clui::window-resize-event-mixin))
  (let ((main-viewport (first (window-viewports window)))
	(new-width (clui::window-resize-event-new-width event))
	(new-height (clui::window-resize-event-new-height event)))


    (unless (or (zerop new-width)
		(zerop new-height))
    
      (setf (viewport-width main-viewport) new-width
	    (viewport-height main-viewport) new-height)
      (set-camera-width-height (viewport-3d-camera main-viewport) new-width new-height :3d)
      (set-camera-width-height (viewport-2d-camera main-viewport) new-width new-height :2d)
      )
    
    (values)))

(defclass krma-application-mixin (vulkan-application-mixin)
  ((vk::application-name :initform "krma-application")
   (dpy :accessor application-display)
   (active-scenes :initform nil :accessor active-scenes)
   (main-window :initform nil :initarg :main-window :accessor main-window))
  (:documentation "Abstract superclass for top-level application objects.  Base your own top-level application class on this mixin.  Objects based on this type will be bound to krma:*app* after instantiation."))

(defmethod initialize-instance :before ((app krma-application-mixin) &rest initargs &key &allow-other-keys)
  (declare (ignorable initargs))
  (setq *app* app)
  (values))

(defmethod initialize-instance :after ((app krma-application-mixin) &rest initargs &key (display (default-display)) &allow-other-keys)
  (declare (ignorable initargs))
  (setf (application-display app) display)
  (setf (main-window app) (make-instance 'window :display display :title (vk::application-name app)))
  
  (let* ((main-window (main-window app))
	 (main-viewport (first (window-viewports main-window)))
	 (new-scene (make-instance (scene-class app) :app app :dpy display)))

    (setf (viewport-scene main-viewport) new-scene)

    (push new-scene (active-scenes app))

    (push app (display-applications (clui::window-display main-window)))

    (let ((device (default-logical-device (clui::window-display main-window))))
      (with-slots (queue command-pool) main-window
	(let ((index (queue-family-index (render-surface main-window))))
	  (setf queue (find-queue device index))
	  (setf command-pool (find-command-pool device index)))))

    (values)))

(defmethod application-default-font ((application krma-application-mixin))
  (default-system-font (clui::window-display (main-window application))))

(defmethod application-scene ((application krma-application-mixin))
  (first (active-scenes application)))
   
(defclass krma-enabled-display-mixin (vk:vulkan-enabled-display-mixin)
  ((pipeline-store :accessor krma-pipeline-store)
   (texture-sampler :accessor krma-texture-sampler)
   (applications :accessor display-applications :initform nil)
   (exit? :initform nil :accessor run-loop-exit?)
   (current-frame-cons :initform (list 0) :accessor current-frame-cons)
   (current-draw-data-cons :initform (list 0) :accessor current-draw-data-cons)
   (retained-mode-handle-count-cons :initform (list -1) :accessor retained-mode-handle-count-cons)
   (immediate-mode-work-function-1 :initform nil :accessor immediate-mode-work-function-1)
   (immediate-mode-work-function-2 :initform nil :accessor immediate-mode-work-function-2)
   (immediate-mode-work-function-3 :initform nil :accessor immediate-mode-work-function-3)
   (backtrace :initform nil :accessor system-backtrace)
   (error-msg :initform nil :accessor system-error-msg)
   (select-box-coords :initform (vec4 -1 -1 -1 -1) :accessor krma-select-box-coords)
   (last-select-box-width :initform 0 :accessor last-select-box-width)
   (last-select-box-height :initform 0 :accessor last-select-box-height)
   (select-box-size :initform -1 :accessor krma-select-box-size)
   (select-boxes-descriptor-set-layout :initform nil :accessor krma-select-boxes-descriptor-set-layout)
   (select-boxes-descriptor-sets :initform nil :accessor krma-select-boxes-descriptor-sets)
   (select-box-2d-memory-resources :initform nil :accessor krma-select-box-2d-memory-resources)
   (select-box-3d-memory-resources :initform nil :accessor krma-select-box-3d-memory-resources)
   (select-box-2d :initform nil :accessor krma-select-box-2d)
   (select-box-3d :initform nil :accessor krma-select-box-3d)
   (ubershader-per-instance-descriptor-set-layout :initform nil :accessor krma-ubershader-per-instance-descriptor-set-layout)
   (ubershader-per-instance-descriptor-set :initform nil :accessor krma-ubershader-per-instance-descriptor-set)
   (cc-semaphore :initform (bt:make-semaphore :name "compacting-complete" :count 1)
		 :accessor compacting-complete-semaphore)
   (fic-semaphore :initform (bt:make-semaphore :name "frame-iteration-complete")
		  :accessor frame-iteration-complete-semaphore)
   (font :initform nil :accessor default-system-font)
   (stock-render-pass :initform nil :accessor display-stock-render-pass)
   (hovered :initform () :accessor krma-hovered)
   (hovered-3d :initform () :accessor krma-hovered-3d))
  (:default-initargs :enable-fragment-stores-and-atomics t))

(defun most-specifically-hovered-2d (2d-select-box x y)
  (when 2d-select-box
    (loop for i from (1- +select-box-2d-depth+) downto 0
	  do (when (not (zerop (aref 2d-select-box x y i)))
	       (return (aref 2d-select-box x y i)))
	  finally (return nil))))

(defun most-specifically-hovered-2d-object (2d-select-box x y)
  (when 2d-select-box
    (let ((hovered (most-specifically-hovered-2d 2d-select-box x y)))
      (when hovered
	(let ((object (object-from-id hovered)))
	  object)))))

(defun all-hovered-2d (2d-select-box x y)
  (loop for i from 0 below +select-box-2d-depth+
	with result = ()
	unless (zerop (aref 2d-select-box x y i))
	do (push (aref 2d-select-box x y i) result)
	finally (return result)))

(defun all-hovered-3d (3d-select-box x y)
  (loop for i from 0 below +select-box-3d-depth+
	with result = ()
	unless (zerop (aref 3d-select-box x y i))
	do (push (aref 3d-select-box x y i) result)
     finally (return result)))


(defun focused-hovered-2d (2d-select-box x y)
  (first (remove-if #'(lambda (list)
			(every #'zerop list))
		    (loop for i from (1- +select-box-2d-depth+) downto (1- +view-depth+) by +view-depth+
		       collect (loop for j from i downto 0 repeat +view-depth+
				  unless (zerop (aref 2d-select-box x y j))
				  collect (aref 2d-select-box x y j))))))

#+NIL
(defun focused-hovered-2d (2d-select-box x y)
  (let ((i (loop for i from (1- +select-box-2d-depth+) downto 0
	      when (and (zerop (mod (1+ i) +view-depth+))
			(not (zerop (aref 2d-select-box x y i))))
	      do (return i)
	      finally (return 0))))
    (loop for j from i
       repeat +view-depth+
       with result = ()
       unless (zerop (aref 2d-select-box x y j))
       do (push (aref 2d-select-box x y j) result)
       finally (return (nreverse result)))))
  

(defun monitor-select-boxes (dpy)
  ;; note: could put this logic in pointer-motion-event on krma-window-mixin
  (let* ((all-hovered (all-hovered-2d (krma-select-box-2d dpy) 0 0)))
    
    (flet ((exit-event (hovered)
	     (let ((object (object-from-id hovered)))
	       (when object
		 (if (clui::window-p object)
		     (multiple-value-bind (x y) (window-cursor-position object)
		       (handle-event object (make-instance 'pointer-exit-event
							   :window object
							   :timestamp (get-internal-real-time)
							   :x x :y y)))
		     (handle-event object (make-instance 'pointer-exit-event
							 :window object
							 :timestamp (get-internal-real-time)))))))
	   (enter-event (hovered)
	     (let ((object (object-from-id hovered)))
	       (when object
		 (if (clui::window-p object)
		     (multiple-value-bind (x y) (window-cursor-position object)
		       (handle-event object (make-instance 'pointer-enter-event
							   :window object
							   :timestamp (get-internal-real-time)
							   :x x :y y)))
		     (handle-event object (make-instance 'pointer-enter-event
							 :window object
							 :timestamp (get-internal-real-time))))))))

      ;;(print (krma-select-box-2d dpy))

      (let ((difference (set-difference (krma-hovered dpy) all-hovered)))
	;;(format t "~%0: ~S" difference)
	(mapcar #'exit-event difference))

      (let ((difference (set-difference all-hovered (krma-hovered dpy))))
	;;(format t "~%1: ~S" difference)
	(mapcar #'enter-event difference))

      ;;(format t "~%2: ~S" all-hovered)
      (setf (krma-hovered dpy) all-hovered)))

  (unless (krma-hovered dpy)
  
    (let* ((all-hovered (all-hovered-3d (krma-select-box-3d dpy) 0 0)))
    
      (flet ((exit-event (hovered)
	       (let ((object (object-from-id hovered)))
		 (when object
		   (if (clui::window-p object)
		       (multiple-value-bind (x y) (window-cursor-position object)
			 (handle-event object (make-instance 'pointer-exit-event
							     :window object
							     :timestamp (get-internal-real-time)
							     :x x :y y)))
		       (handle-event object (make-instance 'pointer-exit-event
							   :window object
							   :timestamp (get-internal-real-time)))))))
	     (enter-event (hovered)
	       (let ((object (object-from-id hovered)))
		 (when object
		   (if (clui::window-p object)
		       (multiple-value-bind (x y) (window-cursor-position object)
			 (handle-event object (make-instance 'pointer-enter-event
							     :window object
							     :timestamp (get-internal-real-time)
							     :x x :y y)))
		       (handle-event object (make-instance 'pointer-enter-event
							   :window object
							   :timestamp (get-internal-real-time))))))))

	;;(print (krma-select-box-2d dpy))

	(let ((difference (set-difference (krma-hovered-3d dpy) all-hovered)))
	  ;;(format t "~%0: ~S" difference)
	  (mapcar #'exit-event difference))

	(let ((difference (set-difference all-hovered (krma-hovered-3d dpy))))
	  ;;(format t "~%1: ~S" difference)
	  (mapcar #'enter-event difference))

	;;(format t "~%2: ~S" all-hovered)
	(setf (krma-hovered-3d dpy) all-hovered)))))
	      

(defun make-select-boxes-descriptor-set-layout-bindings (dpy)
  (declare (ignore dpy)) ;; set 1
  (list (make-instance 'storage-buffer-for-fragment-shader-dsl-binding)
	(make-instance 'storage-buffer-for-fragment-shader-dsl-binding
		       :binding 1)))

(defun create-select-boxes-descriptor-set-layout (device dpy)
  (setf (krma-select-boxes-descriptor-set-layout dpy)
	(create-descriptor-set-layout device :bindings (make-select-boxes-descriptor-set-layout-bindings dpy))))

(defun make-ubershader-per-instance-descriptor-set-layout-bindings (dpy)
  ;; set 2
  (list (make-instance 'descriptor-set-layout-binding
                       :type VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
                       :count 1
                       :flags VK_SHADER_STAGE_FRAGMENT_BIT
                       :samplers (list (krma-texture-sampler dpy)))))

(defun create-ubershader-per-instance-descriptor-set-layout (device dpy)
  (setf (krma-ubershader-per-instance-descriptor-set-layout dpy)
	(create-descriptor-set-layout device :bindings (make-ubershader-per-instance-descriptor-set-layout-bindings dpy))))

(defun setup-krma (dpy &rest initargs)
  (declare (ignorable initargs))
  
  (setf (krma-texture-sampler dpy) (create-sampler (default-logical-device dpy) :allocator (allocator dpy)))
  (create-select-boxes-descriptor-set-layout (default-logical-device dpy) dpy)
  (create-ubershader-per-instance-descriptor-set-layout (default-logical-device dpy) dpy)
  (setf (krma-pipeline-store dpy) (make-instance 'standard-pipeline-store :dpy dpy))
  
  (let* ((helper-window (clui::helper-window dpy))
	 (device (default-logical-device dpy))
	 (index (queue-family-index (render-surface helper-window)))
	 (queue (find-queue device index))
	 (command-pool (find-command-pool device index))
         (command-buffer (elt (command-buffers command-pool) 0))
         (descriptor-pool (default-descriptor-pool dpy))
	 (sampler (krma-texture-sampler dpy))
         (texture-dsl (create-descriptor-set-layout
                       device
                       :bindings (list (make-instance 'descriptor-set-layout-binding
                                                      :type VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
                                                      :count 1
                                                      :flags VK_SHADER_STAGE_FRAGMENT_BIT
                                                      :samplers (list sampler))))))
    (device-wait-idle device)
    
    (reset-command-pool device command-pool)
    
    ;; one time commands here.
    (unless (probe-file (asdf/system:system-relative-pathname :krma "submodules/krma-fonts/rm16cache.json"))
      (sdf-bmfont:create-bmfont
       (asdf/system:system-relative-pathname
	:krma "submodules/krma-fonts/Roboto_Mono/static/RobotoMono-Medium.ttf")
       (asdf/system:system-relative-pathname :krma "submodules/krma-fonts/rm16cache.json")
       :size 16 :mode :msdf+a :type :json :spread 8))
    
    (uiop/filesystem:with-current-directory
	((asdf/system:system-relative-pathname :krma "submodules/krma-fonts/"))
      (setf (default-system-font dpy)
	    (vulkan-make-font
	     device queue sampler texture-dsl descriptor-pool command-buffer
	     :cache-file "rm16cache.json")))
    
    (let* ((bpp 4)
           (bitmap (make-array bpp :element-type '(unsigned-byte 8) :initial-element #xff)))
      
      (setq *white-texture*
            (make-vulkan-texture device queue sampler texture-dsl descriptor-pool command-buffer bpp bitmap 1 1)))
    
    (values)))

(defmethod initialize-instance :after ((instance krma-enabled-display-mixin) &rest initargs)
  (apply #'setup-krma instance initargs)
  (values))

(defclass krma-test-application (krma-application-mixin)
  ((vk::application-name :initform "krma-test-application"))
  (:documentation "A demo application for krma."))


(defgeneric scene-class (application)
  (:documentation "Define your own scene-class method for your custom application object to tell krma which type of scene to use in your application."))

(defmethod scene-class ((application krma-application-mixin))
  'standard-scene)

(defun add-2d-point-primitive (x y &key
				     (color *default-color*)
                                     (point-size *default-point-size*)
                                     (matrix nil)
				     (group nil)
				     (object-id 0)
				     (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, returns a handle. Calls scene-add-2d-point-primitive with color defaulting to *default-color*, point-size defaulting to *default-point-size*, matrix defaulting to nil (identity), group defaulting to nil (no group), and scene defaulting to (application-scene *app*). The required arguments x and y must be real numbers."
  (scene-add-2d-point-primitive scene group matrix point-size color x y object-id))

(defun add-2d-point (x y &key
			   (color *default-color*)
                           (point-size *default-point-size*)
                           (group :default)
			   (object-id 0)
			   (scene (application-scene *app*)))
  "Retained-mode function, returns no values. Calls scene-add-2d-point with color defaulting to *default-color*, point-size defaulting to *default-point-size*, group defaulting to :default, and scene defaulting to (application-scene *app*). The required arguments x and y must be real numbers."
  (scene-add-2d-point scene group point-size color x y object-id))

(defun draw-2d-point (x y &key
			    (color *default-color*)
                            (point-size *default-point-size*)
                            (group :default)
			    (object-id 0)
			    (scene (application-scene *app*)))
  "Immediate-mode function, returns no values. Calls scene-draw-2d-point with color defaulting to *default-color*, point-size defaulting to *default-point-size*, group defaulting to :default, and scene defaulting to (application-scene *app*). The required arguments x and y must be real numbers."
  (scene-draw-2d-point scene group point-size color x y object-id))

(defun add-3d-point-primitive (x y z &key
				       (color *default-color*)
				       (point-size *default-point-size*)
				       (matrix nil)
				       (group nil)
				       (object-id 0)
				       (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, returns a handle. Calls scene-add-3d-point-primitive with color defaulting to *default-color*, point-size defaulting to *default-point-size*, matrix defaulting to nil (identity), group defaulting to nil (no group), and scene defaulting to (application-scene *app*). The required arguments x, y and z must be real numbers."
  (scene-add-3d-point-primitive scene group matrix point-size color x y z object-id))

(defun add-3d-point (x y z &key
			     (color *default-color*)
                             (point-size *default-point-size*)
                             (group :default)
			     (object-id 0)
			     (scene (application-scene *app*)))
  "Retained-mode function, returns no values. Calls scene-add-3d-point with color defaulting to *default-color*, point-size defaulting to *default-point-size*, group defaulting to :default, and scene defaulting to (application-scene *app*). The required arguments x, y and z must be real numbers."
  (scene-add-3d-point scene group point-size color x y z object-id))

(defun draw-3d-point (x y z &key
			      (color *default-color*)
                              (point-size *default-point-size*)
                              (group :default)
			      (object-id 0)
			      (scene (application-scene *app*)))
  "Immediate-mode function, returns no values. Calls scene-draw-3d-point with color defaulting to *default-color*, point-size defaulting to *default-point-size*, group defaulting to :default, and scene defaulting to (application-scene *app*). The required arguments x, y and z must be real numbers."
  (scene-draw-3d-point scene group point-size color x y z object-id))

(defun add-2d-line-primitive (x0 y0 x1 y1 &key
					    (color *default-color*)
					    (line-thickness *default-line-thickness*)
					    (matrix nil)
					    (group nil)
					    (object-id 0)
					    (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, returns a handle.  Calls scene-add-2d-line-primitive with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, matrix defaulting to nil (identity), group defaulting to nil (no group), and scene defaulting to (application-scene *app*).  The required arguments x0, y0, x1, and y1 are the endpoints of the line and must be real numbers."
  (scene-add-2d-line-primitive scene group matrix line-thickness color x0 y0 x1 y1 object-id))

(defun add-2d-line (x0 y0 x1 y1 &key
				  (color *default-color*)
                                  (line-thickness *default-line-thickness*)
                                  (group :default)
				  (object-id 0)
				  (scene (application-scene *app*)))
  "Retained-mode function, returns no values.  Calls scene-add-2d-line with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, group defaulting to nil (no group), and scene defaulting to (application-scene *app*).  The required arguments x0, y0, x1 and y1 are the endpoints of the line and must be real numbers."
  (scene-add-2d-line scene group line-thickness color x0 y0 x1 y1 object-id))

(defun draw-2d-line (x0 y0 x1 y1 &key
				   (color *default-color*)
                                   (line-thickness *default-line-thickness*)
                                   (group :default)
				   (object-id 0)
				   (scene (application-scene *app*)))
  "Immediate-mode function, returns no values.  Calls scene-draw-2d-line with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, group defaulting to nil (no group), and scene defaulting to (application-scene *app*).  The required arguments x0, y0, x1 and y1 are the endpoints of the line and must be real numbers."
  (scene-draw-2d-line scene group line-thickness color x0 y0 x1 y1 object-id))

(defun add-3d-line-primitive (x0 y0 z0 x1 y1 z1 &key
						  (color *default-color*)
						  (line-thickness *default-line-thickness*)
						  (matrix nil)
						  (group nil)
						  (object-id 0)
						  (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, returns a handle.  Calls scene-add-3d-line-primitive with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, matrix defaulting to nil (identity), group defaulting to nil (no group), and scene defaulting to (application-scene *app*).  The required arguments x0, y0, z0, x1, y1 and z1 are the endpoints of the line and must be real numbers."
  (scene-add-3d-line-primitive scene group matrix line-thickness color x0 y0 z0 x1 y1 z1 object-id))

(defun add-3d-line (x0 y0 z0 x1 y1 z1 &key
					(color *default-color*)
                                        (line-thickness *default-line-thickness*)
                                        (group :default)
					(object-id 0)
					(scene (application-scene *app*)))
  "Retained-mode function, returns no values.  Calls scene-add-3d-line with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, group defaulting to nil (no group), and scene defaulting to (application-scene *app*).  The required arguments x0, y0, z0, x1, y1 and z1 are the endpoints of the line and must be real numbers."
  (scene-add-3d-line scene group line-thickness color x0 y0 z0 x1 y1 z1 object-id))

(defun draw-3d-line (x0 y0 z0 x1 y1 z1 &key
					 (color *default-color*)
                                         (line-thickness *default-line-thickness*)
                                         (group :default)
					 (object-id 0)
					 (scene (application-scene *app*)))
  "Immediate-mode function, returns no values.  Calls scene-draw-3d-line-primitive with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, matrix defaulting to nil (identity), group defaulting to nil (no group), and scene defaulting to (application-scene *app*).  The required arguments x0, y0, z0, x1, y1 and z1 are the endpoints of the line and must be real numbers."
  (scene-draw-3d-line scene group line-thickness color x0 y0 z0 x1 y1 z1 object-id))

(defun add-multicolor-2d-polyline-primitive (vertices &key
							(closed? nil)
                                                        (line-thickness *default-line-thickness*)
                                                        (matrix nil)
							(group nil)
							(object-id 0)
							(scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, returns a handle.  Calls scene-add-multicolor-2d-polyline-primitive with closed? defaulting to nil, line-thickness defaulting to *default-line-thickness*, matrix defaulting to nil (identity), group defaulting to nil (no group), and scene defaulting to (application-scene *app*).  The required argument vertices should be of the form (list x0 y0 color0 x1 y1 color1 ... xn yn colorn) where the x and y values must be real numbers and the color value must be a color."
  (scene-add-multicolor-2d-polyline-primitive scene group matrix closed? line-thickness vertices object-id))

(defun add-multicolor-2d-polyline (vertices &key
					      (closed? nil)
                                              (line-thickness *default-line-thickness*)
                                              (group :default)
					      (object-id 0)
					      (scene (application-scene *app*)))
  "Retained-mode function, returns no values.  Calls scene-add-multicolor-2d-polyline with closed? defaulting to nil. line-thickness defaulting to *default-line-thickness*, group defaulting to :default, and scene defaulting to (application-scene *app*).  The required argument vertices should be of the form (list x0 y0 color0 x1 y1 color1 ... xn yn colorn) where the x and y values must be real numbers and the color value must be a color."
  (scene-add-multicolor-2d-polyline scene group closed? line-thickness vertices object-id))

(defun draw-multicolor-2d-polyline (vertices &key
					       (closed? nil)
                                               (line-thickness *default-line-thickness*)
                                               (group :default)
					       (object-id 0)
					       (scene (application-scene *app*)))
  "Immediate-mode function, returns no values.  Calls scene-draw-multicolor-2d-polyline with closed? defaulting to nil. line-thickness defaulting to *default-line-thickness*, group defaulting to :default, and scene defaulting to (application-scene *app*).  The required argument vertices should be of the form (list x0 y0 color0 x1 y1 color1 ... xn yn colorn) where the x and y values must be real numbers and the color value must be a color."
  (scene-draw-multicolor-2d-polyline scene group closed? line-thickness vertices object-id))

(defun add-2d-polyline-primitive (vertices &key
					     (closed? nil)
					     (color *default-color*)
					     (line-thickness *default-line-thickness*)
					     (matrix nil)
					     (group nil)					     
					     (object-id 0)
					     (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, returns a handle.  Calls scene-add-2d-polyline-primitive with closed? defaulting to nil, color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, matrix defaulting to nil (identity), group defaulting to nil (no group), and scene defaulting to (application-scene *app*).  The required argument vertices should be of the form (list x0 y0 x1 y1 ... xn yn) where the x and y values must be real numbers."
  (scene-add-2d-polyline-primitive scene group matrix closed? line-thickness color vertices object-id))

(defun add-2d-polyline (vertices &key
				   (closed? nil)
                                   (color *default-color*)
                                   (line-thickness *default-line-thickness*)
                                   (group :default)
				   (object-id 0)
				   (scene (application-scene *app*)))
  "Retained-mode function, returns no values.  Calls scene-add-2d-polyline with closed? defaulting to nil, color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, group defaulting to :default, and scene defaulting to (application-scene *app*). The required argument vertices should be of the form (list x0 y0 x1 y1 ... xn yn) where the x and y values must be real numbers."
  (scene-add-2d-polyline scene group closed? line-thickness color vertices object-id))

(defun draw-2d-polyline (vertices &key
				    (closed? nil)
                                    (color *default-color*)
                                    (line-thickness *default-line-thickness*)
                                    (group :default)
				    (object-id 0)
				    (scene (application-scene *app*)))
  "Immediate-mode function, returns no values.  Calls scene-draw-2d-polyline with closed? defaulting to nil, color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, group defaulting to :default, and scene defaulting to (application-scene *app*). The required argument vertices should be of the form (list x0 y0 x1 y1 ... xn yn) where the x and y values must be real numbers."
  (scene-draw-2d-polyline scene group closed? line-thickness color vertices object-id))


(defun add-2d-triangle-primitive (x0 y0 x1 y1 x2 y2 &key
                                                      (color *default-color*)
                                                      (line-thickness *default-line-thickness*)
						      (matrix nil)
						      (group nil)
						      (object-id 0)
						      (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive outline of a triangle, returns a handle.  Calls scene-add-2d-triangle-primitive with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required arguments represent the vertices of the triangle and must be real numbers."
  (scene-add-2d-triangle-primitive scene group matrix line-thickness color x0 y0 x1 y1 x2 y2 object-id))

(defun add-2d-triangle (x0 y0 x1 y1 x2 y2 &key
                                            (color *default-color*)
                                            (line-thickness *default-line-thickness*)
                                            (group :default)
					    (object-id 0)
					    (scene (application-scene *app*)))
  "Retained-mode function, creates an outline of a triangle, returns no values.  Calls scene-add-2d-triangle with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required arguments represent the vertices of the triangle and must be real numbers."
  (scene-add-2d-triangle scene group line-thickness color x0 y0 x1 y1 x2 y2 object-id))


(defun draw-2d-triangle (x0 y0 x1 y1 x2 y2 &key
					     (color *default-color*)
                                             (line-thickness *default-line-thickness*)
                                             (group :default)
					     (object-id 0)
					     (scene (application-scene *app*)))
  "Immediate-mode function, creates an outline of a triangle, returns no values.  Calls scene-draw-2d-triangle with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required arguments represent the vertices of the triangle and must be real numbers."
  (scene-draw-2d-triangle scene group line-thickness color x0 y0 x1 y1 x2 y2 object-id))

(defun add-2d-rectangle-primitive (x0 y0 x1 y1
                                   &key
				     (color *default-color*)
                                     (line-thickness *default-line-thickness*)
                                     (matrix nil)
				     (group nil)
				     (object-id 0)
				     (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive outline of a rectangle, returns a handle.  Calls scene-add-2d-rectangle-primitive with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required arguments represent the top-left and bottom-right corners of the rectangle, and must be real numbers."
  (scene-add-2d-rectangle-primitive scene group matrix line-thickness color x0 y0 x1 y1 object-id))

(defun add-2d-rectangle (x0 y0 x1 y1 &key
				       (color *default-color*)
				       (line-thickness *default-line-thickness*)
				       (group :default)
				       (object-id 0)
				       (scene (application-scene *app*)))
  "Retained-mode function, creates an outline of a rectangle, returns no values.  Calls scene-add-2d-rectangle with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*,  group defaulting to :default and scene defaulting to (application-scene *app*).  The required arguments represent the top-left and bottom-right corners of the rectangle, and must be real numbers."
  (scene-add-2d-rectangle scene group line-thickness color x0 y0 x1 y1 object-id))


(defun draw-2d-rectangle (x0 y0 x1 y1 &key
					(color *default-color*)
                                        (line-thickness *default-line-thickness*)
                                        (group :default)
					(object-id 0)
					(scene (application-scene *app*)))
  "Immediate-mode function, creates an outline of a rectangle, returns no values.  Calls scene-draw-2d-rectangle with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*,  group defaulting to :default and scene defaulting to (application-scene *app*).  The required arguments represent the top-left and bottom-right corners of the rectangle, and must be real numbers."
  (scene-draw-2d-rectangle scene group line-thickness color x0 y0 x1 y1 object-id))

(defun add-2d-circular-arc-primitive (center-x center-y radius start-angle end-angle
                                      &key
					(closed? nil)
                                        (color *default-color*)
                                        (line-thickness *default-line-thickness*)
                                        (number-of-segments *default-number-of-segments*)
                                        (matrix nil)
					(group nil)
					(object-id 0)
					(scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, a polyline representing the arc of a circle, returns a handle.  Calls scene-add-2d-circular-arc-primitive with closed? defaulting to nil, color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, number-of-segments defaulting to 64, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required arguments should be real numbers and start-angle and end-angle are in radians."
  (scene-add-2d-circular-arc-primitive scene group matrix closed? line-thickness color
                                       center-x center-y radius start-angle end-angle
				       number-of-segments object-id))

(defun add-2d-circular-arc (center-x center-y radius start-angle end-angle
                            &key
			      (closed? nil)
                              (color *default-color*)
                              (line-thickness *default-line-thickness*)
                              (number-of-segments *default-number-of-segments*)
                              (group :default)
			      (object-id 0)
			      (scene (application-scene *app*)))
  "Retained-mode function, creates a polyline representing the arc of a circle, returns no values.  Calls scene-add-2d-circular-arc with closed? defaulting to nil, color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, number-of-segments defaulting to 64, group defaulting to :default and scene defaulting to (application-scene *app*).  The required arguments should be real numbers and start-angle and end-angle are in radians."
  (scene-add-2d-circular-arc scene group closed? line-thickness color
                             center-x center-y radius start-angle end-angle
			     number-of-segments object-id))

(defun draw-2d-circular-arc (center-x center-y radius start-angle end-angle
                             &key
			       (closed? nil)
                               (color *default-color*)
                               (line-thickness *default-line-thickness*)
                               (number-of-segments *default-number-of-segments*)
                               (group :default)
			       (object-id 0)
			       (scene (application-scene *app*)))
  "Immediate-mode function, creates a polyline representing the arc of a circle, returns no values.  Calls scene-add-2d-circular-arc with closed? defaulting to nil, color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, number-of-segments defaulting to 64, group defaulting to :default and scene defaulting to (application-scene *app*).  The required arguments should be real numbers and start-angle and end-angle are in radians."
  (scene-draw-2d-circular-arc scene group closed? line-thickness color
                              center-x center-y radius start-angle end-angle
			      number-of-segments object-id))


(defun add-2d-circle-primitive (center-x center-y radius
                                &key
				  (color *default-color*)
                                  (line-thickness *default-line-thickness*)
                                  (number-of-segments *default-number-of-segments*)
                                  (matrix nil)
				  (group nil)
				  (object-id 0)
				  (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, a polyline representing the outline of a circle, returns a handle.  Calls scene-add-2d-circle-primitive with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, number-of-segments defaulting to 64, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required arguments should be real numbers."
  (scene-add-2d-circle-primitive scene
                                 group matrix line-thickness color
                                 center-x center-y radius number-of-segments object-id))

(defun add-2d-circle (center-x center-y radius
		      &key
			(color *default-color*)
			(line-thickness *default-line-thickness*)
			(number-of-segments *default-number-of-segments*)
                        (group :default)
			(object-id 0)
			(scene (application-scene *app*)))
  "Retained-mode function, creates a  polyline representing the outline of a circle, returns a no values.  Calls scene-add-2d-circle with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, number-of-segments defaulting to 64,  group defaulting to :default and scene defaulting to (application-scene *app*).  The required arguments should be real numbers."
  (scene-add-2d-circle scene
		       group line-thickness color
		       center-x center-y radius number-of-segments object-id))

(defun draw-2d-circle (center-x center-y radius
		       &key
			 (color *default-color*)
			 (line-thickness *default-line-thickness*)
			 (number-of-segments *default-number-of-segments*)
                         (group :default)
			 (object-id 0)
			 (scene (application-scene *app*)))
  "Immediate-mode function, creates a  polyline representing the outline of a circle, returns a no values.  Calls scene-draw-2d-circle with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, number-of-segments defaulting to 64,  group defaulting to :default and scene defaulting to (application-scene *app*).  The required arguments should be real numbers."
  (scene-draw-2d-circle scene
			group line-thickness color
			center-x center-y radius number-of-segments object-id))

(defun add-filled-2d-circle-primitive (center-x center-y radius
                                       &key
					 (color *default-color*)
                                         (number-of-sectors *default-number-of-segments*)
					 (matrix nil)
					 (group nil)
					 (object-id 0)
					 (layer 0)
					 (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, a filled 2d circle, returns a handle.  Calls scene-add-filled-2d-circle-primitive with color defaulting to *default-color*, number-of-sectors defaulting to 64, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required arguments should be real numbers."
  (scene-add-filled-2d-circle-primitive scene
					group matrix color
					center-x center-y radius
					number-of-sectors object-id layer))

(defun add-filled-2d-circle (center-x center-y radius
                             &key
			       (color *default-color*)
                               (number-of-sectors *default-number-of-segments*)
                               (group :default)
			       (object-id 0)
			       (layer 0)
			       (scene (application-scene *app*)))
  "Retained-mode function, creates  a filled 2d circle, returns no values.  Calls scene-add-filled-2d-circle with color defaulting to *default-color*, number-of-sectors defaulting to 64,  group defaulting to :default and scene defaulting to (application-scene *app*).  The required arguments should be real numbers."
  (scene-add-filled-2d-circle scene group color
			      center-x center-y radius
			      number-of-sectors object-id layer))

(defun draw-filled-2d-circle (center-x center-y radius
                              &key
				(color *default-color*)
                                (number-of-sectors *default-number-of-segments*)
                                (group :default)
				(object-id 0)
				(layer 0)
				(scene (application-scene *app*)))
  "Immediate-mode function, creates  a filled 2d circle, returns no values.  Calls scene-draw-filled-2d-circle with color defaulting to *default-color*, number-of-sectors defaulting to 64,  group defaulting to :default and scene defaulting to (application-scene *app*).  The required arguments should be real numbers."
  (scene-draw-filled-2d-circle scene group color center-x center-y radius number-of-sectors object-id layer))


(defun add-multicolor-3d-polyline-primitive (vertices &key
							(closed? nil)
                                                        (line-thickness *default-line-thickness*)
							(matrix nil)
							(group nil)
							(object-id 0)
							(scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, a multicolored 3d polyline, returns a handle.  Calls scene-add-multicolored-3d-polyline-primitive with closed? defaulting to nil, line-thickness defaulting to *default-line-thickness*, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 z0 color0 x1 y1 z1 color1 ... xn yn zn colorn) where the x y and z values should be real numbers and the color values should represent a color."
  (scene-add-multicolor-3d-polyline-primitive scene group matrix closed? line-thickness vertices object-id))

(defun add-multicolor-3d-polyline (vertices &key
					      (closed? nil)
                                              (line-thickness *default-line-thickness*)
                                              (group :default)
					      (object-id 0)
					      (scene (application-scene *app*)))
  "Retained-mode function, creates a multicolored 3d polyline, returns a no values.  Calls scene-add-multicolored-3d-polyline with closed? defaulting to nil, line-thickness defaulting to *default-line-thickness*,  group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 z0 color0 x1 y1 z1 color1 ... xn yn zn colorn) where the x y and z values should be real numbers and the color values should represent a color."
  (scene-add-multicolor-3d-polyline scene group closed? line-thickness vertices object-id))

(defun draw-multicolor-3d-polyline (vertices &key
					       (closed? nil)
                                               (line-thickness *default-line-thickness*)
                                               (group :default)
					       (object-id 0)
					       (scene (application-scene *app*)))
  "Immediate-mode function, creates a multicolored 3d polyline, returns a no values.  Calls scene-draw-multicolored-3d-polyline with closed? defaulting to nil, line-thickness defaulting to *default-line-thickness*,  group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 z0 color0 x1 y1 z1 color1 ... xn yn zn colorn) where the x y and z values should be real numbers and the color values should represent a color."
  (scene-draw-multicolor-3d-polyline scene group closed? line-thickness vertices object-id))


(defun add-3d-polyline-primitive (vertices &key
					     (color *default-color*)
                                             (closed? nil)
                                             (line-thickness *default-line-thickness*)
					     (matrix nil)
					     (group nil)
					     (object-id 0)
					     (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, a 3d polyline, returns a handle.  Calls scene-add-3d-polyline-primitive with color defaulting to *default-color*, closed? defaulting to nil, line-thickness defaulting to *default-line-thickness*, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn) where the x y and z values should be real numbers."
  (scene-add-3d-polyline-primitive scene group matrix closed? line-thickness color vertices object-id))

(defun add-3d-polyline (vertices &key
				   (color *default-color*)
                                   (closed? nil)
                                   (line-thickness *default-line-thickness*)
                                   (group :default)
				   (object-id 0)
				   (scene (application-scene *app*)))
  "Retained-mode function, creates a 3d polyline, returns a no values.  Calls scene-add-3d-polyline with color defaulting to *default-color*, closed? defaulting to nil, line-thickness defaulting to *default-line-thickness*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn) where the x y and z values should be real numbers."
  (scene-add-3d-polyline scene group closed? line-thickness color vertices object-id))

(defun draw-3d-polyline (vertices &key
				    (color *default-color*)
                                    (closed? nil)
                                    (line-thickness *default-line-thickness*)
                                    (group :default)
				    (object-id 0)
				    (scene (application-scene *app*)))
  "Immediate-mode function, creates a 3d polyline, returns a no values.  Calls scene-draw-3d-polyline with color defaulting to *default-color*, closed? defaulting to nil, line-thickness defaulting to *default-line-thickness*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn) where the x y and z values should be real numbers."
  (scene-draw-3d-polyline scene group closed? line-thickness color vertices object-id))


(defun add-filled-2d-triangle-list-primitive (vertices &key
							 (color *default-color*)
                                                         (matrix nil)
							 (group nil)
							 (object-id 0)
							 (layer 0)
							 (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, a series of filled 2d triangles, returns a handle. Calls scene-add-filled-2d-triangle-list-primitive with color defaulting to *default-color*, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 x10 y10 x20 y20 x01 y01 x11 y11 x21 y21 ... x0n y0n x1n y1n x2n y2n) where the x and y values represent vertices of a triangle in a series of triangles."
  (scene-add-filled-2d-triangle-list-primitive scene group matrix color vertices object-id layer))

(defun add-filled-2d-triangle-list (vertices &key
					       (color *default-color*)
					       (group :default)
					       (object-id 0)
					       (layer 0)
					       (scene (application-scene *app*)))
  "Retained-mode function, creates a series of filled 2d triangles, returns a no values.  Calls scene-add-filled-2d-triangle-list with color defaulting to *default-color*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 x10 y10 x20 y20 x01 y01 x11 y11 x21 y21 ... x0n y0n x1n y1n x2n y2n) where the x and y values represent vertices of a triangle in a series of triangles."
  (scene-add-filled-2d-triangle-list scene group color vertices object-id layer))

(defun draw-filled-2d-triangle-list (vertices &key
						(color *default-color*)
                                                (group :default)
						(object-id 0)
						(layer 0)
						(scene (application-scene *app*)))
  "Immediate-mode function, creates a series of filled 2d triangles, returns a no values.  Calls scene-draw-2d-triangle-list with color defaulting to *default-color*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00  x10 y10 x20 y20 x01 y01 x11 y11 x21 y21 ... x0n y0n x1n y1 x2n y2n) where the x and y values represent vertices of a triangle in a series of triangles."
  (scene-draw-filled-2d-triangle-list scene group color vertices object-id layer))


(defun add-filled-2d-rectangle-list-primitive (vertices &key
							  (color *default-color*)
                                                          (matrix nil)
							  (group nil)
							  (object-id 0)
							  (layer 0)
							  (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, a series of filled 2d rectangles, returns a handle.  Calls scene-add-2d-rectangle-list-primitive with color defaulting to *default-color*, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 x10 y10 x01 y01 x11 y11 ... x0n y0n x1n y1n) where the x0's, and y0's and the x1's and y1's represent the top-left and bottom-right of a series of rectangles."
  (scene-add-filled-2d-rectangle-list-primitive scene group matrix color vertices object-id layer))

(defun add-filled-2d-rectangle-list (vertices &key
						(color *default-color*)
						(group :default)
						(object-id 0)
						(layer 0)
						(scene (application-scene *app*)))
  "Retained-mode function a series of filled 2d rectangles, returns no values.  Calls scene-add-2d-rectangle-list with color defaulting to *default-color*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 x10 y10 x01 y01 x11 y11 ... x0n y0n x1n y1n) where the x0's, and y0's and the x1's and y1's represent the top-left and bottom-right of a series of rectangles."
  (scene-add-filled-2d-rectangle-list scene group color vertices object-id layer))

(defun draw-filled-2d-rectangle-list (vertices &key
						 (color *default-color*)
						 (group :default)
						 (object-id 0)
						 (layer 0)
						 (scene (application-scene *app*)))
  "Immediate-mode function a series of filled 2d rectangles, returns no values.  Calls scene-draw-2d-rectangle-list with color defaulting to *default-color*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 x10 y10 x01 y01 x11 y11 ... x0n y0n x1n y1n) where the x0's, and y0's and the x1's and y1's represent the top-left and bottom-right of a series of rectangles."
  (scene-draw-filled-2d-rectangle-list scene group color vertices object-id layer))


(defun add-textured-2d-rectangle-list-primitive (vertices &key
							    (color *default-color*)
                                                            (texture *white-texture*)
							    (matrix nil)
							    (group nil)
							    (object-id 0)
							    (layer 0)
							    (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, a series of textured 2d rectangles, returns a handle.  Calls scene-add-textured-2d-rectangle-list-primitive with color defaulting to *default-color*, texture defaulting to *white-texture*, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 u00 v00 x10 y10 u10 v10 x01 y01 u01 v01 x11 y11 u11 v11 ... x0n y0n u0n v0n x1n y1n u1n v1n) where the x0's, and y0's and the x1's and y1's represent the top-left and bottom-right of a series of rectangles and the u0's and v0's are the normalized texture coordinates for the top-left corner and the u1's and v1's are the normalized texture coordinates for the bottom-right corner of each rectangle."
  (scene-add-textured-2d-rectangle-list-primitive scene group matrix texture color vertices object-id layer))

(defun add-textured-2d-rectangle-list (vertices &key
						  (color *default-color*)
                                                  (texture *white-texture*)
                                                  (group :default)
						  (object-id 0)
						  (layer 0)
						  (scene (application-scene *app*)))
  "Retained-mode function, creates  a series of textured 2d rectangles, returns no values.  Calls scene-add-textured-2d-rectangle-list with color defaulting to *default-color*, texture defaulting to *white-texture*, group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 u00 v00 x10 y10 u10 v10 x01 y01 u01 v01 x11 y11 u11 v11 ... x0n y0n u0n v0n x1n y1n u1n v1n) where the x0's, and y0's and the x1's and y1's represent the top-left and bottom-right of a series of rectangles and the u0's and v0's are the normalized texture coordinates for the top-left corner and the u1's and v1's are the normalized texture coordinates for the bottom-right corner. of each rectangle."
  (scene-add-textured-2d-rectangle-list scene group texture color vertices object-id layer))

(defun draw-textured-2d-rectangle-list (vertices &key
						   (color *default-color*)
                                                   (texture *white-texture*)
                                                   (group :default)
						   (object-id 0)
						   (layer 0)
						   (scene (application-scene *app*)))
  "Immediate-mode function, creates  a series of textured 2d rectangles, returns no values.  Calls scene-draw-textured-2d-rectangle-list with color defaulting to *default-color*, texture defaulting to *white-texture*, group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 u00 v00 x10 y10 u10 v10 x01 y01 u01 v01 x11 y11 u11 v11 ... x0n y0n u0n v0n x1n y1n u1n v1n) where the x0's, and y0's and the x1's and y1's represent the top-left and bottom-right of a series of rectangles and the u0's and v0's are the normalized texture coordinates for the top-left corner and the u1's and v1's are the normalized texture coordinates for the bottom-right corner. of each rectangle."
  (scene-draw-textured-2d-rectangle-list scene group texture color vertices object-id layer))

(defun add-filled-2d-convex-polygon-primitive (vertices &key
							  (color *default-color*)
							  (matrix nil)
							  (group nil)
							  (object-id 0)
							  (layer 0)
							  (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, a filled 2d convex polygon, returns a handle.  Calls scene-add-filled-2d-convex-polygon-primitive with color defaulting to *default-color*, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 x1 y1 ... xn yn) where the x's, and y's represent a vertex of the polygon."
  (scene-add-filled-2d-convex-polygon-primitive scene group matrix color vertices object-id layer))

(defun add-filled-2d-convex-polygon (vertices &key
						(color *default-color*)
                                                (group :default)
						(object-id 0)
						(layer 0)
						(scene (application-scene *app*)))
  "Retained-mode function, creates a filled 2d convex polygon, returns a no values.  Calls scene-add-filled-2d-convex-polygon with color defaulting to *default-color*, group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 x1 y1 ... xn yn) where the x's, and y's represent a vertex of the polygon."
  (scene-add-filled-2d-convex-polygon scene group color vertices object-id layer))

(defun draw-filled-2d-convex-polygon (vertices &key
						 (color *default-color*)
                                                 (group :default)
						 (object-id 0)
						 (layer 0)
						 (scene (application-scene *app*)))
  "Immediate-mode function, creates a filled 2d convex polygon, returns a no values.  Calls scene-draw-filled-2d-convex-polygon with color defaulting to *default-color*, group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 x1 y1 ... xn yn) where the x's, and y's represent a vertex of the polygon."
  (scene-draw-filled-2d-convex-polygon scene group color vertices object-id layer))

(defun add-filled-3d-triangle-list-primitive (vertices &key
                                                         (color *default-color*)
							 (shading-style :diffuse)
							 (light-position nil)
                                                         (matrix nil)
							 (group nil)
							 (object-id 0)
							 (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, a filled 3d triangle list, returns a handle.  Calls scene-add-filled-3d-triangle-list-primitive-flat or scene-add-filled-3d-triangle-list-primitive-diffuse depending on whether shading-style is :diffuse or :flat, with color defaulting to *default-color*, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 z00 x10 y10 z10 x20 y20 z20 x01 y01 z01 x11 y11 z11 x21 y21 z21... x0n y0n z0n x1n y1n z1n x2n y2n z2n) where the x, y and z values represent vertices of a triangle in a series of triangles.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is up."
  (ecase shading-style
    (:flat (scene-add-filled-3d-triangle-list-primitive-flat scene group matrix color vertices object-id))
    (:diffuse (scene-add-filled-3d-triangle-list-primitive-diffuse scene group matrix color vertices light-position object-id))))

(defun add-filled-3d-triangle-list (vertices &key
					       (color *default-color*)
                                               (shading-style :diffuse)
                                               (group :default)
					       (object-id 0)
					       (scene (application-scene *app*)))
  "Retained-mode function, creates a filled 3d triangle list, returns a no-values.  Calls scene-add-filled-3d-triangle-list-flat or scene-add-filled-3d-triangle-list-diffuse depending on whether shading-style is :diffuse or :flat, with color defaulting to *default-color*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 z00 x10 y10 z10 x20 y20 z20 x01 y01 z01 x11 y11 z11 x21 y21 z21... x0n y0n z0n x1n y1n z1n x2n y2n z2n) where the x, y and z values represent vertices of a triangle in a series of triangles.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is up."
  (ecase shading-style
    (:flat (scene-add-filled-3d-triangle-list-flat scene group color vertices object-id))
    (:diffuse (scene-add-filled-3d-triangle-list-diffuse scene group color vertices object-id))))

(defun draw-filled-3d-triangle-list (vertices &key
                                                (color *default-color*)
						(shading-style :diffuse)
                                                (group :default)
						(object-id 0)
						(scene (application-scene *app*)))
  "Immediate-mode function, creates a filled 3d triangle list, returns a no-values.  Calls scene-draw-filled-3d-triangle-list-flat or scene-draw-filled-3d-triangle-list-diffuse depending on whether shading-style is :diffuse or :flat, with color defaulting to *default-color*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 z00 x10 y10 z10 x20 y20 z20 x01 y01 z01 x11 y11 z11 x21 y21 z21... x0n y0n z0n x1n y1n z1n x2n y2n z2n) where the x, y and z values represent vertices of a triangle in a series of triangles.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is up."
  (ecase shading-style
    (:flat (scene-draw-filled-3d-triangle-list-flat scene group color vertices object-id))
    (:diffuse (scene-draw-filled-3d-triangle-list-diffuse scene group color vertices object-id))))

(defun add-filled-3d-triangle-strip-primitive (vertices &key
							  (color *default-color*)
							  (shading-style :diffuse)
							  (light-position nil)
							  (matrix nil)
							  (group nil)
							  (object-id 0)
							  (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, a filled 3d triangle strip, returns a handle.  Calls scene-add-filled-3d-triangle-strip-primitive-flat or scene-add-filled-3d-triangle-strip-primitive-diffuse depending on whether shading-style is :diffuse or :flat, with color defaulting to *default-color*, group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 z0 x1 y1 z1 x2 y2 z2 ... xn yn zn) where the x, y and z values represent vertices of a triangle in a strip of triangles."
  (ecase shading-style
    (:flat (scene-add-filled-3d-triangle-strip-primitive-flat scene group matrix color vertices object-id))
    (:diffuse (scene-add-filled-3d-triangle-strip-primitive-diffuse scene group matrix color vertices
								    light-position object-id))))

(defun draw-filled-3d-triangle-strip (vertices &key
                                                 (color *default-color*)
                                                 (shading-style :diffuse)
                                                 (group :default)
						 (object-id 0)
						 (scene (application-scene *app*)))
  "Immediate-mode function, creates a filled 3d triangle strip, returns a no values.  Calls scene-draw-filled-3d-triangle-strip-flat or scene-draw-filled-3d-triangle-strip-diffuse depending on whether shading-style is :diffuse or :flat, with color defaulting to *default-color*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 z0 x1 y1 z1 x2 y2 z2 ... xn yn zn) where the x, y and z values represent vertices of a triangle in a strip of triangles."
  (ecase shading-style
    (:flat (scene-draw-filled-3d-triangle-strip-flat scene group color vertices object-id))
    (:diffuse (scene-draw-filled-3d-triangle-strip-diffuse scene group color vertices object-id))))


(defun add-textured-3d-triangle-list-primitive (vertices &key
							   (color *default-color*)
                                                           (texture *white-texture*)
                                                           (shading-style :diffuse)
                                                           (light-position nil)
                                                           (matrix nil)
							   (group nil)
							   (object-id 0)
							   (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, a textured 3d triangle list, returns a handle.  Calls scene-add-textured-3d-triangle-list-primitive-flat when shading-style is :flat, currently errors with shading-style :diffuse, with color defaulting to *default-color*, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 z00 u00 v00 x10 y10 z10 u10 v10 x20 y20 z20 u20 v20 x01 y01 z01 u01 v01 x11 y11 z11 x21 u11 v11 y21 z21 u21 v21... x0n y0n z0n u0n v0n x1n y1n z1n u1n v1n x2n y2n z2n u2n v2n) where the x, y and z values represent vertices of a triangle in a series of triangles and the u's and v's represent the normalized texture coordinates at that vertex.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is up."
  (declare (ignore light-position))
  (ecase shading-style
    (:flat (scene-add-textured-3d-triangle-list-primitive-flat scene group matrix texture color vertices object-id))))

(defun add-textured-3d-triangle-list (vertices &key
						 (color *default-color*)
                                                 (texture *white-texture*)
                                                 (shading-style :diffuse)
                                                 (group :default)
						 (object-id 0)
						 (scene (application-scene *app*)))
  "Retained-mode function, creates a textured 3d triangle list, returns no-values.  Calls scene-add-textured-3d-triangle-list-flat when shading-style is :flat, currently errors with shading-style :diffuse, with color defaulting to *default-color*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 z00 u00 v00 x10 y10 z10 u10 v10 x20 y20 z20 u20 v20 x01 y01 z01 u01 v01 x11 y11 z11 x21 u11 v11 y21 z21 u21 v21... x0n y0n z0n u0n v0n x1n y1n z1n u1n v1n x2n y2n z2n u2n v2n) where the x, y and z values represent vertices of a triangle in a series of triangles and the u's and v's represent the normalized texture coordinates at that vertex.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is up."
  (ecase shading-style
    (:flat (scene-add-textured-3d-triangle-list-flat
            scene group texture color vertices object-id))))

(defun draw-textured-3d-triangle-list (vertices &key
						  (color *default-color*)
                                                  (texture *white-texture*)
                                                  (shading-style :diffuse)
                                                  (group :default)
						  (object-id 0)
						  (scene (application-scene *app*)))
  "Immediate-mode function, creates a textured 3d triangle list, returns no-values.  Calls scene-draw-textured-3d-triangle-list-flat when shading-style is :flat, currently errors with shading-style :diffuse, with color defaulting to *default-color*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 z00 u00 v00 x10 y10 z10 u10 v10 x20 y20 z20 u20 v20 x01 y01 z01 u01 v01 x11 y11 z11 x21 u11 v11 y21 z21 u21 v21... x0n y0n z0n u0n v0n x1n y1n z1n u1n v1n x2n y2n z2n u2n v2n) where the x, y and z values represent vertices of a triangle in a series of triangles and the u's and v's represent the normalized texture coordinates at that vertex.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is up."
  (ecase shading-style
    (:flat (scene-draw-textured-3d-triangle-list-flat scene group texture color vertices object-id))))

(defun add-textured-3d-triangle-strip-primitive (vertices &key
                                                            (color *default-color*)
							    (texture *white-texture*)
							    (shading-style :diffuse)
							    (light-position nil)
							    (matrix nil)
							    (group nil)
							    (object-id 0)
							    (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, a textured 3d triangle strip, returns a handle.  Calls scene-add-textured-3d-triangle-strip-primitive-flat when shading-style is :flat, currently errors with shading-style :diffuse, with color defaulting to *default-color*, group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 z0 u0 v0 x1 y1 z1u1 v1  x2 y2 z2 u2 v2... xn yn zn un vn) where the x, y and z values represent successive vertices of a triangle in a strip of triangles, and the u and v values represent the normalized texture coordinates at the corresponding x, y and z.  vertices must contain at least three vertices."
  (declare (ignore light-position))
  (ecase shading-style
    (:flat (scene-add-textured-3d-triangle-strip-primitive-flat scene group matrix texture color vertices object-id))))

(defun draw-textured-3d-triangle-strip (vertices &key
                                                   (color *default-color*)
                                                   (texture *white-texture*)
                                                   (shading-style :diffuse)
                                                   (group :default)
						   (object-id 0)
						   (scene (application-scene *app*)))
  "Immediate-mode function, creates a textured 3d triangle strip, returns a no values.  Calls scene-draw-textured-3d-triangle-strip-primitive-flat when shading-style is :flat, currently errors with shading-style :diffuse, with color defaulting to *default-color*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 z0 u0 v0 x1 y1 z1u1 v1  x2 y2 z2 u2 v2... xn yn zn un vn) where the x, y and z values represent successive vertices of a triangle in a strip of triangles, and the u and v values represent the normalized texture coordinates at the corresponding x, y and z.  vertices must contain at least three vertices."
  (ecase shading-style
    (:flat (scene-draw-textured-3d-triangle-strip-flat scene group texture color vertices object-id))))

(defun add-filled-3d-convex-polygon-primitive (vertices &key
							  (color *default-color*)
							  (shading-style :diffuse)
							  (light-position nil)
							  (matrix nil)
							  (group nil)
							  (object-id 0)
							  (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, a filled 3d convex polygon, returns a handle.  Calls scene-add-filled-3d-convex-polygon-primitive-diffuse or scene-draw-filled-3d-convex-polygon-flat, depending on whether shading style is :diffuse or :flat, light-position defaults to nil, color defaulting to *default-color*, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn) where the x, y and z's represent a vertex of the polygon."
  (ecase shading-style
    (:diffuse (scene-add-filled-3d-convex-polygon-primitive-diffuse scene group matrix color vertices light-position object-id))
    (:flat (scene-add-filled-3d-convex-polygon-primitive-flat scene group matrix color vertices object-id))))

(defun add-filled-3d-convex-polygon (vertices &key
						(color *default-color*)
						(shading-style :diffuse)
                                                (group :default)
						(object-id 0)
						(scene (application-scene *app*)))
  "Retained-mode function, creates a filled 3d convex polygon, returns no values.  Calls scene-add-filled-3d-convex-polygon-diffuse or scene-add-filled-3d-convex-polygon-flat, depending on whether shading-style is :diffuse or :flat with color defaulting to *default-color*, group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 x1 y1 ... xn yn) where the x's, and y's represent a vertex of the polygon."
  (ecase shading-style
    (:diffuse (scene-add-filled-3d-convex-polygon-diffuse scene group color vertices object-id))
    (:flat (scene-add-filled-3d-convex-polygon-flat scene group color vertices object-id))))

(defun draw-filled-3d-convex-polygon (vertices &key
						 (color *default-color*)
						 (shading-style :diffuse)
                                                 (group :default)
						 (object-id 0)
						 (scene (application-scene *app*)))
  "Immediate-mode function, creates a filled 3d convex polygon, returns no values.  Calls scene-draw-filled-3d-convex-polygon-diffuse or scene-draw-filled-3d-convex-polygon-flat, depending on whether shading-style is :diffuse or :flat, with color defaulting to *default-color*, group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 x1 y1 ... xn yn) where the x's, and y's represent a vertex of the polygon."
  (ecase shading-style
    (:diffuse (scene-draw-filled-3d-convex-polygon-diffuse scene group color vertices object-id))
    (:flat (scene-draw-filled-3d-convex-polygon-flat scene group color vertices object-id))))

(defun add-filled-sphere-primitive (origin-x origin-y origin-z radius &key
									(color *default-color*)
									(resolution 64)
									(shading-style :diffuse)
									(light-position nil)
									(matrix nil)
									(group nil)
									(object-id 0)
									(scene (application-scene *app*)))
  "Retained-mode function, creates a primitive of a filled sphere, returns a handle.  Calls scene-add-filled-sphere-primitive-diffuse when shading style is :diffuse, currently errors with any other shading style, with color defaulting to *default-color*, resolution defaulting to 64, light-position defaulting to nil, matrix defaulting to nil (identity), group defaulting to nil (no group), and scene defaulting to (application-scene *app*).  The required arguments should be real numbers. radius should be positive."
  (ecase shading-style
    (:diffuse (scene-add-filled-sphere-primitive-diffuse scene
							 group matrix color
							 origin-x origin-y origin-z radius
							 light-position resolution object-id))))

(defun add-filled-sphere (origin-x origin-y origin-z radius &key
							      (color *default-color*)
                                                              (resolution 64)
                                                              (shading-style :diffuse)
                                                              (group :default)
							      (object-id 0)
							      (scene (application-scene *app*)))
  "Retained-mode function, creates a filled sphere, returns a no values.  Calls scene-add-filled-sphere-diffuse when shading style is :diffuse, currently errors with any other shading style, with color defaulting to *default-color*, resolution defaulting to 64, group defaulting to :default, and scene defaulting to (application-scene *app*).  The required arguments should be real numbers.   radius should be positive."
  (ecase shading-style
    (:diffuse (scene-add-filled-sphere-diffuse scene
					       group color
					       origin-x origin-y origin-z radius resolution object-id))))

(defun draw-filled-sphere (origin-x origin-y origin-z radius &key
							       (color *default-color*)
                                                               (resolution 64)
                                                               (shading-style :diffuse)
                                                               (group :default)
							       (object-id 0)
							       (scene (application-scene *app*)))
  "Immediate-mode function, creates a filled sphere, returns a no values.  Calls scene-draw-filled-sphere-diffuse when shading style is :diffuse, currently errors with any other shading style, with color defaulting to *default-color*, resolution defaulting to 64, group defaulting to :default, and scene defaulting to (application-scene *app*).  The required arguments should be real numbers.   radius should be positive."
  (ecase shading-style
    (:diffuse (scene-draw-filled-sphere-diffuse scene
                                                group
						color
						origin-x origin-y origin-z radius
						resolution object-id))))




(defun add-text-primitive (string pos-x pos-y &key
						(color *default-color*)
						(font (application-default-font *app*))
						(matrix nil)
						(group nil)
						(object-id 0)
						(layer 0)
						(scene (application-scene *app*)))
  "Retained-mode function, creates a primitive of a text string, returns a handle.  Calls scene-add-text-primitive with color defaulting to *default-color*, font defaulting to (application-default-font *app*), matrix defaulting to nil (identity), group defaulting to nil (no group), and scene defaulting to (application-scene *app*).  The required arguments should be real numbers.  pos-x and pos-y represent the upper left corner of the text."
  (scene-add-text-primitive scene group matrix font color pos-x pos-y string object-id layer))

(defun add-text (string pos-x pos-y &key
				      (color *default-color*)
                                      (font (application-default-font *app*))
                                      (group :default)
				      (object-id 0)
				      (layer 0)
				      (scene (application-scene *app*)))
  "Retained-mode function, creates text, returns a no values.  Calls scene-add-text with color defaulting to *default-color*, font defaulting to (application-default-font *app*), group defaulting to :default, and scene defaulting to (application-scene *app*).  The required arguments should be real numbers.  pos-x and pos-y represent the upper left corner of the text."
  (scene-add-text scene group font color pos-x pos-y string object-id layer))

(defun draw-text (string pos-x pos-y &key (color *default-color*)
                                       (font (application-default-font *app*))
                                       (group :default)
				       (object-id 0)
				       (layer 0)
				       (scene (application-scene *app*)))
  "Immediate-mode function, creates text, returns a no values.  Calls scene-draw-text with color defaulting to *default-color*, font defaulting to (application-default-font *app*), group defaulting to :default, and scene defaulting to (application-scene *app*).  The required arguments should be real numbers.  pos-x and pos-y represent the upper left corner of the text."
  (scene-draw-text scene group font color pos-x pos-y string object-id layer))

