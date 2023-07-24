(in-package :krma)

;;NTSYSAPI NTSTATUS NTAPI NtDelayExecution(BOOLEAN Alertable, PLARGE_INTEGER DelayInterval);
#+windows
(defcfun ("NtDelayExecution" NtDelayExecution) :int
  (alertable :bool)
  (p-delay-interval :pointer))

#+windows
(defcstruct LARGE_INTEGER
  (low-part :uint32)
  (high-part :int32))

#+windows
(defun nt-delay-execution (microseconds &optional (alertable nil))
  (with-foreign-object (p-delay-interval :int64)
    (setf (cffi:mem-aref p-delay-interval :int64) (- (floor microseconds 10)))
    (NtDelayExecution alertable p-delay-interval)))

(defun backtrace-string ()
  (with-output-to-string (*debug-io*)
    #+sbcl(sb-debug:print-backtrace)))

(defun record-backtrace (sys)
  (setf (system-backtrace sys) (backtrace-string)))

(defun record-error-msg (sys c)
  (let ((*print-escape* nil))
    (setf (system-error-msg sys)
          (format nil "~W" c))))

(defun clear-buffer (buffer value aligned-size memory-resource)
  (let ((memory (allocated-memory buffer))
	(offset (vk::memory-resource-offset memory-resource))
	(device (vk::device buffer)))
	      
    (with-foreign-object (pp-dst :pointer)
		
      (check-vk-result (vkMapMemory (h device) (h memory) offset aligned-size 0 pp-dst))
		
      (vk::memset (mem-aref pp-dst :pointer) value aligned-size)

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

	(vkUnmapMemory (h device) (h memory))))))

(defun compute-select-boxes-descriptor-set (dpy window frame-count current-frame)
  (multiple-value-bind (mouse-x mouse-y) (window-cursor-position window)
    (let* ((new-coords (vec4 (- mouse-x 1/2) (- mouse-y 1/2)
			     (+ mouse-x 1/2) (+ mouse-y 1/2)))
	   (width (round (- (vz new-coords) (vx new-coords)))) ;; should be 1.0 atm, in the future it might not be
	   (height (round (- (vw new-coords) (vy new-coords)))) ;; should be 1.0 atm
	   (new-2d-size (* width height +select-box-2d-depth+ (load-time-value (foreign-type-size :unsigned-int))))
	   (new-3d-size (* width height +select-box-3d-depth+ (load-time-value (foreign-type-size :unsigned-int)))))

      

      (when (or (/= width (last-select-box-width dpy))
		(/= height (last-select-box-height dpy))
		(not (krma-select-box-2d dpy))
		(not (krma-select-box-3d dpy)))
	
	(let ((2d-array (make-array (* width height +select-box-2d-depth+) :element-type '(unsigned-byte 32)))
	      (3d-array (make-array (* width height +select-box-3d-depth+) :element-type '(unsigned-byte 32))))
	  
	  (setf (krma-select-box-2d dpy) (make-array (list width height +select-box-2d-depth+)
						     :element-type '(unsigned-byte 32)
						     :displaced-to 2d-array :displaced-index-offset 0))
	  
	  (setf (krma-select-box-3d dpy) (make-array (list width height +select-box-3d-depth+)
						     :element-type '(unsigned-byte 32)
						     :displaced-to 3d-array :displaced-index-offset 0))
	  
	  (setf (last-select-box-width dpy) width
		(last-select-box-height dpy) height)))
      
      (setf (krma-select-box-coords dpy) new-coords)

      (unless (krma-select-boxes-descriptor-sets dpy)
	(setf (krma-select-boxes-descriptor-sets dpy) (make-array frame-count :initial-element nil)))

      (unless (krma-select-box-2d-memory-resources dpy)
	(setf (krma-select-box-2d-memory-resources dpy) (make-array frame-count :initial-element nil)))

      (unless (krma-select-box-3d-memory-resources dpy)
	(setf (krma-select-box-3d-memory-resources dpy) (make-array frame-count :initial-element nil)))

      (let ((aligned-size-2d (aligned-size new-2d-size))
	    (aligned-size-3d (aligned-size new-3d-size))
	    (old-descriptor-set (aref (krma-select-boxes-descriptor-sets dpy) current-frame))
	    (old-2d-memory-resource (aref (krma-select-box-2d-memory-resources dpy) current-frame))
	    (old-3d-memory-resource (aref (krma-select-box-3d-memory-resources dpy) current-frame))
	    (new-2d-memory-resource)
	    (new-3d-memory-resource)
	    (memory-resource-changed-p nil))

	(if old-2d-memory-resource
	      
	    (if (<= aligned-size-2d (vk::memory-resource-size old-2d-memory-resource))
		  
		(setq new-2d-memory-resource old-2d-memory-resource) ;; keep resource the same
		  
		(progn
		  (vk::release-storage-memory dpy old-2d-memory-resource)
		  (setq new-2d-memory-resource (vk::acquire-storage-memory-sized dpy aligned-size-2d :host-visible))
		  (setq memory-resource-changed-p t)))
	      
	    (progn
	      (setq new-2d-memory-resource (vk::acquire-storage-memory-sized dpy aligned-size-2d :host-visible))
	      (setq memory-resource-changed-p t)))

	(if old-3d-memory-resource
	      
	    (if (<= aligned-size-3d (vk::memory-resource-size old-3d-memory-resource))
		  
		(setq new-3d-memory-resource old-3d-memory-resource) ;; keep resource the same
		  
		(progn
		  (vk::release-storage-memory dpy old-3d-memory-resource)
		  (setq new-3d-memory-resource (vk::acquire-storage-memory-sized dpy aligned-size-3d :host-visible))
		  (setq memory-resource-changed-p t)))
	      
	    (progn
	      (setq new-3d-memory-resource (vk::acquire-storage-memory-sized dpy aligned-size-3d :host-visible))
	      (setq memory-resource-changed-p t)))

	(let ((buffer-2d (vk::memory-resource-buffer new-2d-memory-resource))
	      (buffer-3d (vk::memory-resource-buffer new-3d-memory-resource)))
	  
	  (if memory-resource-changed-p
		
	      (progn
		(clear-buffer buffer-2d 0 aligned-size-2d new-2d-memory-resource)
		(clear-buffer buffer-3d 0 aligned-size-3d new-3d-memory-resource)
		  
		(when old-descriptor-set
		  (vk::free-descriptor-sets (list old-descriptor-set) (default-descriptor-pool dpy)))
		  
		(setf (aref (krma-select-box-2d-memory-resources dpy) current-frame)
		      new-2d-memory-resource)
		(setf (aref (krma-select-box-3d-memory-resources dpy) current-frame)
		      new-3d-memory-resource)
		  
		;; create a new descriptor set for new memory resource, offset and range have changed
		(setf (aref (krma-select-boxes-descriptor-sets dpy) current-frame)
		      (create-descriptor-set
		       (default-logical-device dpy)
		       (list (krma-select-boxes-descriptor-set-layout dpy))
		       (default-descriptor-pool dpy)
		       :descriptor-buffer-info (list (make-instance 'descriptor-storage-buffer-info
								    :buffer buffer-2d
								    :offset (vk::memory-resource-offset new-2d-memory-resource)
								    :range new-2d-size)
						     (make-instance 'descriptor-storage-buffer-info
								    :buffer buffer-3d
								    :offset (vk::memory-resource-offset new-3d-memory-resource)
								    :range new-3d-size)))))

	      ;; otherwise return existing descriptor set
	      ;; if the mouse doesn't move the descriptor set doesn't change
	      (aref (krma-select-boxes-descriptor-sets dpy) current-frame)))))))

(defun erase-draw-list (draw-list)
  (declare (type draw-list-mixin draw-list))
  (setf (foreign-array-fill-pointer (draw-list-index-array draw-list)) 0)
  (setf (foreign-array-fill-pointer (draw-list-vertex-array draw-list)) 0)
  (setf (fill-pointer (draw-list-cmd-vector draw-list)) 0))

(defun erase-immediate-mode-draw-data (dpy scene)
  (let* ((draw-data (im-draw-data scene)))
    (let ((combinations-1 (3d-cmd-oriented-combinations (krma-pipeline-store dpy) draw-data))
	  (combinations-2 (3d-draw-list-oriented-combinations (krma-pipeline-store dpy) draw-data))
	  (combinations-3 (2d-cmd-oriented-combinations (krma-pipeline-store dpy) draw-data))
	  (combinations-4 (2d-draw-list-oriented-combinations (krma-pipeline-store dpy) draw-data)))

      (loop for (x draw-list) on combinations-1 by #'cddr
	    do (erase-draw-list draw-list))

      (loop for (x draw-list) on combinations-2 by #'cddr
	    do (erase-draw-list draw-list))

      (loop for (x draw-list) on combinations-3 by #'cddr
	    do (erase-draw-list draw-list))

      (loop for (x draw-list) on combinations-4 by #'cddr
	    do (erase-draw-list draw-list))

      (values))))

(defun call-immediate-mode-work-functions (dpy)
  (let ((f (immediate-mode-work-function-3 dpy)))
    (when f (funcall f)))
  (let ((f (immediate-mode-work-function-2 dpy)))
    (when f (funcall f)))
  (let ((f (immediate-mode-work-function-1 dpy)))
    (when f (funcall f))))

(defun before-frame-begin (dpy scene current-draw-data-index)
  (let ((work-queue))

    (maybe-defer-debug (dpy)
      (erase-immediate-mode-draw-data dpy scene))
	     
    (setq work-queue
	  (draw-data-work-queue (aref (rm-draw-data scene) current-draw-data-index)))

    (maybe-defer-debug (dpy)
      (loop with work = nil
	    while (setq work (and (lparallel.queue:peek-queue work-queue)
				  (lparallel.queue:pop-queue work-queue)))
	    do (funcall work)))

    (sort-2d-draw-lists (aref (rm-draw-data scene) current-draw-data-index))    
    
    (values)))

(defun during-frame (dpy window command-buffer current-draw-data-index show-frame-rate?)

  (let ()


    (when show-frame-rate?
      (maybe-defer-debug (dpy)
	(multiple-value-bind (w h) (window-framebuffer-size window)
	  (declare (ignorable h))
	  (draw-text (format nil "fps: ~4,0f" (window-frame-rate window))
		     (- w 100) 15 :color #x000000ff))))
    
    ;; render here.
    
    (loop for viewport in (window-viewports window)
	  do (let ((scene (viewport-scene viewport)))

	       #+NIL
	       (maybe-defer-debug (dpy)
		 (multiple-value-bind (width height) (window-framebuffer-size window)
		   (update-2d-camera scene
				     (mortho-vulkan 0 width height 0 0 1024))))
	       #+NIL
	       (maybe-defer-debug (dpy)
		 (update-3d-camera scene))

	       (maybe-defer-debug (dpy)
		 (render-scene scene
			       viewport
			       dpy command-buffer
			       (aref (rm-draw-data scene) current-draw-data-index)
			       (im-draw-data scene)))))
 
    
    (values)))

(defun update-counts (current-frame-cons current-draw-data-cons frame-count)
  #+sbcl(sb-ext:atomic-update (car current-frame-cons)
			      #'(lambda (cf) (mod (1+ cf) frame-count)))
  #-sbcl(setf (car current-frame-cons) (mod (1+ (car current-frame-cons)) frame-count))
  #+sbcl(sb-ext:atomic-update (car current-draw-data-cons)
			      #'(lambda (cdd) (mod (1+ cdd) 2)))
  #-sbcl(setf (car current-draw-data-cons) (mod (1+ (car current-draw-data-cons)) 2))
  (values))

(defun recreate-swapchain-when-necessary (window)
  (when (recreate-swapchain? window)
    (multiple-value-bind (width height) (window-framebuffer-size window)
      (recreate-swapchain window (render-pass window) (swapchain window) width height)
      (setf (clui::last-framebuffer-width window) width
	    (clui::last-framebuffer-height window) height)
      (setf (recreate-swapchain? window) nil)))
  (values))

(defun frame-iteration (dpy frame-count show-frame-rate?)
  
  (let* ((current-frame-cons (current-frame-cons dpy))
	 (current-draw-data-cons (current-draw-data-cons dpy))
	 (current-frame (car current-frame-cons))
	 (current-draw-data (car current-draw-data-cons))
	 (image-indices (make-array 100 :adjustable t :fill-pointer 0 :initial-element nil)))

    ;;(print (clui::window-keys (main-window (first (display-applications dpy)))))
    
    ;; maybe create new descriptor set if select box size has changed
    (maybe-defer-debug (dpy)
      ;; probably going to need a select box per framebuffer
      (compute-select-boxes-descriptor-set dpy (main-window (first (display-applications dpy)))
					   frame-count (car current-frame-cons)))

    #-nvidia(maybe-defer-debug (dpy)
	      (read-select-boxes dpy (car (current-frame-cons dpy))))
    
    #-nvidia(maybe-defer-debug (dpy)
	      (monitor-select-boxes dpy))

    ;;(print (krma-select-box-2d dpy))

    ;; This loop takes all the scenes in all the applications
    ;; and updates the portion of the scene's draw-lists
    ;; which are not currently in use, that is,
    ;; not being copied to gpu memory, or not being compacted.
    ;; It is a candidate for parallelization.
    (loop for app in (display-applications dpy)
       do (loop for scene in (active-scenes app)
	     do (before-frame-begin dpy scene current-draw-data)))

    (maybe-defer-debug (dpy)
      (call-immediate-mode-work-functions dpy))

    ;; This loop is a candidate for parallelization
    ;; It processes each scene on each application
    ;; in the context of a window.
    ;; Todo: if the scene does not appear in the window, by means of comparing clip coordinates,
    ;; then it is not processed for that window
    (do ((window (clui::display-window-list-head dpy) (clui::window-next window)))
	((null window))
      
      (recreate-swapchain-when-necessary window)
      
      (with-slots (queue command-pool) window
	
	(let* ((swapchain (swapchain window))
	       (frame-resource (elt (frame-resources swapchain) current-frame))
	       (command-buffer (frame-command-buffer frame-resource)))
	  
	  (vector-push-extend
	   (frame-begin swapchain (render-pass window)
			current-frame (clear-value window)
			command-pool)
	   image-indices)

	  
	  #+nvidia(maybe-defer-debug (dpy)
		    (read-select-boxes dpy (car (current-frame-cons dpy))))

	  (during-frame dpy window command-buffer current-draw-data show-frame-rate?))))
    
    ;; frame-present must occur in this thread, so no parallelization here
    (do* ((window (clui::display-window-list-head dpy) (clui::window-next window))
	  (i 0 (1+ i))
	  (image-index (aref image-indices i)))
	
	 ((null window))

      (with-slots (queue command-pool) window
	
	(let* ((swapchain (swapchain window))
	       (current-frame-cons (current-frame-cons dpy))
	       (current-frame (car current-frame-cons)))
	  
	  (frame-end swapchain queue current-frame)
	  
	  (frame-present swapchain queue current-frame image-index window))))

    

    #+nvidia(maybe-defer-debug (dpy)
	      (monitor-select-boxes dpy))

    ;; first time use of compacting complete semaphore is :count 1
    ;; this needs to be the only thread that modifies current-frame
    (update-counts (current-frame-cons dpy) (current-draw-data-cons dpy) (number-of-images (swapchain (main-window (first (display-applications dpy))))))
    (bt:wait-on-semaphore (compacting-complete-semaphore dpy))
    (bt:signal-semaphore (frame-iteration-complete-semaphore dpy))

    (values)))

(defun compactor-thread-iteration (dpy active-scenes)
  (bt:wait-on-semaphore (frame-iteration-complete-semaphore dpy))
  (let* ((current-draw-data-cons (current-draw-data-cons dpy))
	 (alt-index (mod (1+ (car current-draw-data-cons)) 2)))

    (loop for active-scene in active-scenes
	  do (let ((rm-draw-data-pair (rm-draw-data active-scene)))
	       (compact-draw-lists
		dpy
		;; the draw data that is not currently being modified
		(aref rm-draw-data-pair alt-index))))
    
    (bt:signal-semaphore (compacting-complete-semaphore dpy))))

(defun compactor-loop (dpy)
  ;; doesn't start until after first render loop iteration
  (tagbody
   again
     (let ((active-scenes ()))
       (loop for app in (display-applications dpy)
	     do (setf active-scenes (nconc active-scenes (active-scenes app))))
       (compactor-thread-iteration dpy active-scenes))
     ;; todo: make close button on window setf application-exit? to t.
     (when (run-loop-exit? dpy)
       (go exit))
     (go again)
   exit))

(defun start-compactor-thread (dpy)
  (bt:make-thread #'(lambda ()
		      (compactor-loop dpy))
		  :name "draw-list-compactor-thread"))

(defvar *threshold* 0.008)
(defvar *test* 1290)

#+(and cocoa noglfw)
(defun krma-application-main (app &rest args &key (show-frame-rate? t) &allow-other-keys)
  (declare (ignore args))
  (let* ((main-window (main-window app))
	 (dpy (clui::window-display main-window)))
    
    (setf (window-show-frame-rate? main-window) show-frame-rate?)

    ;; this is called in ApplicationDidFinishLaunching:
    ;;(start-compactor-thread dpy)
    ;; need to move that method from clui/ to krma/
    
    (unwind-protect (progn
		      (ns::|run| dpy))
      (shutdown-run-loop dpy))))



#+cocoa
(defmethod clui::content-view-draw-rect ((window vk::vulkan-window-mixin) view rect)
  (declare (ignore view rect))
  (with-slots ((dpy clui::display) queue command-pool) window
    (maybe-defer-debug (dpy)
      (update-frame-rate window))
    (maybe-defer-debug (dpy)
      (frame-iteration dpy (number-of-images (swapchain window)) (window-show-frame-rate? window)))))

#-cocoa
(defun krma-application-main (app &rest args &key (show-frame-rate? t) &allow-other-keys)
  (declare (ignorable args))

  (let* ((main-window (main-window app))
	 (dpy (clui::window-display main-window)))
    
    (setf (window-show-frame-rate? main-window) show-frame-rate?)

    (unwind-protect
	 (loop until (run-loop-exit? dpy)
		 initially (start-compactor-thread dpy)
	       do (maybe-defer-debug (dpy)
		    (poll-events dpy))

		  (maybe-defer-debug (dpy)
		    (update-frame-rate main-window))
		  
		  (maybe-defer-debug (dpy)
		    (frame-iteration dpy (number-of-images (swapchain main-window)) show-frame-rate?)))
      
      (shutdown-run-loop dpy))))
    

(defgeneric main (application &rest args &key &allow-other-keys)
  (:documentation "Define your own main function for your custom application if necessary."))


(defmethod main ((app krma-application-mixin) &rest args &key &allow-other-keys)
  (apply #'krma-application-main app args))

#+NIL
(defun run-1 (&rest args &key (class *default-application-class*) (throttle-frame-rate? t) &allow-other-keys)
  "Function to call from main thread to create and run an application object."
  ;; #+(and darwin sbcl)(sb-int:set-floating-point-modes :traps nil) ;; this happens in vk:create-instance now.
  (let ((args (copy-list args)))
    (remf args :class)
    (remf args :throttle-frame-rate?)
    (let ((app (apply #'make-instance class :throttle-frame-rate? throttle-frame-rate? args)))
      (apply #'main app args)
      (setq *app* nil)
      t)))
#+NIL
(defun run (&rest args &key (class *default-application-class*) (show-frame-rate? krma::*debug*) &allow-other-keys)
  "Function which can be called from any thread to create and run an application object."
  (trivial-main-thread:call-in-main-thread
   (lambda ()
     (let ((args (copy-list args)))
       (remf args :show-frame-rate?)
       (apply #'run-1 :class class :show-frame-rate? show-frame-rate? args)))))



