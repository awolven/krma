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
    (sb-debug:print-backtrace)))

(defun record-backtrace (app)
  (setf (application-backtrace app) (backtrace-string)))

(defun record-error-msg (app c)
  (let ((*print-escape* nil))
    (setf (application-error-msg app)
          (format nil "~W" c))))

(defun compute-select-box-descriptor-set (app)
  (multiple-value-bind (mouse-x mouse-y) (get-cursor-pos (main-window app))
    (let* ((new-coords (vec4 (- mouse-x 1/2) (- mouse-y 1/2)
			     (+ mouse-x 1/2) (+ mouse-y 1/2)))
	   (width (round (- (vz new-coords) (vx new-coords)))) ;; should be 1.0 atm, in the future it might not be
	   (height (round (- (vw new-coords) (vy new-coords)))) ;; should be 1.0 atm
	   (new-size (* width height +select-box-depth+ (load-time-value (foreign-type-size :unsigned-int)))))
      
      (setf (application-select-box-coords app) new-coords)
      
      (let ((aligned-size (aligned-size new-size))
	    (old-descriptor-set (application-select-box-descriptor-set app))
	    (old-memory-resource (application-select-box-memory-resource app))
	    (new-memory-resource)
	    (memory-resource-changed-p nil))
        
	(if old-memory-resource
	      
	    (if (<= aligned-size (vk::memory-resource-size old-memory-resource))
		  
		(setq new-memory-resource old-memory-resource) ;; keep resource the same
		  
		(progn
		  (vk::release-storage-memory app old-memory-resource)
		  (setq new-memory-resource (vk::acquire-storage-memory-sized app aligned-size :host-visible))
		  (setq memory-resource-changed-p t)))
	      
	    (progn 
	      (setq new-memory-resource (vk::acquire-storage-memory-sized app aligned-size :host-visible))
	      (setq memory-resource-changed-p t)))
	
	(flet ((clear-buffer (buffer value aligned-size memory-resource)
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

		       (vkUnmapMemory (h device) (h memory)))))))
	  
	  (let ((buffer (vk::memory-pool-buffer (vk::storage-buffer-memory-pool app))))
	    
	    (clear-buffer buffer 0 aligned-size new-memory-resource)

	    (if memory-resource-changed-p
		
		(progn
		  (when old-descriptor-set
		    (vk::free-descriptor-sets (list old-descriptor-set) (default-descriptor-pool app)))
		  
		  (setf (application-select-box-memory-resource app)
			new-memory-resource)
		  
		  ;; create a new descriptor set for new memory resource, offset and range have changed
		  (setf (application-select-box-descriptor-set app)
			(create-descriptor-set
			 (default-logical-device app)
			 (list (application-select-box-descriptor-set-layout app))
			 (default-descriptor-pool app)
			 :descriptor-buffer-info (list (make-instance 'descriptor-storage-buffer-info
								      :buffer buffer
								      :offset (vk::memory-resource-offset new-memory-resource)
								      :range new-size)))))

		;; otherwise return existing descriptor set
		;; if the mouse doesn't move the descriptor set doesn't change
		(application-select-box-descriptor-set app))))))))

(defun erase-draw-list (draw-list)
  (declare (type draw-list-mixin draw-list))
  (setf (foreign-array-fill-pointer (draw-list-index-array draw-list)) 0)
  (setf (foreign-array-fill-pointer (draw-list-vertex-array draw-list)) 0)
  (setf (fill-pointer (draw-list-cmd-vector draw-list)) 0))

(defun erase-immediate-mode-draw-data (app)
  (let* ((scene (application-scene app))
         (draw-data (im-draw-data scene)))
    (let ((combinations-1 (3d-cmd-oriented-combinations (application-pipeline-store app) draw-data))
	  (combinations-2 (3d-draw-list-oriented-combinations (application-pipeline-store app) draw-data))
	  (combinations-3 (2d-cmd-oriented-combinations (application-pipeline-store app) draw-data))
	  (combinations-4 (2d-draw-list-oriented-combinations (application-pipeline-store app) draw-data)))

      (loop for (x draw-list) on combinations-1 by #'cddr
	    do (erase-draw-list draw-list))

      (loop for (x draw-list) on combinations-2 by #'cddr
	    do (erase-draw-list draw-list))

      (loop for (x draw-list) on combinations-3 by #'cddr
	    do (erase-draw-list draw-list))

      (loop for (x draw-list) on combinations-4 by #'cddr
	    do (erase-draw-list draw-list))

      (values))))

(defun call-immediate-mode-work-functions (app)
  (let ((f (immediate-mode-work-function-1 app)))
    (when f (funcall f))))

(defun before-frame-begin (app rm-draw-data)
  (let ((work-queue))

    ;; maybe create new descriptor set if select box size has changed
    (maybe-defer-debug (app)
      (compute-select-box-descriptor-set app))
    
    (maybe-defer-debug (app)
      (erase-immediate-mode-draw-data app))
    
    (setq work-queue (draw-data-work-queue rm-draw-data))
    
    (maybe-defer-debug (app)
      (loop with work = nil
	    while (setq work (sb-concurrency:dequeue work-queue))
	    do (funcall work)))

    (maybe-defer-debug (app)
      (call-immediate-mode-work-functions app))

    (values)))

(defun during-frame (app command-buffer scene rm-draw-data show-frame-rate?)
  
  (maybe-defer-debug (app)
    (update-2d-camera scene))
  
  (maybe-defer-debug (app)
    (update-3d-camera scene))

  (when show-frame-rate?
    (maybe-defer-debug (app)
      (multiple-value-bind (w h) (get-framebuffer-size (main-window app))
	(draw-text (format nil "fps: ~4,0f" (application-frame-rate app)) (- w 100) (- h 25) :color #x000000ff))))
  
  ;; render here.
  (maybe-defer-debug (app)
    (render-scene scene app command-buffer rm-draw-data (im-draw-data scene)))

  (maybe-defer-debug (app)
    (read-select-box app))

  (values))

(defun update-counts (current-frame-cons current-draw-data-cons frame-count)
  (sb-ext:atomic-update (car current-frame-cons)
			#'(lambda (cf) (mod (1+ cf) frame-count)))
  (sb-ext:atomic-update (car current-draw-data-cons)
			#'(lambda (cdd) (mod (1+ cdd) 2))))

(defun frame-iteration (app queue command-pool show-frame-rate?)
  (let ((main-window (main-window app)))

    (glfwPollEvents)
		 
    (when (recreate-swapchain? main-window)
      (multiple-value-bind (width height) (get-framebuffer-size main-window)
	(recreate-swapchain main-window (swapchain main-window) width height)
	(setf (main-window-width app) width
	      (main-window-height app) height)
	(setf (recreate-swapchain? main-window) nil)))

    (let* ((swapchain (swapchain main-window))
	   (frame-count (number-of-images swapchain))
	   (image-index)
	   (current-frame-cons (current-frame-cons app))
	   (current-draw-data-cons (current-draw-data-cons app))
	   (current-frame (car current-frame-cons))
	   (current-draw-data (car current-draw-data-cons))
	   (scene (application-scene app))
	   (frame-resource (elt (frame-resources swapchain) current-frame))
	   (command-buffer (frame-command-buffer frame-resource))
	   (rm-draw-data (aref (rm-draw-data scene) current-draw-data)))

      (before-frame-begin app rm-draw-data)

      (setq image-index
	    (frame-begin swapchain (render-pass swapchain)
			 current-frame (clear-value main-window)
			 command-pool))

      (during-frame app command-buffer scene rm-draw-data show-frame-rate?)
		       
      (frame-end swapchain queue current-frame)
		       
      (frame-present swapchain queue current-frame image-index main-window)

      ;; this needs to be the only thread that modifies current-frame
      (update-counts current-frame-cons current-draw-data-cons frame-count))))

(defvar *threshold* 0.008)
(defvar *test* 1000000000)

(defun krma-application-main (app &rest args &key (show-frame-rate? t) (throttle-frame-rate? t) &allow-other-keys)
  (declare (ignore args))
  (let* ((main-window (main-window app))
	 (device (default-logical-device app))
	 (index (queue-family-index (render-surface main-window)))
	 (queue (find-queue device index))
	 (command-pool (find-command-pool device index)))
    
    (loop while (zerop (glfwWindowShouldClose (h main-window)))
	  with frames = 0
	  with delta-time = 1
	  with time = (/ (get-internal-real-time) internal-time-units-per-second)
	  with base-time = 0
	  with last-time = time
	  with old-time = 0
	  with dt-total = 0
	  when (application-exit? app)	    
	    do (return)
	  do (maybe-defer-debug (app)
	       (incf frames)
	       (setq time (/ (get-internal-real-time) internal-time-units-per-second))
	       (setq delta-time (- time base-time))
	       (when (>= delta-time 1)
		 (setf (application-frame-rate app) (float (/ frames delta-time)))
		 (setq base-time time)
		 (setq frames 0))
	       (when throttle-frame-rate?
		 (setq dt-total (- time old-time))
		 (loop while (> dt-total 0) for i from 0 to 100
		       do
			  (let ((dt))
			    (if (> dt-total *threshold*)
				(setq dt *threshold*)
				(progn
				  (setq dt dt-total)))
			    #+windows(nt-delay-execution (floor (* dt 840000)))
			    #+unix(sb-unix:nanosleep 0 (floor (* dt 840000000)))
			    (decf dt-total dt)))
		 (setq old-time time))
	       (frame-iteration app queue command-pool show-frame-rate?)))
    
    (shutdown-application app)))

(defmethod main ((app krma-test-application) &rest args &key &allow-other-keys)
  (apply #'krma-application-main app args))
  
(defun run-1 (&rest args &key (class *default-application-class*) (throttle-frame-rate? t) &allow-other-keys)
  ;; #+darwin(sb-int:set-floating-point-modes :traps nil) ;; this happens in vk:create-instance now.
  (let ((args (copy-list args)))
    (remf args :class)
    (remf args :throttle-frame-rate?)
    (let ((app (apply #'make-instance class :throttle-frame-rate? throttle-frame-rate? args)))
      (apply #'main app args)
      (setq *app* nil)
      t)))

(defun run (&rest args &key (class *default-application-class*) (show-frame-rate? krma::*debug*) &allow-other-keys)
  (trivial-main-thread:call-in-main-thread
   (lambda ()
     (let ((args (copy-list args)))
       (remf args :show-frame-rate?)
       (apply #'run-1 :class class :show-frame-rate? show-frame-rate? args)))))
