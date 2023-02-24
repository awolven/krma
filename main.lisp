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

(defun compute-select-box-descriptor-set (dpy window)
  (multiple-value-bind (mouse-x mouse-y) (window-cursor-position window)
    (multiple-value-bind (xscale yscale) (window-content-scale window)
      (setq mouse-x (* xscale mouse-x))
      (setq mouse-y (* yscale mouse-y)))
    (let* ((new-coords (vec4 (- mouse-x 1/2) (- mouse-y 1/2)
			     (+ mouse-x 1/2) (+ mouse-y 1/2)))
	   (width (round (- (vz new-coords) (vx new-coords)))) ;; should be 1.0 atm, in the future it might not be
	   (height (round (- (vw new-coords) (vy new-coords)))) ;; should be 1.0 atm
	   (new-size (* width height +select-box-depth+ (load-time-value (foreign-type-size :unsigned-int)))))
      
      (setf (krma-select-box-coords dpy) new-coords)
      
      (let ((aligned-size (aligned-size new-size))
	    (old-descriptor-set (krma-select-box-descriptor-set dpy))
	    (old-memory-resource (krma-select-box-memory-resource dpy))
	    (new-memory-resource)
	    (memory-resource-changed-p nil))
        
	(if old-memory-resource
	      
	    (if (<= aligned-size (vk::memory-resource-size old-memory-resource))
		  
		(setq new-memory-resource old-memory-resource) ;; keep resource the same
		  
		(progn
		  (vk::release-storage-memory dpy old-memory-resource)
		  (setq new-memory-resource (vk::acquire-storage-memory-sized dpy aligned-size :host-visible))
		  (setq memory-resource-changed-p t)))
	      
	    (progn 
	      (setq new-memory-resource (vk::acquire-storage-memory-sized dpy aligned-size :host-visible))
	      (setq memory-resource-changed-p t)))

	(let ((buffer (vk::memory-pool-buffer (vk::storage-buffer-memory-pool dpy))))
	  
	  (if memory-resource-changed-p
		
	      (progn
		(clear-buffer buffer 0 aligned-size new-memory-resource)
		  
		(when old-descriptor-set
		  (vk::free-descriptor-sets (list old-descriptor-set) (default-descriptor-pool dpy)))
		  
		(setf (krma-select-box-memory-resource dpy)
		      new-memory-resource)
		  
		;; create a new descriptor set for new memory resource, offset and range have changed
		(setf (krma-select-box-descriptor-set dpy)
		      (create-descriptor-set
		       (default-logical-device dpy)
		       (list (krma-select-box-descriptor-set-layout dpy))
		       (default-descriptor-pool dpy)
		       :descriptor-buffer-info (list (make-instance 'descriptor-storage-buffer-info
								    :buffer buffer
								    :offset (vk::memory-resource-offset new-memory-resource)
								    :range new-size)))))

	      ;; otherwise return existing descriptor set
	      ;; if the mouse doesn't move the descriptor set doesn't change
	      (krma-select-box-descriptor-set dpy)))))))

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

(defun call-immediate-mode-work-functions (scene)
  (let ((f (immediate-mode-work-function-1 scene)))
    (when f (funcall f))))

(defun before-frame-begin (dpy scene current-draw-data-index)
  (let ((work-queue))

    (maybe-defer-debug (dpy)
      (erase-immediate-mode-draw-data dpy scene))
	     
    (setq work-queue
	  (draw-data-work-queue (aref (rm-draw-data scene) current-draw-data-index)))
    
    (maybe-defer-debug (dpy)
      (loop with work = nil
	    while (setq work #+sbcl (sb-concurrency:dequeue work-queue)
			     #-sbcl (lparallel.queue:pop-queue work-queue))
	    do (funcall work)))
    
    (maybe-defer-debug (dpy)
      (call-immediate-mode-work-functions scene))
    
    (values)))

(defun during-frame (dpy command-buffer scene current-draw-data-index show-frame-rate?)

  (let ()
  
    (maybe-defer-debug (dpy)
      (update-2d-camera scene))
  
    (maybe-defer-debug (dpy)
      (update-3d-camera scene))

    #+NOTYET
    (when show-frame-rate?
      (maybe-defer-debug (dpy)
	(multiple-value-bind (w h) (window-framebuffer-size window)
	  (multiple-value-bind (xscale yscale) (window-content-scale window)
	    (draw-text (format nil "fps: ~4,0f" (window-frame-rate window))
		       (- (/ w xscale) 100) (- (/ h yscale) 25) :color #x000000ff)))))

    ;; render here.
    (maybe-defer-debug (dpy)
      (render-scene scene dpy command-buffer
		    (aref (rm-draw-data scene) current-draw-data-index)
		    (im-draw-data scene)))
  
    (maybe-defer-debug (dpy)
      (read-select-box dpy))

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
      (recreate-swapchain window (swapchain window) width height)
      (setf (clui::last-framebuffer-width window) width
	    (clui::last-framebuffer-height window) height)
      (setf (recreate-swapchain? window) nil)))
  (values))

(defun frame-iteration (dpy frame-count show-frame-rate?)
  
  (let ((current-frame-cons (current-frame-cons dpy))
	(current-draw-data-cons (current-draw-data-cons dpy))
	(current-frame (car current-frame-cons))
	(current-draw-data (car current-draw-data-cons))
	(image-indices (make-array 100 :adjustable t :fill-pointer 0 :initial-element nil)))
    
    ;; maybe create new descriptor set if select box size has changed
    (maybe-defer-debug (dpy)
      (compute-select-box-descriptor-set dpy))
    
    ;; this loop is a candidate for parallelization
    ;; though, even though each scene will process it's work queue in order
    ;; developer might not expect operations between scenes to be executed interleaved
    ;; theres actually no gaurantee when the op is done except that it's before render
    (loop for scene in (active-scenes dpy) 
	  do (before-frame-begin dpy scene current-draw-data))

    ;; this loop is a candidate for parallelization
    (do ((window (clui::display-window-list-head dpy) (clui::window-next window)))
	((null window))
      
      (recreate-swapchain-if-necessary window)
      
      (with-slots (queue command-pool) window
	
	(let* ((swapchain (swapchain window))
	       (frame-resource (elt (frame-resources swapchain) current-frame))
	       (command-buffer (frame-command-buffer frame-resource))
	       (image-index))
	  
	  (vector-push-extend
	   (frame-begin swapchain (render-pass swapchain)
			current-frame (clear-value window)
			command-pool)
	   image-indices)
	  
	  (loop for scene in (active-scenes window) ;; this loop is a candidate for parallelization
		do (during-frame dpy command-buffer scene current-draw-data show-frame-rate?)))))

    ;; frame-present must occur in this thread, so no parallelization here
    (do ((window (clui::display-window-list-head dpy) (clui::window-next window))
	 (i 0 (1+ i))
	 (image-index (aref image-indices i)))
	
	((null window))

      (with-slots (queue command-pool) window
	
	(let* ((swapchain (swapchain window))
	       (current-frame-cons (current-frame-cons dpy))
	       (current-frame (car current-frame-cons)))
	  
	  (frame-end swapchain queue current-frame)
	  
	  (frame-present swapchain queue current-frame image-index window))))

    ;; this needs to be the only thread that modifies current-frame
    (update-counts current-frame-cons current-draw-data-cons frame-count)
    
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
     (compactor-thread-iteration dpy (active-scenes dpy))
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

#+(and noglfw nil)
(defun krma-application-main (app &rest args &key (show-frame-rate? t) &allow-other-keys)
  (declare (ignore args))
  (setf (window-show-frame-rate? (main-window app)) show-frame-rate?)
;;  (ns::|setNeedsDisplay:| (abstract-os::window-content-view (main-window app)) t)
  (unwind-protect (progn
		    (start-compactor-thread app)
		    (ns::|run| app))
    (shutdown-application app)))

#+(and noglfw darwin)
(defmethod abstract-os::application-did-finish-launching ((application vulkan-application-mixin) notification)
  (declare (ignorable notification))
  ;;(abstract-os::post-empty-event application)
  ;;(ns::|stop:| application nil)
  (start-compactor-thread application)
  (values))



#+cocoa
(defmethod clui::content-view-draw-rect ((window vk::vulkan-window-mixin) view rect)
  (with-slots ((app vk::application) queue command-pool) window
    (maybe-defer-debug (app)
      (update-frame-rate window))
    (maybe-defer-debug (app)
      (frame-iteration app queue command-pool t))))

(defun krma-application-main (dpy main-window &rest args &key (show-frame-rate? t) &allow-other-keys)
  (declare (ignorable args))
  #+(and noglfw darwin)
  (abstract-os::cocoa-finish-init app)

  (setf (window-show-frame-rate? main-window) show-frame-rate?)

  (unwind-protect
       (loop until (run-loop-exit? dpy)
	       initially (start-compactor-thread dpy)
	     do (maybe-defer-debug (dpy)
		  (poll-events dpy))
		
		(maybe-defer-debug (dpy)
		  (update-frame-rate main-window))
		
		(do ((window (clui::display-window-list-head dpy) (clui::window-next window)))
		    ((null window))
		  (with-slots (queue command-pool) window
		    (maybe-defer-debug (dpy)
		      (frame-iteration dpy window queue command-pool show-frame-rate?))))
       
		;; first time use of compacting complete semaphore is :count 1
		(bt:wait-on-semaphore (compacting-complete-semaphore dpy))
		(bt:signal-semaphore (frame-iteration-complete-semaphore dpy)))

    (shutdown-run-loop dpy)))
    
#+OLD
(defun krma-application-main (app &rest args &key (show-frame-rate? t) (throttle-frame-rate? t) &allow-other-keys)
  (declare (ignore args))
  (let* ((main-window (main-window app))
	 (device (default-logical-device app))
	 (index (queue-family-index (render-surface main-window)))
	 (queue (find-queue device index))
	 (command-pool (find-command-pool device index)))
    
    (loop until (window-should-close? main-window)
	  initially (start-compactor-thread app)
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
		 #+ignore
		 (
		 (setq dt-total (- time old-time))
		 (loop while (> dt-total 0) for i from 0 below 1
		       do
			  (let ((dt))
			    (if (> dt-total *threshold*)
				(setq dt *threshold*)
				(progn
				  (setq dt dt-total)))
			    ;; because the time resolution sucks, frame rate throttling sucks.
			    #+windows(nt-delay-execution (* dt *test*))
			    #+unix(sb-unix:nanosleep 0 (floor (* dt 840000000)))
			    (decf dt-total dt)))
		 (setq old-time time))
		 )
	       (frame-iteration app queue command-pool show-frame-rate?)
	       ;; first time use of compacting complete semaphore is :count 1
	       (bt:wait-on-semaphore (compacting-complete-semaphore app))
	       (bt:signal-semaphore (frame-iteration-complete-semaphore app))))
    
    (shutdown-application app)))

#+NIL
(defgeneric main (application &rest args &key &allow-other-keys)
  (:documentation "Define your own main function for your custom application if necessary."))

#+NIL
(defmethod main ((app krma-test-application) &rest args &key &allow-other-keys)
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
