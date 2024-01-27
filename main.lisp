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

(defun read-selection-set (window frame-count frame-to-read)
  (read-buffer (vk::memory-resource-buffer
		(aref (krma-selection-set-table-memory-resources window) frame-to-read))
	       (krma-selection-set-table window) (* 4 1024)
	       (aref (krma-selection-set-table-memory-resources window) frame-to-read)
	       (* 4 1024))
  #+NIL
  (read-buffer (vk::memory-resource-buffer
		(aref (krma-selection-set-buckets-memory-resources window) frame-to-read))
	       (krma-selection-set-buckets window) (* 4 32 1024)
	       (aref (krma-selection-set-buckets-memory-resources window) frame-to-read)
	       (* 4 32 1024))
  #+NIL
  (read-buffer (vk::memory-resource-buffer
		(krma-selection-set-counter-memory-resource window))
	       (krma-selection-set-counters window) (* frame-count 4)
	       (krma-selection-set-counter-memory-resource window) 512))
  

(defun allocate-selection-set-tables (window frame-count current-frame)
  (let ((display (clui:window-display window)))

    (unless (krma-selection-set-counters window)
      (setf (krma-selection-set-counters window)
	    (make-array frame-count :element-type '(unsigned-byte 32))))

    (unless (krma-selection-set-buckets window)
      (setf (krma-selection-set-buckets window) (make-array (* 32 1024) :element-type '(unsigned-byte 32))))

    (unless (krma-selection-set-table window)
      (setf (krma-selection-set-table window) (make-array 1024 :element-type '(unsigned-byte 32))))
    
    (unless (krma-selection-set-buckets-pointers window)
      (setf (krma-selection-set-buckets-pointers window) (make-array frame-count :initial-element nil)))

    (unless (krma-selection-set-table-pointers window)
      (setf (krma-selection-set-table-pointers window) (make-array frame-count :initial-element nil)))

    (unless (krma-selection-set-counter-pointers window)
      (setf (krma-selection-set-counter-pointers window) (make-array frame-count :initial-element nil)))

    (unless (krma-selection-set-counter-memory-resource window)
      ;; minimum aligned size for 8 bytes X num-frames
      (setf (krma-selection-set-counter-memory-resource window)
	    (vk::acquire-storage-memory-sized display 512 :host-visible)))

    (unless (aref (krma-selection-set-counter-pointers window) current-frame)
      (setf (aref (krma-selection-set-counter-pointers window) current-frame)
	    (let ((mr (krma-selection-set-counter-memory-resource window)))
	      (%vk::with-vkBufferDeviceAddressInfo (p-info)
		(setf %vk::buffer (h (vk::memory-resource-buffer mr)))
		(+ (* 8 current-frame)
		   (vk::memory-resource-offset mr) (%vk::vkGetBufferDeviceAddress (h (default-logical-device display)) p-info))))))
    
    (unless (krma-selection-set-buckets-memory-resources window)
      (setf (krma-selection-set-buckets-memory-resources window) (make-array frame-count :initial-element nil)))

    (unless (aref (krma-selection-set-buckets-memory-resources window) current-frame)
      (setf (aref (krma-selection-set-buckets-memory-resources window) current-frame)
	    (vk::acquire-storage-memory-sized display (* 4 32 1024) :host-visible)))

    (unless (aref (krma-selection-set-buckets-pointers window) current-frame)
      (setf (aref (krma-selection-set-buckets-pointers window) current-frame)
	    (let ((mr (aref (krma-selection-set-buckets-memory-resources window) current-frame)))
	      (%vk::with-vkBufferDeviceAddressInfo (p-info)
		(setf %vk::buffer (h (vk::memory-resource-buffer mr)))
		(+ (vk::memory-resource-offset mr) (%vk::vkGetBufferDeviceAddress (h (default-logical-device display)) p-info))))))
        
    (unless (krma-selection-set-table-memory-resources window)
      (setf (krma-selection-set-table-memory-resources window) (make-array frame-count :initial-element nil)))

    (unless (aref (krma-selection-set-table-memory-resources window) current-frame)
      (setf (aref (krma-selection-set-table-memory-resources window) current-frame)
	    (vk::acquire-storage-memory-sized display (* 4 1024) :host-visible)))

    (unless (aref (krma-selection-set-table-pointers window) current-frame)
      (setf (aref (krma-selection-set-table-pointers window) current-frame)
	    (let ((mr (aref (krma-selection-set-table-memory-resources window) current-frame)))
	      (%vk::with-vkBufferDeviceAddressInfo (p-info)
		(setf %vk::buffer (h (vk::memory-resource-buffer mr)))
		(+ (vk::memory-resource-offset mr) (%vk::vkGetBufferDeviceAddress (h (default-logical-device display)) p-info))))))

    (clear-buffer (vk::memory-resource-buffer
		   (aref (krma-selection-set-buckets-memory-resources window) current-frame))
		  0 (* 4 32 1024)
		  (aref (krma-selection-set-buckets-memory-resources window) current-frame))

    (clear-buffer (vk::memory-resource-buffer
		   (aref (krma-selection-set-table-memory-resources window) current-frame))
		  0 (* 4 1024)
		  (aref (krma-selection-set-table-memory-resources window) current-frame))

    (clear-buffer (vk::memory-resource-buffer
		   (krma-selection-set-counter-memory-resource window))
		  0 512
		  (krma-selection-set-counter-memory-resource window))

    (values)))

      

    
    
  


(defun compute-select-boxes-descriptor-set (window frame-count current-frame)
  (let* (#+NOMORE(width (abs (round (- (krma-select-box-x1 window) (krma-select-box-x0 window)))))
	 #+NOMORE(height (abs (round (- (krma-select-box-y1 window) (krma-select-box-y0 window)))))
	 (width 1)
	 (height 1)
	 (new-2d-size (* width height +select-box-2d-depth+ (load-time-value (foreign-type-size :unsigned-int))))
	 (new-3d-size (* width height +select-box-3d-depth+ (load-time-value (foreign-type-size :unsigned-int))))
	 (display (clui:window-display window)))

    (when (or (/= width (last-select-box-width window))
	      (/= height (last-select-box-height window))
	      (not (krma-select-box-2d window))
	      (not (krma-select-box-3d window)))
	
      (let ((2d-array (make-array (* width height +select-box-2d-depth+) :element-type '(unsigned-byte 32)))
	    (3d-array (make-array (* width height +select-box-3d-depth+) :element-type '(unsigned-byte 32))))
	  
	(setf (krma-select-box-2d window) (make-array (list width height +select-box-2d-depth+)
						   :element-type '(unsigned-byte 32)
						   :displaced-to 2d-array :displaced-index-offset 0))
	  
	(setf (krma-select-box-3d window) (make-array (list width height +select-box-3d-depth+)
						   :element-type '(unsigned-byte 32)
						   :displaced-to 3d-array :displaced-index-offset 0))
	  
	(setf (last-select-box-width window) width
	      (last-select-box-height window) height)))

    (unless (krma-select-boxes-descriptor-sets window)
      (setf (krma-select-boxes-descriptor-sets window) (make-array frame-count :initial-element nil)))

    (unless (krma-select-box-2d-memory-resources window)
      (setf (krma-select-box-2d-memory-resources window) (make-array frame-count :initial-element nil)))

    (unless (krma-select-box-3d-memory-resources window)
      (setf (krma-select-box-3d-memory-resources window) (make-array frame-count :initial-element nil)))

    (let ((aligned-size-2d (aligned-size new-2d-size))
	  (aligned-size-3d (aligned-size new-3d-size))
	  (old-descriptor-set (aref (krma-select-boxes-descriptor-sets window) current-frame))
	  (old-2d-memory-resource (aref (krma-select-box-2d-memory-resources window) current-frame))
	  (old-3d-memory-resource (aref (krma-select-box-3d-memory-resources window) current-frame))
	  (new-2d-memory-resource)
	  (new-3d-memory-resource)
	  (memory-resource-changed-p nil))

      (if old-2d-memory-resource
	      
	  (if (<= aligned-size-2d (vk::memory-resource-size old-2d-memory-resource))
		  
	      (setq new-2d-memory-resource old-2d-memory-resource) ;; keep resource the same
		  
	      (progn
		(vk::release-storage-memory display old-2d-memory-resource)
		(setq new-2d-memory-resource (vk::acquire-storage-memory-sized display aligned-size-2d :host-visible))
		(setq memory-resource-changed-p t)))
	      
	  (progn
	    (setq new-2d-memory-resource (vk::acquire-storage-memory-sized display aligned-size-2d :host-visible))
	    (setq memory-resource-changed-p t)))

      (if old-3d-memory-resource
	      
	  (if (<= aligned-size-3d (vk::memory-resource-size old-3d-memory-resource))
		  
	      (setq new-3d-memory-resource old-3d-memory-resource) ;; keep resource the same
		  
	      (progn
		(vk::release-storage-memory display old-3d-memory-resource)
		(setq new-3d-memory-resource (vk::acquire-storage-memory-sized display aligned-size-3d :host-visible))
		(setq memory-resource-changed-p t)))
	      
	  (progn
	    (setq new-3d-memory-resource (vk::acquire-storage-memory-sized display aligned-size-3d :host-visible))
	    (setq memory-resource-changed-p t)))

      (let ((buffer-2d (vk::memory-resource-buffer new-2d-memory-resource))
	    (buffer-3d (vk::memory-resource-buffer new-3d-memory-resource)))
	  
	(if memory-resource-changed-p
		
	    (progn
	      (clear-buffer buffer-2d 0 aligned-size-2d new-2d-memory-resource)
	      (clear-buffer buffer-3d 0 aligned-size-3d new-3d-memory-resource)
		  
	      (when old-descriptor-set
		(vk::free-descriptor-sets (list old-descriptor-set) (default-descriptor-pool display)))
		  
	      (setf (aref (krma-select-box-2d-memory-resources window) current-frame)
		    new-2d-memory-resource)
	      (setf (aref (krma-select-box-3d-memory-resources window) current-frame)
		    new-3d-memory-resource)
		  
	      ;; create a new descriptor set for new memory resource, offset and range have changed
	      (setf (aref (krma-select-boxes-descriptor-sets window) current-frame)
		    (create-descriptor-set
		     (default-logical-device display)
		     (list (krma-select-boxes-descriptor-set-layout display))
		     (default-descriptor-pool display)
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
	    (aref (krma-select-boxes-descriptor-sets window) current-frame))))))

(defun read-select-boxes (window frame-to-read)
  (let* ((cols 1 #+NOMORE(floor (- (krma-select-box-x1 window) (krma-select-box-x0 window))))
	 (rows 1 #+NOMORE(floor (- (krma-select-box-y1 window) (krma-select-box-y0 window)))))
    
    (when (aref (krma-select-box-2d-memory-resources window) frame-to-read)
    
      (let* ((size (* cols rows +select-box-2d-depth+))
	     (size-in-bytes (* size (foreign-type-size :unsigned-int)))
	     (aligned-size (aligned-size size-in-bytes)))

	(read-buffer (vk::memory-resource-buffer
		      (aref (krma-select-box-2d-memory-resources window) frame-to-read))
		     (array-displacement (krma-select-box-2d window)) size-in-bytes
		     (aref (krma-select-box-2d-memory-resources window) frame-to-read)
		     aligned-size)
      
	(clear-buffer (vk::memory-resource-buffer
		       (aref (krma-select-box-2d-memory-resources window) frame-to-read))
		      0 aligned-size
		      (aref (krma-select-box-2d-memory-resources window) frame-to-read))))

    (when (aref (krma-select-box-3d-memory-resources window) frame-to-read)

      (let* ((size (* cols rows +select-box-3d-depth+))
	     (size-in-bytes (* size (foreign-type-size :unsigned-int)))
	     (aligned-size (aligned-size size-in-bytes)))
	
	(read-buffer (vk::memory-resource-buffer
		      (aref (krma-select-box-3d-memory-resources window) frame-to-read))
		     (array-displacement (krma-select-box-3d window)) size-in-bytes
		     (aref (krma-select-box-3d-memory-resources window) frame-to-read)
		     aligned-size)

	(clear-buffer (vk::memory-resource-buffer
		       (aref (krma-select-box-3d-memory-resources window) frame-to-read))
		      0 aligned-size
		      (aref (krma-select-box-3d-memory-resources window) frame-to-read))))))

(defun erase-draw-list (draw-list)
  (declare (type draw-list-mixin draw-list))
  (setf (foreign-array-fill-pointer (draw-list-index-array draw-list)) 0)
  (setf (foreign-array-fill-pointer (draw-list-vertex-array draw-list)) 0)
  (setf (fill-pointer (draw-list-cmd-vector draw-list)) 0))

(defun erase-immediate-mode-draw-data (dpy scene)
  (let* ((draw-data (im-draw-data scene)))
    (let ((combinations-1 (3d-cmd-oriented-combinations (krma-pipeline-store dpy) draw-data dpy))
	  (combinations-2 (3d-draw-list-oriented-combinations (krma-pipeline-store dpy) draw-data dpy))
	  (combinations-3 (2d-cmd-oriented-combinations (krma-pipeline-store dpy) draw-data dpy))
	  (combinations-4 (2d-draw-list-oriented-combinations (krma-pipeline-store dpy) draw-data dpy)))

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
  (let ((f (immediate-mode-work-function-5 dpy)))
    (when f (funcall f)))
  (let ((f (immediate-mode-work-function-4 dpy)))
    (when f (funcall f)))
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

	       (maybe-defer-debug (dpy)
		 (render-scene scene
			       window
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
      #+NIL
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

    ;;(print (clui::window-keys (main-window (first (display-frame-managers dpy)))))
    
    ;; maybe create new descriptor set if select box size has changed
    (maybe-defer-debug (dpy)
      ;; probably going to need a select box per framebuffer
      (do ((window (clui::display-window-list-head dpy) (clui::window-next window)))
	  ((null window))
	(allocate-selection-set-tables window frame-count (car current-frame-cons))
	(compute-select-boxes-descriptor-set window frame-count (car current-frame-cons))))


    
    

    ;;(print (krma-select-box-2d dpy))
    
    ;; This loop takes all the scenes in all the applications
    ;; and updates the portion of the scene's draw-lists
    ;; which are not currently in use, that is,
    ;; not being copied to gpu memory, or not being compacted.
    ;; It is a candidate for parallelization.
    
    

    ;; This loop is a candidate for parallelization
    ;; It processes each scene on each application
    ;; in the context of a window.
    ;; Todo: if the scene does not appear in the window, by means of comparing clip coordinates,
    ;; then it is not processed for that window
    (let ((once nil))
      (do ((window (clui::display-window-list-head dpy) (clui::window-next window)))
	  ((null window))
	
	(recreate-swapchain-when-necessary window)
      
	(with-slots (queue command-pool) window

	  ;; todo: extract out wait-for-fences from frame-begin in cl-vulkan
	  ;; and call it here instead of queuewaitidle.  it won't then be necessary in frame begin, but could just
	  ;; leave it there since it is harmless, but needed when wait-for-fences is otherwise not called

	  (let* ((swapchain (swapchain window))
		 (previous-frame-number (mod (1- current-frame) (number-of-images swapchain))))
	    ;; make sure the previous frame is done being processed before altering it's draw lists
	    (vk::wait-for-fence swapchain previous-frame-number)
	    
	    (maybe-defer-debug (dpy)
	      (read-select-boxes window previous-frame-number))
	    
	    (maybe-defer-debug (dpy)
	      (read-selection-set window (number-of-images swapchain) previous-frame-number))
	    
	    )

	  ;;(print (krma-selection-set-table window))
	  ;;(print (krma-selection-set-buckets window))
		  
	  (loop for app in (display-frame-managers dpy)
		do (loop for scene in (active-scenes app)
			 do (before-frame-begin dpy scene current-draw-data)))
	  
	  (maybe-defer-debug (dpy)
	    (call-immediate-mode-work-functions dpy))
	
	  (let* ((swapchain (swapchain window))
		 (frame-resource (elt (frame-resources swapchain) current-frame))
		 (command-buffer (frame-command-buffer frame-resource)))
	  
	    (vector-push-extend
	     (frame-begin swapchain (render-pass window)
			  current-frame (clear-value window)
			  command-pool)
	     image-indices)

	    (during-frame dpy window command-buffer current-draw-data show-frame-rate?)))))
    
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

    ;; first time use of compacting complete semaphore is :count 1
    ;; this needs to be the only thread that modifies current-frame
    (update-counts (current-frame-cons dpy) (current-draw-data-cons dpy) (number-of-images (swapchain (main-window (first (display-frame-managers dpy))))))
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
       (loop for app in (display-frame-managers dpy)
	     do (setf active-scenes (nconc active-scenes (active-scenes app))))
       (compactor-thread-iteration dpy active-scenes))
     ;; todo: make close button on window setf application-exit? to t.
     (when (clui::run-loop-exit? dpy)
       (go exit))
     (go again)
   exit))

(defun start-compactor-thread (dpy)
  (setf (compactor-thread dpy)
	(bt:make-thread #'(lambda ()
			    (compactor-loop dpy))
			:name "draw-list-compactor-thread")))

(defvar *threshold* 0.008)
(defvar *test* 1290)

#+(and cocoa noglfw)
(defun krma-main (app &rest args &key (show-frame-rate? t) &allow-other-keys)
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
(defun krma-main (app &rest args &key (show-frame-rate? t) &allow-other-keys)
  (declare (ignorable args))

  (let* ((main-window (main-window app))
	 (dpy (clui::window-display main-window)))
    
    (setf (window-show-frame-rate? main-window) show-frame-rate?)

    (unwind-protect
	 (loop until (clui::run-loop-exit? dpy)
		 initially (start-compactor-thread dpy)
	       do (maybe-defer-debug (dpy)
		    (poll-events dpy))

		  (when (clui::run-loop-exit? dpy)
		    (return))
		  
		  (maybe-defer-debug (dpy)
		    (update-frame-rate main-window))
		  
		  (maybe-defer-debug (dpy)
		    (frame-iteration dpy (number-of-images (swapchain main-window)) show-frame-rate?)))
      
      (shutdown-run-loop dpy))))
    

(defgeneric main (frame-manager &rest args &key &allow-other-keys)
  (:documentation "Define your own main function for your custom frame-manager if necessary."))


(defmethod main ((frame-manager krma-frame-manager-mixin) &rest args &key &allow-other-keys)
  (apply #'krma-main frame-manager args))

