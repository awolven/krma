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

(defun clear-buffer (buffer value aligned-size memory-block)
  (let ((memory (allocated-memory buffer))
	(offset (memory-block-offset memory-block))
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
  (declare (ignorable frame-count))
  (read-buffer (memory-block-buffer
		(aref (krma-selection-set-table-memory-blocks window) frame-to-read))
	       (krma-selection-set-table window) (* 4 1024)
	       (aref (krma-selection-set-table-memory-blocks window) frame-to-read)
	       (* 4 1024))
  #+NIL
  (read-buffer (memory-block-buffer
		(aref (krma-selection-set-buckets-memory-blocks window) frame-to-read))
	       (krma-selection-set-buckets window) (* 4 32 1024)
	       (aref (krma-selection-set-buckets-memory-blocks window) frame-to-read)
	       (* 4 32 1024))
  #+NIL
  (read-buffer (memory-block-buffer
		(krma-selection-set-counter-memory-block window))
	       (krma-selection-set-counters window) (* frame-count 4)
	       (krma-selection-set-counter-memory-block window) 512))
  

(defun allocate-selection-set-tables (window frame-count current-frame)
  (let* ((display (clui:window-display window))
	 (device (default-logical-device display)))

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

    (unless (krma-selection-set-counter-memory-block window)
      ;; minimum aligned size for 8 bytes X num-frames
      (setf (krma-selection-set-counter-memory-block window)
	    (acquire-memory-sized device 512 VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT)))

    (unless (aref (krma-selection-set-counter-pointers window) current-frame)
      (setf (aref (krma-selection-set-counter-pointers window) current-frame)
	    (let ((mr (krma-selection-set-counter-memory-block window)))
	      (%vk::with-vkBufferDeviceAddressInfo (p-info)
		(setf %vk::buffer (h (memory-block-buffer mr)))
		(+ (* 8 current-frame)
		   (memory-block-offset mr) (%vk::vkGetBufferDeviceAddress (h device) p-info))))))
    
    (unless (krma-selection-set-buckets-memory-blocks window)
      (setf (krma-selection-set-buckets-memory-blocks window) (make-array frame-count :initial-element nil)))

    (unless (aref (krma-selection-set-buckets-memory-blocks window) current-frame)
      (setf (aref (krma-selection-set-buckets-memory-blocks window) current-frame)
	    (acquire-memory-sized device (* 4 32 1024) VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT)))

    (unless (aref (krma-selection-set-buckets-pointers window) current-frame)
      (setf (aref (krma-selection-set-buckets-pointers window) current-frame)
	    (let ((mr (aref (krma-selection-set-buckets-memory-blocks window) current-frame)))
	      (%vk::with-vkBufferDeviceAddressInfo (p-info)
		(setf %vk::buffer (h (memory-block-buffer mr)))
		(+ (memory-block-offset mr) (%vk::vkGetBufferDeviceAddress (h device) p-info))))))
        
    (unless (krma-selection-set-table-memory-blocks window)
      (setf (krma-selection-set-table-memory-blocks window) (make-array frame-count :initial-element nil)))

    (unless (aref (krma-selection-set-table-memory-blocks window) current-frame)
      (setf (aref (krma-selection-set-table-memory-blocks window) current-frame)
	    (acquire-memory-sized device (* 4 1024) VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT)))

    (unless (aref (krma-selection-set-table-pointers window) current-frame)
      (setf (aref (krma-selection-set-table-pointers window) current-frame)
	    (let ((mr (aref (krma-selection-set-table-memory-blocks window) current-frame)))
	      (%vk::with-vkBufferDeviceAddressInfo (p-info)
		(setf %vk::buffer (h (memory-block-buffer mr)))
		(+ (memory-block-offset mr) (%vk::vkGetBufferDeviceAddress (h device) p-info))))))

    (clear-buffer (memory-block-buffer
		   (aref (krma-selection-set-buckets-memory-blocks window) current-frame))
		  0 (* 4 32 1024)
		  (aref (krma-selection-set-buckets-memory-blocks window) current-frame))

    (clear-buffer (memory-block-buffer
		   (aref (krma-selection-set-table-memory-blocks window) current-frame))
		  0 (* 4 1024)
		  (aref (krma-selection-set-table-memory-blocks window) current-frame))

    (clear-buffer (memory-block-buffer
		   (krma-selection-set-counter-memory-block window))
		  0 512
		  (krma-selection-set-counter-memory-block window))

    (values)))

      

    
    
  


(defun compute-select-boxes-descriptor-set (window frame-count current-frame)
  (let* (#+NOMORE(width (abs (round (- (krma-select-box-x1 window) (krma-select-box-x0 window)))))
	 #+NOMORE(height (abs (round (- (krma-select-box-y1 window) (krma-select-box-y0 window)))))
	 (width 1)
	 (height 1)
	 (new-2d-size (* width height +select-box-2d-depth+ (load-time-value (foreign-type-size :unsigned-int))))
	 (new-3d-size (* width height +select-box-3d-depth+ (load-time-value (foreign-type-size :unsigned-int))))
	 (display (clui:window-display window))
	 (device (default-logical-device display)))

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

    (unless (krma-select-box-2d-memory-blocks window)
      (setf (krma-select-box-2d-memory-blocks window) (make-array frame-count :initial-element nil)))

    (unless (krma-select-box-3d-memory-blocks window)
      (setf (krma-select-box-3d-memory-blocks window) (make-array frame-count :initial-element nil)))

    (let ((aligned-size-2d (aligned-size new-2d-size))
	  (aligned-size-3d (aligned-size new-3d-size))
	  (old-descriptor-set (aref (krma-select-boxes-descriptor-sets window) current-frame))
	  (old-2d-memory-block (aref (krma-select-box-2d-memory-blocks window) current-frame))
	  (old-3d-memory-block (aref (krma-select-box-3d-memory-blocks window) current-frame))
	  (new-2d-memory-block)
	  (new-3d-memory-block)
	  (memory-block-changed-p nil))

      (if old-2d-memory-block
	      
	  (if (<= aligned-size-2d (memory-block-size old-2d-memory-block))
		  
	      (setq new-2d-memory-block old-2d-memory-block) ;; keep resource the same
		  
	      (progn
		(release-memory old-2d-memory-block)
		(setq new-2d-memory-block (acquire-memory-sized device aligned-size-2d VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT))
		(setq memory-block-changed-p t)))
	      
	  (progn
	    (setq new-2d-memory-block (acquire-memory-sized device aligned-size-2d VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT))
	    (setq memory-block-changed-p t)))

      (if old-3d-memory-block
	      
	  (if (<= aligned-size-3d (memory-block-size old-3d-memory-block))
		  
	      (setq new-3d-memory-block old-3d-memory-block) ;; keep resource the same
		  
	      (progn
		(release-memory old-3d-memory-block)
		(setq new-3d-memory-block (acquire-memory-sized device aligned-size-3d VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT))
		(setq memory-block-changed-p t)))
	      
	  (progn
	    (setq new-3d-memory-block (acquire-memory-sized device aligned-size-3d VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT))
	    (setq memory-block-changed-p t)))

      (let ((buffer-2d (memory-block-buffer new-2d-memory-block))
	    (buffer-3d (memory-block-buffer new-3d-memory-block)))
	  
	(if memory-block-changed-p
		
	    (progn
	      (clear-buffer buffer-2d 0 aligned-size-2d new-2d-memory-block)
	      (clear-buffer buffer-3d 0 aligned-size-3d new-3d-memory-block)
		  
	      (when old-descriptor-set
		(vk::free-descriptor-sets (list old-descriptor-set) (default-descriptor-pool device)))
		  
	      (setf (aref (krma-select-box-2d-memory-blocks window) current-frame)
		    new-2d-memory-block)
	      (setf (aref (krma-select-box-3d-memory-blocks window) current-frame)
		    new-3d-memory-block)
		  
	      ;; create a new descriptor set for new memory resource, offset and range have changed
	      (setf (aref (krma-select-boxes-descriptor-sets window) current-frame)
		    (create-descriptor-set
		     device
		     (list (krma-select-boxes-descriptor-set-layout display))
		     (default-descriptor-pool device)
		     :descriptor-buffer-info (list (make-instance 'descriptor-storage-buffer-info
								  :buffer buffer-2d
								  :offset (memory-block-offset new-2d-memory-block)
								  :range new-2d-size)
						   (make-instance 'descriptor-storage-buffer-info
								  :buffer buffer-3d
								  :offset (memory-block-offset new-3d-memory-block)
								  :range new-3d-size)))))

	    ;; otherwise return existing descriptor set
	    ;; if the mouse doesn't move the descriptor set doesn't change
	    (aref (krma-select-boxes-descriptor-sets window) current-frame))))))

(defun read-select-boxes (window frame-to-read)
  (let* ((cols 1 #+NOMORE(floor (- (krma-select-box-x1 window) (krma-select-box-x0 window))))
	 (rows 1 #+NOMORE(floor (- (krma-select-box-y1 window) (krma-select-box-y0 window)))))
    
    (when (aref (krma-select-box-2d-memory-blocks window) frame-to-read)
    
      (let* ((size (* cols rows +select-box-2d-depth+))
	     (size-in-bytes (* size (foreign-type-size :unsigned-int)))
	     (aligned-size (aligned-size size-in-bytes)))

	(read-buffer (memory-block-buffer
		      (aref (krma-select-box-2d-memory-blocks window) frame-to-read))
		     (array-displacement (krma-select-box-2d window)) size-in-bytes
		     (aref (krma-select-box-2d-memory-blocks window) frame-to-read)
		     aligned-size)
      
	(clear-buffer (memory-block-buffer
		       (aref (krma-select-box-2d-memory-blocks window) frame-to-read))
		      0 aligned-size
		      (aref (krma-select-box-2d-memory-blocks window) frame-to-read))))

    (when (aref (krma-select-box-3d-memory-blocks window) frame-to-read)

      (let* ((size (* cols rows +select-box-3d-depth+))
	     (size-in-bytes (* size (foreign-type-size :unsigned-int)))
	     (aligned-size (aligned-size size-in-bytes)))
	
	(read-buffer (memory-block-buffer
		      (aref (krma-select-box-3d-memory-blocks window) frame-to-read))
		     (array-displacement (krma-select-box-3d window)) size-in-bytes
		     (aref (krma-select-box-3d-memory-blocks window) frame-to-read)
		     aligned-size)

	(clear-buffer (memory-block-buffer
		       (aref (krma-select-box-3d-memory-blocks window) frame-to-read))
		      0 aligned-size
		      (aref (krma-select-box-3d-memory-blocks window) frame-to-read))))))

(defun erase-draw-list (draw-list)
  (declare (type draw-list-mixin draw-list))
  (setf (foreign-array-fill-pointer (draw-list-index-array draw-list)) 0)
  (setf (foreign-array-fill-pointer (draw-list-vertex-array draw-list)) 0)
  (setf (fill-pointer (draw-list-cmd-vector draw-list)) 0))

(defun erase-immediate-mode-draw-data (dpy scene)
  (let* ((draw-data (im-draw-data scene)))
    (let ((combinations-1 (im-standard-3d-cmd-oriented-combinations (krma-pipeline-store dpy) draw-data dpy))
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
  (let ((f (immediate-mode-work-function-6 dpy)))
    (when f (funcall f)))
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
	       (if scene
		   (maybe-defer-debug (dpy)
		     (render-scene scene
				   window
				   viewport
				   dpy command-buffer
				   (aref (rm-draw-data scene) current-draw-data-index)
				   (im-draw-data scene)
				   (aref (releaseme-queues dpy) current-draw-data-index)))
		   (maybe-defer-debug (dpy)
		     (vkCmdNextSubpass (h command-buffer) VK_SUBPASS_CONTENTS_INLINE)))))

     
    
    (values)))

(defvar *frame-initialized* nil)

(defun update-counts (current-frame-cons current-draw-data-cons frame-count)
  #+sbcl(sb-ext:atomic-update (car current-frame-cons)
			      #'(lambda (cf) (mod (1+ cf) frame-count)))
  #-sbcl(setf (car current-frame-cons) (mod (1+ (car current-frame-cons)) frame-count))
  #+sbcl(sb-ext:atomic-update (car current-draw-data-cons)
			      #'(lambda (cdd) (mod (1+ cdd) 2)))
  #-sbcl(setf (car current-draw-data-cons) (mod (1+ (car current-draw-data-cons)) 2))
  (values))

(defun recreate-swapchain-when-necessary (window device)
  (when (recreate-swapchain? window)
    #+LINUX(sleep 0.02);;https://github.com/KhronosGroup/Vulkan-Samples/issues/250
    (multiple-value-bind (width height) (window-framebuffer-size window)
      (recreate-swapchain window device (render-pass window) (swapchain window) width height)
      (setf (recreate-swapchain? window) nil)))
  (values))

(defun maybe-destroy-old-swapchain (window)
  (let ((old-swapchain (vk::swapchain-recreated? window)))
    (when old-swapchain
      (destroy-swapchain old-swapchain)
      (setf (vk::swapchain-recreated? window) nil))))

(defvar *wait-time* 1)

(defun frame-iteration (dpy frame-count show-frame-rate?)
  
  (let* ((current-frame-cons (current-frame-cons dpy))
	 (current-draw-data-cons (current-draw-data-cons dpy))
	 (current-frame (car current-frame-cons))
	 (current-draw-data (car current-draw-data-cons))
	 (device (default-logical-device dpy)))

    (do ((window (clui::display-window-list-head dpy) (clui::window-next window)))
	((null window))
      
      (maybe-defer-debug (dpy)
	(allocate-selection-set-tables window frame-count current-frame)
	(compute-select-boxes-descriptor-set window frame-count current-frame)))

    (loop for app in (display-frame-managers dpy)
	  do (loop for scene in (active-scenes app)
		   do (before-frame-begin dpy scene current-draw-data)))
    
    (maybe-defer-debug (dpy)
      (call-immediate-mode-work-functions dpy))
    
    ;; maybe create new descriptor set if select box size has changed
    ;; probably going to need a select box per framebuffer
    (do ((window (clui::display-window-list-head dpy) (clui::window-next window)))
	((null window))

      (when (vk::window-initialized? window)
	(maybe-defer-debug (dpy)
	  (maybe-destroy-old-swapchain window))
      
	(maybe-defer-debug (dpy)
	  (recreate-swapchain-when-necessary window device)))
      
      (maybe-defer-debug (dpy)
	(let ((frame-resources (frame-resources window)))
	  (vk::wait-for-fence device frame-resources current-frame))))
    
    (bt:wait-on-semaphore (compacting-complete-semaphore dpy))
    (bt:signal-semaphore (frame-iteration-complete-semaphore dpy))
    
    (do ((window (clui::display-window-list-head dpy) (clui::window-next window)))
	((null window))
      
      (when (vk::window-initialized? window)
	
	(multiple-value-bind (w h) (window-framebuffer-size window)

	  (if (or (= 0 w) (= 0 h))
	      (sleep 0.016)

	      (let ((frame-resources (frame-resources window)))
      
		(with-slots (queue command-pool) window
	
		  (let* ((swapchain (swapchain window))
			 (frame-resource (elt frame-resources current-frame))
			 (command-buffer (frame-command-buffer frame-resource)))
	  
		    (let ((image-index
			    (frame-begin swapchain frame-resource
					 (render-pass window)
					 (clear-value window)
					 command-pool)))

		      (during-frame dpy window command-buffer current-draw-data show-frame-rate?)
		      
			
		      (frame-end device queue frame-resource)
	      
		      (frame-present swapchain frame-resource queue image-index window)))))))))
    
    (setq *frame-initialized* t)

    (do ((window (clui::display-window-list-head dpy) (clui::window-next window)))
	  ((null window))
      ;; probably going to need a select box per framebuffer
      (when *frame-initialized*
	(let* ((number-of-images (number-of-images (swapchain window))))
	  (maybe-defer-debug (dpy)
	    (read-select-boxes window current-frame))
	  (maybe-defer-debug (dpy)
	    (read-selection-set window number-of-images current-frame)))))

    (update-counts (current-frame-cons dpy) (current-draw-data-cons dpy) (number-of-images (swapchain (main-window (first (display-frame-managers dpy)))))))
  
  (values))

(defun compactor-thread-iteration (dpy active-scenes)
  (bt:wait-on-semaphore (frame-iteration-complete-semaphore dpy))
  (let* ((current-draw-data-cons (current-draw-data-cons dpy))
	 (alt-index (mod (1+ (car current-draw-data-cons)) 2))
	 (releaseme-queue (aref (releaseme-queues dpy) alt-index)))

    
    (maybe-defer-debug (dpy)
      (loop with work = nil
	    while (setq work (and (lparallel.queue:peek-queue releaseme-queue)
				  (lparallel.queue:pop-queue releaseme-queue)))
	    do (funcall work)))
    
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

#+cocoa
(defun krma-main (frame-manager &rest args &key (show-frame-rate? t) &allow-other-keys)
  (declare (ignore args))
  (let* ((main-window (main-window frame-manager))
	 (dpy (clui::window-display main-window)))
    
    (setf (window-show-frame-rate? main-window) show-frame-rate?)

    ;; cocoa uses an ApplicationDidFinishLaunching callback
    ;; we use this callback to start the compactor thread
    ;; a clui::application-did-finish-launching method is defined in clui-support

    ;; In clui, in the default resize-event handler, clui::initialize-window-devices is called.
    ;; A method of clui::initialize-window-devices is defined
    ;; in application-mixin.lisp of krma/ specializing on vulkan windows.
    ;; For cocoa, no other calls to initialize-window-devices is required.
    
    (unwind-protect (ns::|run| dpy)
      (shutdown-run-loop dpy))))



#+cocoa
(defmethod clui::content-view-draw-rect ((window vk::vulkan-window-mixin) view rect)
  (declare (ignore view rect))
  (with-slots ((dpy clui::display) queue command-pool) window
    (maybe-defer-debug (dpy)
      (update-frame-rate window))
    (maybe-defer-debug (dpy)
      (frame-iteration dpy (number-of-images (swapchain window)) (window-show-frame-rate? window)))))

#+cocoa
(defmethod clui::content-view-draw-rect (window view rect)
  (values))

#-cocoa
(defun krma-main (frame-manager &rest args &key (show-frame-rate? t) &allow-other-keys)
  (declare (ignorable args))

  (let* ((main-window (main-window frame-manager))
	 (dpy (clui::window-display main-window)))
    
    (setf (window-show-frame-rate? main-window) show-frame-rate?)

    (unwind-protect
	 (loop until (clui::run-loop-exit? dpy)
	       initially (start-compactor-thread dpy)
			 (maybe-defer-debug (dpy)
			   (poll-events dpy))
			 
			 (unless (render-surface main-window)
			   ;; initial poll-events should trigger a resize event
			   ;; which calls initialize-window-devices
			   ;; which calls create-swapchain
			   ;; but if for some reason that event does not get triggered
			   ;; do the work here:
			   (multiple-value-bind (w h) (window-framebuffer-size main-window)
			     (clui::initialize-window-devices main-window
							      :width w
							      :height h)
			     (setf (recreate-swapchain? main-window) nil)
			     (setf (vk::window-initialized? main-window) t)))
			 
	       do (maybe-defer-debug (dpy)
		    (poll-events dpy))
		  
		  (when (clui::run-loop-exit? dpy)
		    (return))

		  (maybe-defer-debug (dpy)
		    (update-frame-rate main-window))
		  
		  (maybe-defer-debug (dpy)
		    (frame-iteration dpy (number-of-images (swapchain main-window)) show-frame-rate?)))
      
      (shutdown-run-loop dpy))))

(defmethod clui:main ((frame-manager krma-frame-manager-mixin) &rest args &key &allow-other-keys)
  (apply #'krma-main frame-manager args))

