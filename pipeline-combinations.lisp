(in-package :krma)

(defmethod 3d-cmd-oriented-combinations ((pipeline-store pipeline-store-mixin) (draw-data retained-mode-draw-data) display)
  (rm-standard-3d-cmd-oriented-combinations pipeline-store draw-data display))

(defmethod rm-standard-3d-cmd-oriented-combinations (pipeline-store draw-data display)
  (declare (type pipeline-store-mixin pipeline-store))
  (declare (type retained-mode-draw-data draw-data))
  
  (list (or (pipeline-store-3d-triangle-list-with-normals-pipeline pipeline-store)
	    (setf (pipeline-store-3d-triangle-list-with-normals-pipeline pipeline-store)
		  (make-instance '3d-triangle-list-with-normals-pipeline
				 :dpy display
				 :name :3d-triangle-list-with-normals-pipeline
				 :subpass 0)))
	(rm-draw-data-3d-triangle-list-with-normals-draw-list draw-data)

	(or (pipeline-store-3d-triangle-strip-with-normals-pipeline pipeline-store)
	    (setf (pipeline-store-3d-triangle-strip-with-normals-pipeline pipeline-store)
		  (make-instance '3d-triangle-strip-with-normals-pipeline
				 :dpy display
				 :name :3d-triangle-strip-with-normals-pipeline
				 :subpass 0)))
	(rm-draw-data-3d-triangle-strip-with-normals-draw-list draw-data)

	(or (pipeline-store-3d-triangle-list-pipeline pipeline-store)
	    (setf (pipeline-store-3d-triangle-list-pipeline pipeline-store)
		  (make-instance '3d-triangle-list-pipeline
				 :dpy display
				 :name :3d-triangle-list-pipeline
				 :subpass 0)))
	(rm-draw-data-3d-triangle-list-draw-list draw-data)

	(or (pipeline-store-3d-triangle-strip-pipeline pipeline-store)
	    (setf (pipeline-store-3d-triangle-strip-pipeline pipeline-store)
		  (make-instance '3d-triangle-strip-pipeline
				 :dpy display
				 :name :3d-triangle-strip-pipeline
				 :subpass 0)))		  
	(rm-draw-data-3d-triangle-strip-draw-list draw-data)

	(or (pipeline-store-3d-instanced-tube-pipeline pipeline-store)
	    (setf (pipeline-store-3d-instanced-tube-pipeline pipeline-store)
		  (make-instance '3d-instanced-tube-pipeline
				 :dpy display
				 :name :3d-instanced-tube-pipeline
				 :subpass 0)))
	(rm-draw-data-3d-instanced-tube-draw-list draw-data)

	(or (pipeline-store-3d-line-strip-pipeline pipeline-store)
	    (setf (pipeline-store-3d-line-strip-pipeline pipeline-store)
		  (make-instance '3d-line-strip-pipeline
				 :dpy display
				 :name :3d-line-strip-pipeline
				 :subpass 0)))
	(rm-draw-data-3d-line-strip-draw-list draw-data)
	
	(or (pipeline-store-3d-line-list-pipeline pipeline-store)
	    (setf (pipeline-store-3d-line-list-pipeline pipeline-store)
		  (make-instance '3d-line-list-pipeline
				 :dpy display
				 :name :3d-line-list-pipeline
				 :subpass 0)))
	(rm-draw-data-3d-line-list-draw-list draw-data)

	(or (pipeline-store-3d-point-list-pipeline pipeline-store)
	    (setf (pipeline-store-3d-point-list-pipeline pipeline-store)
		  (make-instance '3d-point-list-pipeline
				 :dpy display
				 :name :3d-point-list-pipeline
				 :subpass 0)))
	(rm-draw-data-3d-point-list-draw-list draw-data)))

(defmethod 3d-draw-list-oriented-combinations ((pipeline-store pipeline-store-mixin)
					       (draw-data retained-mode-draw-data) display)
  (rm-standard-3d-draw-list-oriented-combinations pipeline-store draw-data display))

(defun rm-standard-3d-draw-list-oriented-combinations (pipeline-store draw-data display)
  (declare (type pipeline-store-mixin pipeline-store))
  (declare (type retained-mode-draw-data draw-data))

  (let ((res ())
	(point-pipeline (or (pipeline-store-3d-point-list-pipeline pipeline-store)
			    (setf (pipeline-store-3d-point-list-pipeline pipeline-store)
				  (make-instance '3d-point-list-pipeline
						 :dpy display
						 :name :3d-point-list-pipeline
						 :subpass 0))))
	(line-pipeline (or (pipeline-store-3d-line-list-pipeline pipeline-store)
			   (setf (pipeline-store-3d-line-list-pipeline pipeline-store)
				 (make-instance '3d-line-list-pipeline
						:dpy display
						:name :3d-line-list-pipeline
						:subpass 0))))
	(triangle-pipeline (or (pipeline-store-3d-triangle-list-pipeline pipeline-store)
			       (setf (pipeline-store-3d-triangle-list-pipeline pipeline-store)
				     (make-instance '3d-triangle-list-pipeline
						    :dpy display
						    :name :3d-triangle-list-pipeline
						    :subpass 0))))
	(normal-pipeline (or (pipeline-store-3d-triangle-list-with-normals-pipeline pipeline-store)
			     (setf (pipeline-store-3d-triangle-list-with-normals-pipeline pipeline-store)
				   (make-instance '3d-triangle-list-with-normals-pipeline
						  :dpy display
						  :name :3d-triangle-list-with-normals-pipeline
						  :subpass 0)))))

    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push point-pipeline res))
	     (draw-data-3d-point-list-draw-list-table draw-data))

    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push line-pipeline res))
	     (draw-data-3d-line-list-draw-list-table draw-data))

    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push triangle-pipeline res))
	     (draw-data-3d-triangle-list-draw-list-table draw-data))

    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push normal-pipeline res))
	     (draw-data-3d-triangle-list-with-normals-draw-list-table draw-data))
    res))	

(defmethod 2d-cmd-oriented-combinations ((pipeline-store pipeline-store-mixin) (draw-data retained-mode-draw-data) display)
  (rm-standard-2d-cmd-oriented-combinations pipeline-store draw-data display))

(defun rm-standard-2d-cmd-oriented-combinations (pipeline-store draw-data display)
  (declare (type pipeline-store-mixin pipeline-store))
  (declare (type retained-mode-draw-data draw-data))

  ;; 2d is currently using depth testing, so order does not matter
  ;; but for in case that the depth testing is turned off, the order
  ;; of the list is so the triangles are drawn first, followed by 
  ;; lines followed by points, followed by text
  ;; the last drawn will be on top when depth testing is turned off

  ;; the last listed is the last thing drawn in render operations
  
  (list (or (pipeline-store-2d-triangle-list-pipeline pipeline-store)
	    (setf (pipeline-store-2d-triangle-list-pipeline pipeline-store)
		  (make-instance '2d-triangle-list-pipeline
				 :dpy display
				 :name :2d-triangle-list-pipeline
				 :subpass 1)))
	(rm-draw-data-2d-triangle-list-draw-list draw-data)

	(or (pipeline-store-2d-triangle-strip-pipeline pipeline-store)
	    (setf (pipeline-store-2d-triangle-strip-pipeline pipeline-store)
		  (make-instance '2d-triangle-strip-pipeline
				 :dpy display
				 :name :2d-triangle-strip-pipeline
				 :subpass 1)))
	(rm-draw-data-2d-triangle-strip-draw-list draw-data)

	(or (pipeline-store-2d-instanced-line-pipeline pipeline-store)
	    (setf (pipeline-store-2d-instanced-line-pipeline pipeline-store)
		  (make-instance '2d-instanced-line-pipeline
				 :dpy display
				 :name :2d-instanced-line-pipeline
				 :subpass 1)))				 
	(rm-draw-data-2d-instanced-line-draw-list draw-data)

	(or (pipeline-store-2d-line-strip-pipeline pipeline-store)
	    (setf (pipeline-store-2d-line-strip-pipeline pipeline-store)
		  (make-instance '2d-line-strip-pipeline
				 :dpy display
				 :name :2d-line-strip-pipeline
				 :subpass 1)))
	(rm-draw-data-2d-line-strip-draw-list draw-data)

	(or (pipeline-store-2d-line-list-pipeline pipeline-store)
	    (setf (pipeline-store-2d-line-list-pipeline pipeline-store)
		  (make-instance '2d-line-list-pipeline
				 :dpy display
				 :name :2d-line-list-pipeline
				 :subpass 1)))				 
	(rm-draw-data-2d-line-list-draw-list draw-data)

	(or (pipeline-store-2d-point-list-pipeline pipeline-store)
	    (setf (pipeline-store-2d-point-list-pipeline pipeline-store)
		  (make-instance '2d-point-list-pipeline
				 :dpy display
				 :name :2d-point-list-pipeline
				 :subpass 1)))
	(rm-draw-data-2d-point-list-draw-list draw-data)

	#+NOMORE(pipeline-store-msdf-text-pipeline pipeline-store)
	#+NOMORE(draw-data-2d-triangle-list-draw-list-for-text draw-data)))


(defmethod 2d-draw-list-oriented-combinations ((pipeline-store pipeline-store-mixin)
					       (draw-data retained-mode-draw-data) display)
  (rm-standard-2d-draw-list-oriented-combinations pipeline-store draw-data display))

(defun rm-standard-2d-draw-list-oriented-combinations (pipeline-store draw-data display)
  (declare (type pipeline-store-mixin pipeline-store))
  (declare (type retained-mode-draw-data draw-data))
  
  (let ((res ())
	(point-pipeline (or (pipeline-store-2d-point-list-pipeline pipeline-store)
			    (setf (pipeline-store-2d-point-list-pipeline pipeline-store)
				  (make-instance '2d-point-list-pipeline
						 :dpy display
						 :name :2d-point-list-pipeline
						 :subpass 1))))
	(line-pipeline (or (pipeline-store-2d-line-list-pipeline pipeline-store)
			   (setf (pipeline-store-2d-line-list-pipeline pipeline-store)
				 (make-instance '2d-line-list-pipeline
						:dpy display
						:name :2d-line-list-pipeline
						:subpass 1))))
	(triangle-pipeline (or (pipeline-store-2d-triangle-list-pipeline pipeline-store)
			       (setf (pipeline-store-2d-triangle-list-pipeline pipeline-store)
				     (make-instance '2d-triangle-list-pipeline
						    :dpy display
						    :name :2d-triangle-list-pipeline
						    :subpass 1))))
	#+NOMORE
	(text-pipeline (pipeline-store-msdf-text-pipeline pipeline-store)))

    ;; 2d is currently using depth testing, so order does not matter
    ;; but for in case the depth testing is turned off, the order of
    ;; the list is so the triangles are drawn first, followed by lines
    ;; followed by points, followed by text
    ;; the last drawn will be on top when depth testing is turned off

    ;; the first pushed is the last thing drawn in render operations

    ;; note that there are no triangle strips for draw-list oriented renders

    #+NOMORE(maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push text-pipeline res))
	     (draw-data-2d-triangle-list-draw-list-for-text-table draw-data))

    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push point-pipeline res))
	     (draw-data-2d-point-list-draw-list-table draw-data))

    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push line-pipeline res))
	     (draw-data-2d-line-list-draw-list-table draw-data))

    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push triangle-pipeline res))
	     (draw-data-2d-triangle-list-draw-list-table draw-data))    
    
    res))

(defmethod 3d-cmd-oriented-combinations ((pipeline-store pipeline-store-mixin) (draw-data immediate-mode-draw-data) display)
  (im-standard-3d-cmd-oriented-combinations pipeline-store draw-data display))

(defun im-standard-3d-cmd-oriented-combinations (pipeline-store draw-data display)
  (declare (type pipeline-store-mixin pipeline-store))
  (declare (type immediate-mode-draw-data draw-data))

  #+NIL
  (let ((res ())
	(point-pipeline (or (pipeline-store-3d-point-list-pipeline pipeline-store)
			    (setf (pipeline-store-3d-point-list-pipeline pipeline-store)
				  (make-instance '3d-point-list-pipeline
						 :dpy display
						 :name :3d-point-list-pipeline
						 :subpass 0))))
	(line-pipeline (or (pipeline-store-3d-line-list-pipeline pipeline-store)
			   (setf (pipeline-store-3d-line-list-pipeline pipeline-store)
				 (make-instance '3d-line-list-pipeline
						:dpy display
						:name :3d-line-list-pipeline
						:subpass 0))))
	(triangle-pipeline (or (pipeline-store-3d-triangle-list-pipeline pipeline-store)
			       (setf (pipeline-store-3d-triangle-list-pipeline pipeline-store)
				     (make-instance '3d-triangle-list-pipeline
						    :dpy display
						    :name :3d-triangle-list-pipeline
						    :subpass 0)))))

    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push point-pipeline res))
	     (draw-data-3d-point-list-draw-list-table draw-data))

    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push line-pipeline res))
	     (draw-data-3d-line-list-draw-list-table draw-data))

    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push triangle-pipeline res))
	     (draw-data-3d-triangle-list-draw-list-table draw-data))

    (push (draw-data-3d-line-strip-draw-list draw-data) res)
    (push (or (pipeline-store-3d-line-strip-pipeline pipeline-store)
	      (setf (pipeline-store-3d-line-strip-pipeline pipeline-store)
		    (make-instance '3d-line-strip-pipeline
				   :dpy display
				   :name :3d-line-strip-pipeline
				   :subpass 0)))
	  res)

    (push (draw-data-3d-triangle-strip-draw-list draw-data) res)
    (push (or (pipeline-store-3d-triangle-strip-pipeline pipeline-store)
	      (setf (pipeline-store-3d-triangle-strip-pipeline pipeline-store)
		    (make-instance '3d-triangle-strip-pipeline
				   :dpy display
				   :name :3d-triangle-strip-pipeline
				   :subpass 0)))
	  res)

    (push (draw-data-3d-triangle-strip-with-normals-draw-list draw-data) res)
    (push (or (pipeline-store-3d-triangle-strip-with-normals-pipeline pipeline-store)
	      (setf (pipeline-store-3d-triangle-strip-with-normals-pipeline pipeline-store)
		    (make-instance '3d-triangle-strip-with-normals-pipeline
				   :dpy display
				   :name :3d-triangle-strip-with-normals-pipeline
				   :subpass 0)))
	  res)

    res))

(defmethod 3d-draw-list-oriented-combinations ((pipeline-store pipeline-store-mixin) (draw-data immediate-mode-draw-data) display)
  (im-standard-3d-draw-list-oriented-combinations pipeline-store draw-data display))

(defun im-standard-3d-draw-list-oriented-combinations (pipeline-store draw-data display)
  (declare (type pipeline-store-mixin pipeline-store))
  (declare (type immediate-mode-draw-data draw-data))

  (let ((res ())
	(point-pipeline (or (pipeline-store-3d-point-list-pipeline pipeline-store)
			    (setf (pipeline-store-3d-point-list-pipeline pipeline-store)
				  (make-instance '3d-point-list-pipeline
						 :dpy display
						 :name :3d-point-list-pipeline
						 :subpass 0))))
	(line-pipeline (or (pipeline-store-3d-line-list-pipeline pipeline-store)
			   (setf (pipeline-store-3d-line-list-pipeline pipeline-store)
				 (make-instance '3d-line-list-pipeline
						:dpy display
						:name :3d-line-list-pipeline
						:subpass 0))))
	(triangle-pipeline (or (pipeline-store-3d-triangle-list-pipeline pipeline-store)
			       (setf (pipeline-store-3d-triangle-list-pipeline pipeline-store)
				     (make-instance '3d-triangle-list-pipeline
						    :dpy display
						    :name :3d-triangle-list-pipeline
						    :subpass 0))))
	(normal-pipeline (or (pipeline-store-3d-triangle-list-with-normals-pipeline pipeline-store)
			     (setf (pipeline-store-3d-triangle-list-with-normals-pipeline pipeline-store)
				   (make-instance '3d-triangle-list-with-normals-pipeline
						  :dpy display
						  :name :3d-triangle-list-with-normals-pipeline
						  :subpass 0)))))


    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push point-pipeline res))
	     (draw-data-3d-point-list-draw-list-table draw-data))


    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push line-pipeline res))
	     (draw-data-3d-line-list-draw-list-table draw-data))


    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push triangle-pipeline res))
	     (draw-data-3d-triangle-list-draw-list-table draw-data))


    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push normal-pipeline res))
	     (draw-data-3d-triangle-list-with-normals-draw-list-table draw-data))

    res))

(defmethod 2d-cmd-oriented-combinations ((pipeline-store pipeline-store-mixin) (draw-data immediate-mode-draw-data) display)
  (im-standard-2d-cmd-oriented-combinations pipeline-store draw-data display))

(defun im-standard-2d-cmd-oriented-combinations (pipeline-store draw-data display)
  (declare (type pipeline-store-mixin pipeline-store))
  (declare (type immediate-mode-draw-data draw-data))

  (let ((res ())
	(point-pipeline (or (pipeline-store-2d-point-list-pipeline pipeline-store)
			    (setf (pipeline-store-2d-point-list-pipeline pipeline-store)
				  (make-instance '2d-point-list-pipeline
						 :dpy display
						 :name :2d-point-list-pipeline
						 :subpass 1))))
	(line-pipeline (or (pipeline-store-2d-line-list-pipeline pipeline-store)
			   (setf (pipeline-store-2d-line-list-pipeline pipeline-store)
				 (make-instance '2d-line-list-pipeline
						:dpy display
						:name :2d-line-list-pipeline
						:subpass 1))))
	(triangle-pipeline (or (pipeline-store-2d-triangle-list-pipeline pipeline-store)
			       (setf (pipeline-store-2d-triangle-list-pipeline pipeline-store)
				     (make-instance '2d-triangle-list-pipeline
						    :dpy display
						    :name :2d-triangle-list-pipeline
						    :subpass 1))))
	#+NOMORE
	(text-pipeline (pipeline-store-msdf-text-pipeline pipeline-store)))

    ;; 2d has no depth buffer
    ;; so the first thing displayed is covered by later things displayed
    ;; the first thing pushed is the last thing displayed

    #+NOMORE
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push text-pipeline res))
	     (draw-data-2d-triangle-list-draw-list-for-text-table draw-data))

    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push point-pipeline res))
	     (draw-data-2d-point-list-draw-list-table draw-data))

    (push (draw-data-3d-line-strip-draw-list draw-data) res)
    (push (or (pipeline-store-2d-line-strip-pipeline pipeline-store)
	      (setf (pipeline-store-2d-line-strip-pipeline pipeline-store)
		  (make-instance '2d-line-strip-pipeline
				 :dpy display
				 :name :2d-line-strip-pipeline
				 :subpass 1)))
	  res)

    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push line-pipeline res))
	     (draw-data-2d-line-list-draw-list-table draw-data))

    (push (draw-data-2d-triangle-strip-draw-list draw-data) res)
    (push (or (pipeline-store-2d-triangle-strip-pipeline pipeline-store)
	      (setf (pipeline-store-2d-triangle-strip-pipeline pipeline-store)
		    (make-instance '2d-triangle-strip-pipeline
				   :dpy display
				   :name :2d-triangle-strip-pipeline
				   :subpass 1)))
	  res)	  
    
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push triangle-pipeline res))
	     (draw-data-2d-triangle-list-draw-list-table draw-data))    
    res))

(defmethod 2d-draw-list-oriented-combinations ((pipeline-store pipeline-store-mixin) (draw-data immediate-mode-draw-data) display)
  (im-standard-2d-draw-list-oriented-combinations pipeline-store draw-data display))

(defun im-standard-2d-draw-list-oriented-combinations (pipeline-store draw-data display)
  (declare (type pipeline-store-mixin pipeline-store))
  (declare (type immediate-mode-draw-data draw-data))
  
  (let ((res ())
	(point-pipeline (or (pipeline-store-2d-point-list-pipeline pipeline-store)
			    (setf (pipeline-store-2d-point-list-pipeline pipeline-store)
				  (make-instance '2d-point-list-pipeline
						 :dpy display
						 :name :2d-point-list-pipeline
						 :subpass 1))))
	(line-pipeline (or (pipeline-store-2d-line-list-pipeline pipeline-store)
			   (setf (pipeline-store-2d-line-list-pipeline pipeline-store)
				 (make-instance '2d-line-list-pipeline
						:dpy display
						:name :2d-line-list-pipeline
						:subpass 1))))
	(triangle-pipeline (or (pipeline-store-2d-triangle-list-pipeline pipeline-store)
			       (setf (pipeline-store-2d-triangle-list-pipeline pipeline-store)
				     (make-instance '2d-triangle-list-pipeline
						    :dpy display
						    :name :2d-triangle-list-pipeline
						    :subpass 1))))
	#+NOMORE
	(text-pipeline (pipeline-store-msdf-text-pipeline pipeline-store)))

    #+NOMORE
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push text-pipeline res))
	     (draw-data-2d-triangle-list-draw-list-for-text-table draw-data))

    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push point-pipeline res))
	     (draw-data-2d-point-list-draw-list-table draw-data))

    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push line-pipeline res))
	     (draw-data-2d-line-list-draw-list-table draw-data))
    
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push triangle-pipeline res))
	     (draw-data-2d-triangle-list-draw-list-table draw-data))
    res))
