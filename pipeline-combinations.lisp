(in-package :krma)

(defmethod 3d-cmd-oriented-combinations ((pipeline-store pipeline-store-mixin) (draw-data retained-mode-draw-data))
  (rm-standard-3d-cmd-oriented-combinations pipeline-store draw-data))

(defmethod rm-standard-3d-cmd-oriented-combinations (pipeline-store draw-data)
  (declare (type pipeline-store-mixin pipeline-store))
  (declare (type retained-mode-draw-data draw-data))
  
  (list (pipeline-store-3d-point-list-pipeline pipeline-store)
	(draw-data-3d-point-list-draw-list draw-data)
	
	(pipeline-store-3d-line-list-pipeline pipeline-store)
	(draw-data-3d-line-list-draw-list draw-data)

	(pipeline-store-3d-line-strip-pipeline pipeline-store)
	(draw-data-3d-line-strip-draw-list draw-data)
	
	(pipeline-store-3d-triangle-list-pipeline pipeline-store)
	(draw-data-3d-triangle-list-draw-list draw-data)
	
	(pipeline-store-3d-triangle-list-with-normals-pipeline pipeline-store)
	(draw-data-3d-triangle-list-with-normals-draw-list draw-data)

	(pipeline-store-3d-triangle-strip-pipeline pipeline-store)
	(draw-data-3d-triangle-strip-draw-list draw-data)
	
	(pipeline-store-3d-triangle-strip-with-normals-pipeline pipeline-store)
	(draw-data-3d-triangle-strip-with-normals-draw-list draw-data)))

(defmethod 3d-draw-list-oriented-combinations ((pipeline-store pipeline-store-mixin)
					       (draw-data retained-mode-draw-data))
  (rm-standard-3d-draw-list-oriented-combinations pipeline-store draw-data))

(defun rm-standard-3d-draw-list-oriented-combinations (pipeline-store draw-data)
  (declare (type pipeline-store-mixin pipeline-store))
  (declare (type retained-mode-draw-data draw-data))

  (let ((res ())
	(point-pipeline (pipeline-store-3d-point-list-pipeline pipeline-store))
	(line-pipeline (pipeline-store-3d-line-list-pipeline pipeline-store))
	(triangle-pipeline (pipeline-store-3d-triangle-list-pipeline pipeline-store))
	(normal-pipeline (pipeline-store-3d-triangle-list-with-normals-pipeline pipeline-store)))

    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push normal-pipeline res))
	     (draw-data-3d-triangle-list-with-normals-draw-list-table draw-data))

    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push triangle-pipeline res))
	     (draw-data-3d-triangle-list-draw-list-table draw-data))
    
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push line-pipeline res))
	     (draw-data-3d-line-list-draw-list-table draw-data))
    
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push point-pipeline res))
	     (draw-data-3d-point-list-draw-list-table draw-data))
    
    res))	

(defmethod 2d-cmd-oriented-combinations ((pipeline-store pipeline-store-mixin) (draw-data retained-mode-draw-data))
  (rm-standard-2d-cmd-oriented-combinations pipeline-store draw-data))

(defun rm-standard-2d-cmd-oriented-combinations (pipeline-store draw-data)
  (declare (type pipeline-store-mixin pipeline-store))
  (declare (type retained-mode-draw-data draw-data))
  
  (list (pipeline-store-2d-point-list-pipeline pipeline-store)
	(draw-data-2d-point-list-draw-list draw-data)

	(pipeline-store-2d-line-list-pipeline pipeline-store)
	(draw-data-2d-line-list-draw-list draw-data)

	(pipeline-store-2d-line-strip-pipeline pipeline-store)
	(draw-data-2d-line-strip-draw-list draw-data)

	(pipeline-store-2d-triangle-list-pipeline pipeline-store)
	(draw-data-2d-triangle-list-draw-list draw-data)

	(pipeline-store-2d-triangle-strip-pipeline pipeline-store)
	(draw-data-2d-triangle-strip-draw-list draw-data)

	(pipeline-store-msdf-text-pipeline pipeline-store)
	(draw-data-2d-triangle-list-draw-list-for-text draw-data)))

(defmethod 2d-draw-list-oriented-combinations ((pipeline-store pipeline-store-mixin)
					       (draw-data retained-mode-draw-data))
  (rm-standard-2d-draw-list-oriented-combinations pipeline-store draw-data))

(defun rm-standard-2d-draw-list-oriented-combinations (pipeline-store draw-data)
  (declare (type pipeline-store-mixin pipeline-store))
  (declare (type retained-mode-draw-data draw-data))
  
  (let ((res ())
	(point-pipeline (pipeline-store-2d-point-list-pipeline pipeline-store))
	(line-pipeline (pipeline-store-2d-line-list-pipeline pipeline-store))
	(triangle-pipeline (pipeline-store-2d-triangle-list-pipeline pipeline-store))
	(text-pipeline (pipeline-store-msdf-text-pipeline pipeline-store)))

    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push triangle-pipeline res))
	     (draw-data-2d-triangle-list-draw-list-table draw-data))
    
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push line-pipeline res))
	     (draw-data-2d-line-list-draw-list-table draw-data))
    
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push point-pipeline res))
	     (draw-data-2d-point-list-draw-list-table draw-data))

    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push text-pipeline res))
	     (draw-data-2d-triangle-list-draw-list-for-text-table draw-data))
    res))

(defmethod 3d-cmd-oriented-combinations ((pipeline-store pipeline-store-mixin) (draw-data immediate-mode-draw-data))
  (im-standard-3d-cmd-oriented-combinations pipeline-store draw-data))

(defun im-standard-3d-cmd-oriented-combinations (pipeline-store draw-data)
  (declare (type pipeline-store-mixin pipeline-store))
  (declare (type immediate-mode-draw-data draw-data))

  (let ((res ())
	(point-pipeline (pipeline-store-3d-point-list-pipeline pipeline-store))
	(line-pipeline (pipeline-store-3d-line-list-pipeline pipeline-store))
	(triangle-pipeline (pipeline-store-3d-triangle-list-pipeline pipeline-store)))
    
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push triangle-pipeline res))
	     (draw-data-3d-triangle-list-draw-list-table draw-data))
    
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push line-pipeline res))
	     (draw-data-3d-line-list-draw-list-table draw-data))
    
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push point-pipeline res))
	     (draw-data-3d-point-list-draw-list-table draw-data))
    
    (list* (pipeline-store-3d-line-strip-pipeline pipeline-store)
	   (draw-data-3d-line-strip-draw-list draw-data)

	   (pipeline-store-3d-triangle-strip-pipeline pipeline-store)
	   (draw-data-3d-triangle-strip-draw-list draw-data)

	   (pipeline-store-3d-triangle-strip-with-normals-pipeline pipeline-store)
	   (draw-data-3d-triangle-strip-with-normals-draw-list draw-data)

	   res)))

(defmethod 3d-draw-list-oriented-combinations ((pipeline-store pipeline-store-mixin) (draw-data immediate-mode-draw-data))
  (im-standard-3d-draw-list-oriented-combinations pipeline-store draw-data))

(defun im-standard-3d-draw-list-oriented-combinations (pipeline-store draw-data)
  (declare (type pipeline-store-mixin pipeline-store))
  (declare (type immediate-mode-draw-data draw-data))
  
  (let ((res ())
	(point-pipeline (pipeline-store-3d-point-list-pipeline pipeline-store))
	(line-pipeline (pipeline-store-3d-line-list-pipeline pipeline-store))
	(triangle-pipeline (pipeline-store-3d-triangle-list-pipeline pipeline-store))
	(normal-pipeline (pipeline-store-3d-triangle-list-with-normals-pipeline pipeline-store)))

    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push normal-pipeline res))
	     (draw-data-3d-triangle-list-with-normals-draw-list-table draw-data))

    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push triangle-pipeline res))
	     (draw-data-3d-triangle-list-draw-list-table draw-data))
    
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push line-pipeline res))
	     (draw-data-3d-line-list-draw-list-table draw-data))
    
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push point-pipeline res))
	     (draw-data-3d-point-list-draw-list-table draw-data))

    res))

(defmethod 2d-cmd-oriented-combinations ((pipeline-store pipeline-store-mixin) (draw-data immediate-mode-draw-data))
  (im-standard-2d-cmd-oriented-combinations pipeline-store draw-data))

(defun im-standard-2d-cmd-oriented-combinations (pipeline-store draw-data)
  (declare (type pipeline-store-mixin pipeline-store))
  (declare (type immediate-mode-draw-data draw-data))

  (let ((res ())
	(point-pipeline (pipeline-store-2d-point-list-pipeline pipeline-store))
	(line-pipeline (pipeline-store-2d-line-list-pipeline pipeline-store))
	(triangle-pipeline (pipeline-store-2d-triangle-list-pipeline pipeline-store))
	(text-pipeline (pipeline-store-msdf-text-pipeline pipeline-store)))

    ;; 2d has no depth buffer
    ;; so the first thing displayed is covered by later things displayed
    ;; the first thing pushed is the last thing displayed

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
    (push (pipeline-store-2d-line-strip-pipeline pipeline-store) res)

    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push line-pipeline res))
	     (draw-data-2d-line-list-draw-list-table draw-data))

    (push (draw-data-2d-triangle-strip-draw-list draw-data) res)
    (push (pipeline-store-2d-triangle-strip-pipeline pipeline-store) res)    
    
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (push v res)
		 (push triangle-pipeline res))
	     (draw-data-2d-triangle-list-draw-list-table draw-data))    
    res))

(defmethod 2d-draw-list-oriented-combinations ((pipeline-store pipeline-store-mixin) (draw-data immediate-mode-draw-data))
  (im-standard-2d-draw-list-oriented-combinations pipeline-store draw-data))

(defun im-standard-2d-draw-list-oriented-combinations  (pipeline-store draw-data)
  (declare (type pipeline-store-mixin pipeline-store))
  (declare (type immediate-mode-draw-data draw-data))
  
  (let ((res ())
	(point-pipeline (pipeline-store-2d-point-list-pipeline pipeline-store))
	(line-pipeline (pipeline-store-2d-line-list-pipeline pipeline-store))
	(triangle-pipeline (pipeline-store-2d-triangle-list-pipeline pipeline-store))
	(text-pipeline (pipeline-store-msdf-text-pipeline pipeline-store)))

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
