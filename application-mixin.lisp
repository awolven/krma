(in-package :krma)

(defvar *font*)
(defvar *font2*)

(defclass pipeline-store-mixin ()
  ((2d-point-list-pipeline :accessor pipeline-store-2d-point-list-pipeline)
   (2d-line-list-pipeline :accessor pipeline-store-2d-line-list-pipeline)
   (2d-line-strip-pipeline :accessor pipeline-store-2d-line-strip-pipeline)
   (2d-triangle-list-pipeline :accessor pipeline-store-2d-triangle-list-pipeline)
   (msdf-text-pipeline :accessor pipeline-store-msdf-text-pipeline)
   (2d-triangle-strip-pipeline :accessor pipeline-store-2d-triangle-strip-pipeline)
   (3d-point-list-pipeline :accessor pipeline-store-3d-point-list-pipeline)
   (3d-line-list-pipeline :accessor pipeline-store-3d-line-list-pipeline)
   (3d-line-strip-pipeline :accessor pipeline-store-3d-line-strip-pipeline)
   (3d-triangle-list-pipeline :accessor pipeline-store-3d-triangle-list-pipeline)
   (3d-triangle-list-with-normals-pipeline :accessor pipeline-store-3d-triangle-list-with-normals-pipeline)
   (3d-triangle-strip-pipeline :accessor pipeline-store-3d-triangle-strip-pipeline)
   (3d-triangle-strip-with-normals-pipeline :accessor pipeline-store-3d-triangle-strip-with-normals-pipeline)
   ))

(defclass standard-pipeline-store (pipeline-store-mixin)
  ())

(defmethod initialize-instance :after ((instance pipeline-store-mixin) &rest initargs &key app)
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
               msdf-text-pipeline
               2d-triangle-strip-pipeline)
      instance

    (setf 2d-point-list-pipeline
	  (make-instance '2d-point-list-pipeline
			 :app app
			 :name :2d-point-list-pipeline)

	  2d-line-list-pipeline
	  (make-instance '2d-line-list-pipeline
			 :app app
			 :name :2d-line-list-pipeline)

	  2d-line-strip-pipeline
	  (make-instance '2d-line-strip-pipeline
			 :app app
			 :name :2d-line-strip-pipeline)

	  2d-triangle-list-pipeline
	  (make-instance '2d-triangle-list-pipeline
			 :app app
			 :name :2d-triangle-list-pipeline)

	  msdf-text-pipeline
	  (make-instance 'msdf-text-pipeline
			 :app app
			 :name :msdf-text-pipeline)
	  
          2d-triangle-strip-pipeline
	  (make-instance '2d-triangle-strip-pipeline
			 :app app
			 :name :2d-triangle-strip-pipeline)

	  3d-point-list-pipeline
	  (make-instance '3d-point-list-pipeline
			 :app app
			 :name :3d-point-list-pipeline)
	  
	  3d-line-list-pipeline
	  (make-instance '3d-line-list-pipeline
			 :app app
			 :name :3d-line-list-pipeline)
	  
          3d-line-strip-pipeline
	  (make-instance '3d-line-strip-pipeline
			 :app app
			 :name :3d-line-strip-pipeline)
	  
          3d-triangle-list-pipeline
	  (make-instance '3d-triangle-list-pipeline
			 :app app
			 :name :3d-triangle-list-pipeline)
	  
          3d-triangle-list-with-normals-pipeline
	  (make-instance '3d-triangle-list-with-normals-pipeline
			 :app app
			 :name :3d-triangle-list-with-normals-pipeline)

	  3d-triangle-strip-pipeline
	  (make-instance '3d-triangle-strip-pipeline
			 :app app
			 :name :3d-triangle-strip-pipeline)
	  
	  3d-triangle-strip-with-normals-pipeline
	  (make-instance '3d-triangle-strip-with-normals-pipeline
			 :app app
			 :name :3d-triangle-strip-with-normals-pipeline)))
  (values))

(defclass krma-application-mixin ()
  ((vk::application-name :initform "krma-application")
   (scene :initform nil :accessor application-scene)
   (pipeline-store :accessor application-pipeline-store)
   (exit? :initform nil :accessor application-exit?)
   (current-frame-cons :initform (list 0) :accessor current-frame-cons)
   (current-draw-data-cons :initform (list 0) :accessor current-draw-data-cons)
   (retained-mode-handle-count-cons :initform (list -1) :accessor retained-mode-handle-count-cons)
   (immediate-mode-work-function-1 :initform nil :accessor immediate-mode-work-function-1)
   (backtrace :initform nil :accessor application-backtrace)
   (error-msg :initform nil :accessor application-error-msg)
   (width :accessor main-window-width)
   (height :accessor main-window-height)))

(defun backtrace-string ()
  (with-output-to-string (*debug-io*)
    (sb-debug:print-backtrace)))

(defun record-backtrace (app)
  (setf (application-backtrace app) (backtrace-string)))

(defun record-error-msg (app c)
  (let ((*print-escape* nil))
    (setf (application-error-msg app)
          (format nil "~W" c))))

(defmacro maybe-defer-debug ((app) &body body)
  (let ((app-sym (gensym)))
    `(let ((,app-sym ,app))
       (restart-bind ((ignore (lambda (&optional c)
                                (declare (ignorable c))
                                (throw :ignore nil))))
         (catch :ignore
           (handler-bind ((error (lambda (c)
                                   (record-error-msg ,app-sym c)
                                   (record-backtrace ,app-sym))))
             ,@body))))))

(defmethod initialize-instance :after ((instance krma-application-mixin) &rest initargs)
  (declare (ignore initargs))
  (setf (application-pipeline-store instance) (make-instance 'standard-pipeline-store :app instance))
  (setf (application-scene instance) (make-instance (scene-class instance)))
  (values))

(defclass krma-test-application (krma-application-mixin #-darwin vulkan-application-mixin
                                                        #+darwin metal-application-mixin)
  ((vk::application-name :initform "krma-test-application")))

(defmethod scene-class ((application krma-test-application))
  'standard-scene)

(defun add-2d-point (x y &key (color *default-color*)
			   (point-size *default-point-size*)
			   (matrix nil))
  (scene-add-2d-point (application-scene *app*) matrix point-size color x y))

(defun group-add-2d-point (group x y &key (color *default-color*)
				       (point-size *default-point-size*))
  (scene-add-2d-point-to-group (application-scene *app*) group point-size color x y))

(defun draw-2d-point (x y &key (color *default-color*)
			    (point-size *default-point-size*))
  (scene-draw-2d-point (application-scene *app*) point-size color x y))

(defun add-3d-point (x y z &key (color *default-color*)
			     (point-size *default-point-size*)
			     (matrix nil))
  (scene-add-3d-point (application-scene *app*) matrix point-size color x y z))

(defun group-add-3d-point (group x y z &key (color *default-color*)
					 (point-size *default-point-size*))
  (scene-add-3d-point-to-group (application-scene *app*) group point-size color x y z))

(defun draw-3d-point (x y z &key (color *default-color*)
			      (point-size *default-point-size*))
  (scene-draw-3d-point (application-scene *app*) point-size color x y z))

(defun add-2d-line (x0 y0 x1 y1 &key (color *default-color*)
				  (line-thickness *default-line-thickness*)
				  (matrix nil))
  (scene-add-2d-line (application-scene *app*) matrix line-thickness color x0 y0 x1 y1))

(defun group-add-2d-line (group x0 y0 x1 y1 &key (color *default-color*)
					      (line-thickness *default-line-thickness*))
  (scene-add-2d-line-to-group (application-scene *app*) group line-thickness color x0 y0 x1 y1))

(defun draw-2d-line (x0 y0 x1 y1 &key (color *default-color*)
				   (line-thickness *default-line-thickness*))
  (scene-draw-2d-line (application-scene *app*) line-thickness color x0 y0 x1 y1))

(defun add-3d-line (x0 y0 z0 x1 y1 z1 &key (color *default-color*)
					(line-thickness *default-line-thickness*)
					(matrix nil))
  (scene-add-3d-line (application-scene *app*) matrix line-thickness color x0 y0 z0 x1 y1 z1))

(defun group-add-3d-line (group x0 y0 z0 x1 y1 z1 &key (color *default-color*)
						    (line-thickness *default-line-thickness*))
  (scene-add-3d-line-to-group (application-scene *app*) group line-thickness color x0 y0 z0 x1 y1 z1))

(defun draw-3d-line (x0 y0 z0 x1 y1 z1 &key (color *default-color*)
					 (line-thickness *default-line-thickness*))
  (scene-draw-3d-line (application-scene *app*) line-thickness color x0 y0 z0 x1 y1 z1))

(defun add-multicolor-2d-polyline (vertices &key (closed? nil)
					      (line-thickness *default-line-thickness*)
                                              (matrix nil))                                              
  (scene-add-multicolor-2d-polyline (application-scene *app*) matrix closed? line-thickness vertices))

(defun group-add-multicolor-2d-polyline (group vertices &key (closed? nil)
							  (line-thickness *default-line-thickness*))
  (scene-add-multicolor-2d-polyline-to-group (application-scene *app*) group closed? line-thickness vertices))

(defun draw-multicolor-2d-polyline (vertices &key (closed? nil)
					       (line-thickness *default-line-thickness*))
  (scene-draw-multicolor-2d-polyline (application-scene *app*) closed? line-thickness vertices))

(defun add-2d-polyline (vertices &key (closed? nil)
                                   (color *default-color*)
                                   (line-thickness *default-line-thickness*)
				   (matrix nil))
  (scene-add-2d-polyline (application-scene *app*) matrix closed? line-thickness color vertices))

(defun group-add-2d-polyline (group vertices &key (closed? nil)
					       (color *default-color*)
					       (line-thickness *default-line-thickness*))
  (scene-add-2d-polyline-to-group (application-scene *app*) group closed? line-thickness color vertices))

(defun draw-2d-polyline (vertices &key (closed? nil)
                                    (color *default-color*)
                                    (line-thickness *default-line-thickness*))
  (scene-draw-2d-polyline (application-scene *app*) closed? line-thickness color vertices))
  

(defun add-2d-triangle (x0 y0 x1 y1 x2 y2 &key
                                            (color *default-color*)
                                            (line-thickness *default-line-thickness*)
					    (matrix nil))
  (scene-add-2d-triangle (application-scene *app*) matrix line-thickness color x0 y0 x1 y1 x2 y2))

(defun group-add-2d-triangle (group x0 y0 x1 y1 x2 y2 &key
							(color *default-color*)
							(line-thickness *default-line-thickness*))
  (scene-add-2d-triangle-to-group (application-scene *app*) group line-thickness color x0 y0 x1 y1 x2 y2))
  

(defun draw-2d-triangle (x0 y0 x1 y1 x2 y2 &key (color *default-color*)
					     (line-thickness *default-line-thickness*))
  (scene-draw-2d-triangle (application-scene *app*) line-thickness color x0 y0 x1 y1 x2 y2))

(defun add-2d-rectangle (x0 y0 x1 y1
                         &key (color *default-color*)
                           (line-thickness *default-line-thickness*)
			   (matrix nil))
  (scene-add-2d-rectangle (application-scene *app*) matrix line-thickness color x0 y0 x1 y1))

(defun group-add-2d-rectangle (group x0 y0 x1 y1
			       &key (color *default-color*)
				 (line-thickness *default-line-thickness*))
  (scene-add-2d-rectangle-to-group (application-scene *app*) group line-thickness color x0 y0 x1 y1))
  

(defun draw-2d-rectangle (x0 y0 x1 y1 &key (color *default-color*)
					(line-thickness *default-line-thickness*))
  (scene-draw-2d-rectangle (application-scene *app*) line-thickness color x0 y0 x1 y1))

(defun add-2d-circular-arc (center-x center-y radius start-angle end-angle
                            &key (closed? nil)
                              (color *default-color*)
                              (line-thickness *default-line-thickness*)
                              (number-of-segments *default-number-of-segments*)
			      (matrix nil))
  (scene-add-2d-circular-arc (application-scene *app*) matrix closed? line-thickness color
                             center-x center-y radius start-angle end-angle number-of-segments))

(defun group-add-2d-circular-arc (group center-x center-y radius start-angle end-angle
					&key (closed? nil)
					(color *default-color*)
					(line-thickness *default-line-thickness*)
					  (number-of-segments *default-number-of-segments*))
  (scene-add-2d-circular-arc-to-group (application-scene *app*) group closed? line-thickness color
                                      center-x center-y radius start-angle end-angle number-of-segments))

(defun draw-2d-circular-arc (center-x center-y radius start-angle end-angle
                             &key (closed? nil)
                               (color *default-color*)
                               (line-thickness *default-line-thickness*)
                               (number-of-segments *default-number-of-segments*))
  (scene-draw-2d-circular-arc (application-scene *app*) closed? line-thickness color
                             center-x center-y radius start-angle end-angle number-of-segments))
  

(defun add-2d-circle (center-x center-y radius
                      &key (color *default-color*)
                        (line-thickness *default-line-thickness*)
                        (number-of-segments *default-number-of-segments*)
			(matrix nil))
  (scene-add-2d-circle (application-scene *app*)
                       matrix line-thickness color
                       center-x center-y radius number-of-segments))

(defun group-add-2d-circle (group center-x center-y radius
			    &key (color *default-color*)
			      (line-thickness *default-line-thickness*)
			      (number-of-segments *default-number-of-segments*))
    (scene-add-2d-circle-to-group (application-scene *app*)
				  group line-thickness color
				  center-x center-y radius number-of-segments))

(defun add-filled-2d-circle (center-x center-y radius
                             &key (color *default-color*)
                               (number-of-segments *default-number-of-segments*)
			       (matrix nil))
  (scene-add-filled-2d-circle (application-scene *app*)
                              matrix color center-x center-y radius number-of-segments))

(defun group-add-filled-2d-circle (group center-x center-y radius
					 &key (color *default-color*)
					   (number-of-segments *default-number-of-segments*))
  (scene-add-filled-2d-circle-to-group (application-scene *app*)
                                       group color center-x center-y radius number-of-segments))

(defun draw-filled-2d-circle (center-x center-y radius
                              &key (color *default-color*)
                                (number-of-segments *default-number-of-segments*))
  (scene-draw-filled-2d-circle (application-scene *app*) color center-x center-y radius number-of-segments))
  

(defun add-multicolor-3d-polyline (vertices &key (closed? nil)
                                              (line-thickness *default-line-thickness*)
					      (matrix nil))
  (scene-add-multicolor-3d-polyline (application-scene *app*) matrix closed? line-thickness vertices))

(defun group-add-multicolor-3d-polyline (group vertices &key (closed? nil)
							  (line-thickness *default-line-thickness*))
  (scene-add-multicolor-3d-polyline-to-group (application-scene *app*) group closed? line-thickness vertices))

(defun draw-multicolor-3d-polyline (vertices &key (closed? nil)
                                              (line-thickness *default-line-thickness*))
  (scene-draw-multicolor-3d-polyline (application-scene *app*) closed? line-thickness vertices))
  

(defun add-3d-polyline (vertices &key (color *default-color*)
                                   (closed? nil)
                                   (line-thickness *default-line-thickness*)
				                   (matrix nil))
  (scene-add-3d-polyline (application-scene *app*) matrix closed? line-thickness color vertices))

(defun group-add-3d-polyline (group vertices &key (color *default-color*)
					       (closed? nil)
					       (line-thickness *default-line-thickness*))
  (scene-add-3d-polyline-to-group (application-scene *app*) group closed? line-thickness color vertices))

(defun draw-3d-polyline (vertices &key (color *default-color*)
                                   (closed? nil)
                                   (line-thickness *default-line-thickness*))
  (scene-draw-3d-polyline (application-scene *app*) closed? line-thickness color vertices))
  

(defun add-filled-2d-triangle-list (vertices &key (color *default-color*)
                                               (matrix nil))
  (scene-add-filled-2d-triangle-list (application-scene *app*) matrix color vertices))

(defun group-add-2d-triangle-list (group vertices &key (color *default-color*))
  (scene-add-filled-2d-triangle-list-to-group (application-scene *app*) group color vertices))

(defun draw-filled-2d-triangle-list (vertices &key (color *default-color*))
  (scene-draw-filled-2d-triangle-list (application-scene *app*) color vertices))
  

(defun add-filled-2d-rectangle-list (vertices &key (color *default-color*)
                                                (matrix nil))
  (scene-add-filled-2d-rectangle-list (application-scene *app*) matrix color vertices))

(defun group-add-filled-2d-triangle-list (group vertices &key (color *default-color*))
  (scene-add-filled-2d-rectangle-list-to-group (application-scene *app*) group color vertices))

(defun draw-filled-2d-rectangle-list (vertices &key (color *default-color*))
  (scene-draw-filled-2d-rectangle-list (application-scene *app*) color vertices))
						

(defun add-textured-2d-rectangle-list (vertices &key (color *default-color*)
                                                  (texture *white-texture*)
						  (matrix nil))
  (scene-add-textured-2d-rectangle-list (application-scene vk::*app*) matrix texture color vertices))

(defun group-add-textured-2d-rectangle-list (group vertices &key (color *default-color*)
							      (texture *white-texture*))
  (scene-add-textured-2d-rectangle-list-to-group (application-scene vk::*app*) group texture color vertices))

(defun draw-textured-2d-rectangle-list (vertices &key (color *default-color*)
                                                  (texture *white-texture*))
  (scene-draw-textured-2d-rectangle-list (application-scene vk::*app*) texture color vertices))

(defun add-filled-2d-convex-polygon (vertices &key
						(color *default-color*)
						(matrix nil))
  (scene-add-filled-2d-convex-polygon (application-scene *app*) matrix color vertices))

(defun group-add-filled-2d-convex-polygon (group vertices &key (color *default-color*))
  (scene-add-filled-2d-convex-polygon-to-group (application-scene *app*) group color vertices))

(defun draw-filled-2d-convex-polygon (vertices &key
						                         (color *default-color*))
  (scene-draw-filled-2d-convex-polygon (application-scene *app*) color vertices))

(defun add-filled-3d-triangle-list (vertices &key
                                               (color *default-color*)
					       (shading-style :diffuse)
					       (light-position *default-light-position*)
                                               (matrix nil))
  (ecase shading-style
    (:flat (scene-add-filled-3d-triangle-list-flat (application-scene *app*) matrix color vertices))
    (:diffuse (scene-add-filled-3d-triangle-list-diffuse (application-scene *app*) matrix color vertices
							 light-position))))

(defun group-add-filled-3d-triangle-list (group vertices &key (color *default-color*)
							   (shading-style :diffuse))
  (ecase shading-style
    (:flat (scene-add-filled-3d-triangle-list-flat-to-group (application-scene *app*) group color vertices))
    (:diffuse (scene-add-filled-3d-triangle-list-diffuse-to-group (application-scene *app*) group color vertices))))

(defun draw-filled-3d-triangle-list (vertices &key
                                                (color *default-color*)
					                            (shading-style :diffuse))
  (ecase shading-style
    (:flat (scene-draw-filled-3d-triangle-list-flat (application-scene *app*) color vertices))
    (:diffuse (scene-draw-filled-3d-triangle-list-diffuse (application-scene *app*) color vertices))))

(defun add-filled-3d-triangle-strip (vertices &key
						(color *default-color*)
						(shading-style :diffuse)
						(light-position *default-light-position*)
						(matrix nil))
  (ecase shading-style
    (:flat (scene-add-filled-3d-triangle-strip-flat (application-scene *app*) matrix color vertices))
    (:diffuse (scene-add-filled-3d-triangle-strip-diffuse (application-scene *app*) matrix color vertices
                                                          light-position))))

(defun draw-filled-3d-triangle-strip (vertices &key
                                                 (color *default-color*)
                                                 (shading-style :diffuse))
  (ecase shading-style
    (:flat (scene-draw-filled-3d-triangle-strip-flat (application-scene *app*) color vertices))
    (:diffuse (scene-draw-filled-3d-triangle-strip-diffuse (application-scene *app*) color vertices))))

#+NOTYET
;; need to implement group semantics for pseudo-primitives which actually are using real primitives underneath
(defun group-add-filled-3d-triangle-strip (group vertices &key (color *default-color*)
							    (shading-style :diffuse))
  (ecase shading-style
    (:flat (scene-add-filled-3d-triangle-strip-flat-to-group (application-scene *app*) group color vertices))
    (:diffuse (scene-add-filled-3d-triangle-strip-diffuse-to-group
	       (application-scene *app*) group color vertices))))
  

(defun add-textured-3d-triangle-list (vertices &key (color *default-color*)
                                                 (texture *white-texture*)
						 (shading-style :diffuse)
						 (light-position *default-light-position*)
						 (matrix nil))
  (declare (ignore light-position))
  (ecase shading-style
    (:flat (scene-add-textured-3d-triangle-list-flat (application-scene *app*) matrix texture color vertices))))

(defun group-add-textured-3d-triangle-list (group vertices &key (color *default-color*)
							     (texture *white-texture*)
							     (shading-style :diffuse))
  (ecase shading-style
    (:flat (scene-add-textured-3d-triangle-list-flat-to-group
            (application-scene *app*) group texture color vertices))))

(defun draw-textured-3d-triangle-list (vertices &key (color *default-color*)
                                                 (texture *white-texture*)
						 (shading-style :diffuse))
  (ecase shading-style
    (:flat (scene-draw-textured-3d-triangle-list-flat (application-scene *app*) texture color vertices))))

(defun add-textured-3d-triangle-strip (vertices &key
                                                 (color *default-color*)
						  (texture *white-texture*)
						  (shading-style :diffuse)
						  (light-position *default-light-position*)
						  (matrix nil))
  (declare (ignore light-position))
  (ecase shading-style
    (:flat (scene-add-textured-3d-triangle-strip-flat (application-scene *app*) matrix texture color vertices))))

#+NOTYET
;; need to implement group semantics for pseudo-primitives which actually are using real primitives underneath
(defun group-add-textured-3d-triangle-strip (vertices &key
							(color *default-color*)
							(texture *white-texture*)
							(shading-style :diffuse))
  (ecase shading-style
    (:flat (scene-add-textured-3d-triangle-strip-flat-to-group
            (application-scene *app*) group texture color vertices))))

(defun draw-textured-3d-triangle-strip (vertices &key
                                                 (color *default-color*)
						  (texture *white-texture*)
						  (shading-style :diffuse))
  (ecase shading-style
    (:flat (scene-draw-textured-3d-triangle-strip-flat (application-scene *app*) texture color vertices))))

(defun add-filled-sphere (origin-x origin-y origin-z radius &key (color *default-color*)
							      (resolution 64)
							      (shading-style :diffuse)
							      (light-position *default-light-position*)
							      (matrix nil))

  (ecase shading-style
    (:diffuse (scene-add-filled-sphere-diffuse (application-scene *app*)
					       matrix color
					       origin-x origin-y origin-z radius
					       resolution light-position))))

(defun group-add-filled-sphere (group origin-x origin-y origin-z radius &key (color *default-color*)
									  (resolution 64)
									  (shading-style :diffuse))
  (ecase shading-style
    (:diffuse (scene-add-filled-sphere-diffuse-to-group (application-scene *app*)
							group color
							origin-x origin-y origin-z radius resolution))))

(defun draw-filled-sphere (origin-x origin-y origin-z radius &key (color *default-color*)
                                                               (resolution 64)
                                                               (shading-style :diffuse))

  (ecase shading-style
    (:diffuse (scene-draw-filled-sphere-diffuse (application-scene *app*)
					                            color
					                            origin-x origin-y origin-z radius
					                            resolution))))
  



(defun add-text (string pos-x pos-y &key (color *default-color*)
                                      (font *font*)
                                      (matrix nil))
  (scene-add-text (application-scene *app*) matrix font color pos-x pos-y string))

(defun group-add-text (group string pos-x pos-y &key (color *default-color*)
						  (font *font*))
  (scene-add-text-to-group (application-scene *app*) group font color pos-x pos-y string))

(defun draw-text (string pos-x pos-y &key (color *default-color*)
                                       (font *font*))
  (scene-draw-text (application-scene *app*) font color pos-x pos-y string))

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
  (maybe-defer-debug (app)
    (let ((f (immediate-mode-work-function-1 app)))
      (when f (funcall f)))))

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

    ;; these should be put in initialize-instance of krma-test-application
    (multiple-value-bind (width height) (get-framebuffer-size main-window)
      (setf (main-window-width app) width
	    (main-window-height app) height))
    
    (setf *sampler* sampler)

    (device-wait-idle device)

    (reset-command-pool device command-pool)

    ;; one time commands here.
    (unless (probe-file (asdf/system:system-relative-pathname :krma "acache.json"))
      (sdf-bmfont:create-bmfont #+linux "/usr/share/fonts/liberation-mono/LiberationMono-Regular.ttf" #+windows "C:/Windows/Fonts/Arial.ttf" "acache.json" :size 32 :mode :msdf+a :type :json :spread 8))
    #-linux
    (unless (probe-file (asdf/system:system-relative-pathname :krma "tcache.json"))
      (sdf-bmfont:create-bmfont "C:/Windows/Fonts/Times.ttf" "tcache.json" :size 32 :mode :msdf+a :type :json :spread 8))


    (setq *font*
          (vulkan-make-font device queue sampler texture-dsl descriptor-pool command-buffer
                            :cache-file "acache.json"))
    #-linux
    (setq *font2*
          (vulkan-make-font device queue sampler texture-dsl descriptor-pool command-buffer
                            :cache-file "tcache.json"))

    (let* ((bpp 4)
           (bitmap (make-array bpp :element-type '(unsigned-byte 8) :initial-element #xff)))
      (setq *white-texture*
            (make-vulkan-texture device queue sampler texture-dsl descriptor-pool command-buffer bpp bitmap 1 1)))

    ;;(test)

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
		     (setf (main-window-width app) width
			   (main-window-height app) height)
                     (setf (recreate-swapchain? main-window) nil)))

                 (let* ((swapchain (swapchain main-window))
                        (frame-count (number-of-images swapchain))
                        (current-frame (car current-frame-cons))
                        (current-draw-data (car current-draw-data-cons))
                        (scene (application-scene app))
                        (frame-resource0 (elt (frame-resources swapchain) current-frame))
                        (command-buffer (frame-command-buffer frame-resource0))
                        (rm-draw-data (aref (rm-draw-data scene) current-draw-data)))

                   (erase-immediate-mode-draw-data app)
                   (setq work-queue (draw-data-work-queue rm-draw-data))

                   (maybe-defer-debug (app)
                     (loop with work = nil
                           while (setq work (sb-concurrency:dequeue work-queue))
                           do (funcall work)))

                   (call-immediate-mode-work-functions app)

                   (setq image-index
                         (frame-begin swapchain (render-pass swapchain)
                                      current-frame (clear-value main-window)
                                      command-pool))

		   (update-2d-camera scene)
		   (update-3d-camera scene)
                   ;; render here.
		   (render-scene scene app command-buffer rm-draw-data (im-draw-data scene))

                   (frame-end swapchain queue current-frame)

                   (frame-present swapchain queue current-frame image-index main-window)

                   ;; this needs to be the only thread that modifies current-frame
                   (sb-ext:atomic-update (car current-frame-cons)
                                         #'(lambda (cf) (mod (1+ cf) frame-count)))
                   (sb-ext:atomic-update (car current-draw-data-cons)
                                         #'(lambda (cdd) (mod (1+ cdd) 2)))))

        (shutdown-application app)))))


