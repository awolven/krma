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

(defclass krma-application-mixin (  vulkan-application-mixin
                                    )
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

(defclass krma-test-application (krma-application-mixin)
  ((vk::application-name :initform "krma-test-application")))

(defmethod scene-class ((application krma-test-application))
  'standard-scene)

(defun add-2d-point-primitive (x y &key (color *default-color*)
                                     (point-size *default-point-size*)
                                     (matrix nil)
				     (group nil)
				     (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, returns a handle. Calls scene-add-2d-point-primitive with color defaulting to *default-color*, point-size defaulting to *default-point-size*, matrix defaulting to nil (identity), group defaulting to nil (no group), and scene defaulting to (application-scene *app*). The required arguments x and y must be real numbers."
  (scene-add-2d-point-primitive scene group matrix point-size color x y))

(defun add-2d-point (x y &key (color *default-color*)
                           (point-size *default-point-size*)
                           (group :default)
			   (scene (application-scene *app*)))
  "Retained-mode function, returns no values. Calls scene-add-2d-point with color defaulting to *default-color*, point-size defaulting to *default-point-size*, group defaulting to :default, and scene defaulting to (application-scene *app*). The required arguments x and y must be real numbers."
  (scene-add-2d-point scene group point-size color x y))

(defun draw-2d-point (x y &key (color *default-color*)
                            (point-size *default-point-size*)
                            (group :default)
			    (scene (application-scene *app*)))
  "Immediate-mode function, returns no values. Calls scene-draw-2d-point with color defaulting to *default-color*, point-size defaulting to *default-point-size*, group defaulting to :default, and scene defaulting to (application-scene *app*). The required arguments x and y must be real numbers."
  (scene-draw-2d-point scene group point-size color x y))

(defun add-3d-point-primitive (x y z &key (color *default-color*)
				       (point-size *default-point-size*)
				       (matrix nil)
				       (group nil)
				       (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, returns a handle. Calls scene-add-3d-point-primitive with color defaulting to *default-color*, point-size defaulting to *default-point-size*, matrix defaulting to nil (identity), group defaulting to nil (no group), and scene defaulting to (application-scene *app*). The required arguments x, y and z must be real numbers."
  (scene-add-3d-point-primitive scene group matrix point-size color x y z))

(defun add-3d-point (x y z &key (color *default-color*)
                             (point-size *default-point-size*)
                             (group :default)
			     (scene (application-scene *app*)))
  "Retained-mode function, returns no values. Calls scene-add-3d-point with color defaulting to *default-color*, point-size defaulting to *default-point-size*, group defaulting to :default, and scene defaulting to (application-scene *app*). The required arguments x, y and z must be real numbers."
  (scene-add-3d-point scene group point-size color x y z))

(defun draw-3d-point (x y z &key (color *default-color*)
                              (point-size *default-point-size*)
                              (group :default)
			      (scene (application-scene *app*)))
  "Immediate-mode function, returns no values. Calls scene-draw-3d-point with color defaulting to *default-color*, point-size defaulting to *default-point-size*, group defaulting to :default, and scene defaulting to (application-scene *app*). The required arguments x, y and z must be real numbers."
  (scene-draw-3d-point scene group point-size color x y z))

(defun add-2d-line-primitive (x0 y0 x1 y1 &key (color *default-color*)
					    (line-thickness *default-line-thickness*)
					    (matrix nil)
					    (group nil)
					    (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, returns a handle.  Calls scene-add-2d-line-primitive with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, matrix defaulting to nil (identity), group defaulting to nil (no group), and scene defaulting to (application-scene *app*).  The required arguments x0, y0, x1, and y1 are the endpoints of the line and must be real numbers."
  (scene-add-2d-line-primitive scene group matrix line-thickness color x0 y0 x1 y1))

(defun add-2d-line (x0 y0 x1 y1 &key (color *default-color*)
                                  (line-thickness *default-line-thickness*)
                                  (group :default)
				  (scene (application-scene *app*)))
  "Retained-mode function, returns no values.  Calls scene-add-2d-line with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, group defaulting to nil (no group), and scene defaulting to (application-scene *app*).  The required arguments x0, y0, x1 and y1 are the endpoints of the line and must be real numbers."
  (scene-add-2d-line scene group line-thickness color x0 y0 x1 y1))

(defun draw-2d-line (x0 y0 x1 y1 &key (color *default-color*)
                                   (line-thickness *default-line-thickness*)
                                   (group :default)
				   (scene (application-scene *app*)))
  "Immediate-mode function, returns no values.  Calls scene-draw-2d-line with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, group defaulting to nil (no group), and scene defaulting to (application-scene *app*).  The required arguments x0, y0, x1 and y1 are the endpoints of the line and must be real numbers."
  (scene-draw-2d-line scene group line-thickness color x0 y0 x1 y1))

(defun add-3d-line-primitive (x0 y0 z0 x1 y1 z1 &key (color *default-color*)
						  (line-thickness *default-line-thickness*)
						  (matrix nil)
						  (group nil)
						  (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, returns a handle.  Calls scene-add-3d-line-primitive with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, matrix defaulting to nil (identity), group defaulting to nil (no group), and scene defaulting to (application-scene *app*).  The required arguments x0, y0, z0, x1, y1 and z1 are the endpoints of the line and must be real numbers."
  (scene-add-3d-line-primitive scene group matrix line-thickness color x0 y0 z0 x1 y1 z1))

(defun add-3d-line (x0 y0 z0 x1 y1 z1 &key (color *default-color*)
                                        (line-thickness *default-line-thickness*)
                                        (group :default)
					(scene (application-scene *app*)))
  "Retained-mode function, returns no values.  Calls scene-add-3d-line with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, group defaulting to nil (no group), and scene defaulting to (application-scene *app*).  The required arguments x0, y0, z0, x1, y1 and z1 are the endpoints of the line and must be real numbers."
  (scene-add-3d-line scene group line-thickness color x0 y0 z0 x1 y1 z1))

(defun draw-3d-line (x0 y0 z0 x1 y1 z1 &key (color *default-color*)
                                         (line-thickness *default-line-thickness*)
                                         (group :default)
					 (scene (application-scene *app*)))
  "Immediate-mode function, returns no values.  Calls scene-draw-3d-line-primitive with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, matrix defaulting to nil (identity), group defaulting to nil (no group), and scene defaulting to (application-scene *app*).  The required arguments x0, y0, z0, x1, y1 and z1 are the endpoints of the line and must be real numbers."
  (scene-draw-3d-line scene group line-thickness color x0 y0 z0 x1 y1 z1))

(defun add-multicolor-2d-polyline-primitive (vertices &key (closed? nil)
                                                        (line-thickness *default-line-thickness*)
                                                        (matrix nil)
							(group nil)
							(scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, returns a handle.  Calls scene-add-multicolor-2d-polyline-primitive with closed? defaulting to nil, line-thickness defaulting to *default-line-thickness*, matrix defaulting to nil (identity), group defaulting to nil (no group), and scene defaulting to (application-scene *app*).  The required argument vertices should be of the form (list x0 y0 color0 x1 y1 color1 ... xn yn colorn) where the x and y values must be real numbers and the color value must be a color."
  (scene-add-multicolor-2d-polyline-primitive scene group matrix closed? line-thickness vertices))

(defun add-multicolor-2d-polyline (vertices &key (closed? nil)
                                              (line-thickness *default-line-thickness*)
                                              (group :default)
					      (scene (application-scene *app*)))
  "Retained-mode function, returns no values.  Calls scene-add-multicolor-2d-polyline with closed? defaulting to nil. line-thickness defaulting to *default-line-thickness*, group defaulting to :default, and scene defaulting to (application-scene *app*).  The required argument vertices should be of the form (list x0 y0 color0 x1 y1 color1 ... xn yn colorn) where the x and y values must be real numbers and the color value must be a color."
  (scene-add-multicolor-2d-polyline scene group closed? line-thickness vertices))

(defun draw-multicolor-2d-polyline (vertices &key (closed? nil)
                                               (line-thickness *default-line-thickness*)
                                               (group :default)
					       (scene (application-scene *app*)))
  "Immediate-mode function, returns no values.  Calls scene-draw-multicolor-2d-polyline with closed? defaulting to nil. line-thickness defaulting to *default-line-thickness*, group defaulting to :default, and scene defaulting to (application-scene *app*).  The required argument vertices should be of the form (list x0 y0 color0 x1 y1 color1 ... xn yn colorn) where the x and y values must be real numbers and the color value must be a color."
  (scene-draw-multicolor-2d-polyline scene group closed? line-thickness vertices))

(defun add-2d-polyline-primitive (vertices &key (closed? nil)
					     (color *default-color*)
					     (line-thickness *default-line-thickness*)
					     (matrix nil)
					     (group nil)
					     (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, returns a handle.  Calls scene-add-2d-polyline-primitive with closed? defaulting to nil, color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, matrix defaulting to nil (identity), group defaulting to nil (no group), and scene defaulting to (application-scene *app*).  The required argument vertices should be of the form (list x0 y0 x1 y1 ... xn yn) where the x and y values must be real numbers."
  (scene-add-2d-polyline-primitive scene group matrix closed? line-thickness color vertices))

(defun add-2d-polyline (vertices &key (closed? nil)
                                   (color *default-color*)
                                   (line-thickness *default-line-thickness*)
                                   (group :default)
				   (scene (application-scene *app*)))
  "Retained-mode function, returns no values.  Calls scene-add-2d-polyline with closed? defaulting to nil, color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, group defaulting to :default, and scene defaulting to (application-scene *app*). The required argument vertices should be of the form (list x0 y0 x1 y1 ... xn yn) where the x and y values must be real numbers."
  (scene-add-2d-polyline scene group closed? line-thickness color vertices))

(defun draw-2d-polyline (vertices &key (closed? nil)
                                    (color *default-color*)
                                    (line-thickness *default-line-thickness*)
                                    (group :default)
				    (scene (application-scene *app*)))
  "Immediate-mode function, returns no values.  Calls scene-draw-2d-polyline with closed? defaulting to nil, color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, group defaulting to :default, and scene defaulting to (application-scene *app*). The required argument vertices should be of the form (list x0 y0 x1 y1 ... xn yn) where the x and y values must be real numbers."
  (scene-draw-2d-polyline scene group closed? line-thickness color vertices))


(defun add-2d-triangle-primitive (x0 y0 x1 y1 x2 y2 &key
                                                      (color *default-color*)
                                                      (line-thickness *default-line-thickness*)
						      (matrix nil)
						      (group nil)
						      (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive outline of a triangle, returns a handle.  Calls scene-add-2d-triangle-primitive with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required arguments represent the vertices of the triangle and must be real numbers."
  (scene-add-2d-triangle-primitive scene group matrix line-thickness color x0 y0 x1 y1 x2 y2))

(defun add-2d-triangle (x0 y0 x1 y1 x2 y2 &key
                                            (color *default-color*)
                                            (line-thickness *default-line-thickness*)
                                            (group :default)
					    (scene (application-scene *app*)))
  "Retained-mode function, creates an outline of a triangle, returns no values.  Calls scene-add-2d-triangle with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required arguments represent the vertices of the triangle and must be real numbers."
  (scene-add-2d-triangle scene group line-thickness color x0 y0 x1 y1 x2 y2))


(defun draw-2d-triangle (x0 y0 x1 y1 x2 y2 &key (color *default-color*)
                                             (line-thickness *default-line-thickness*)
                                             (group :default)
					     (scene (application-scene *app*)))
  "Immediate-mode function, creates an outline of a triangle, returns no values.  Calls scene-draw-2d-triangle with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required arguments represent the vertices of the triangle and must be real numbers."
  (scene-draw-2d-triangle scene group line-thickness color x0 y0 x1 y1 x2 y2))

(defun add-2d-rectangle-primitive (x0 y0 x1 y1
                                   &key (color *default-color*)
                                     (line-thickness *default-line-thickness*)
                                     (matrix nil)
				     (group nil)
				     (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive outline of a rectangle, returns a handle.  Calls scene-add-2d-rectangle-primitive with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required arguments represent the top-left and bottom-right corners of the rectangle, and must be real numbers."
  (scene-add-2d-rectangle-primitive scene group matrix line-thickness color x0 y0 x1 y1))

(defun add-2d-rectangle (x0 y0 x1 y1
                         &key (color *default-color*)
                           (line-thickness *default-line-thickness*)
                           (group :default)
			   (scene (application-scene *app*)))
  "Retained-mode function, creates an outline of a rectangle, returns no values.  Calls scene-add-2d-rectangle with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*,  group defaulting to :default and scene defaulting to (application-scene *app*).  The required arguments represent the top-left and bottom-right corners of the rectangle, and must be real numbers."
  (scene-add-2d-rectangle scene group line-thickness color x0 y0 x1 y1))


(defun draw-2d-rectangle (x0 y0 x1 y1 &key (color *default-color*)
                                        (line-thickness *default-line-thickness*)
                                        (group :default)
					(scene (application-scene *app*)))
  "Immediate-mode function, creates an outline of a rectangle, returns no values.  Calls scene-draw-2d-rectangle with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*,  group defaulting to :default and scene defaulting to (application-scene *app*).  The required arguments represent the top-left and bottom-right corners of the rectangle, and must be real numbers."
  (scene-draw-2d-rectangle scene group line-thickness color x0 y0 x1 y1))

(defun add-2d-circular-arc-primitive (center-x center-y radius start-angle end-angle
                                      &key (closed? nil)
                                        (color *default-color*)
                                        (line-thickness *default-line-thickness*)
                                        (number-of-segments *default-number-of-segments*)
                                        (matrix nil)
					(group nil)
					(scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, a polyline representing the arc of a circle, returns a handle.  Calls scene-add-2d-circular-arc-primitive with closed? defaulting to nil, color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, number-of-segments defaulting to 64, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required arguments should be real numbers and start-angle and end-angle are in radians."
  (scene-add-2d-circular-arc-primitive scene group matrix closed? line-thickness color
                                       center-x center-y radius start-angle end-angle number-of-segments))

(defun add-2d-circular-arc (center-x center-y radius start-angle end-angle
                            &key (closed? nil)
                              (color *default-color*)
                              (line-thickness *default-line-thickness*)
                              (number-of-segments *default-number-of-segments*)
                              (group :default)
			      (scene (application-scene *app*)))
  "Retained-mode function, creates a polyline representing the arc of a circle, returns no values.  Calls scene-add-2d-circular-arc with closed? defaulting to nil, color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, number-of-segments defaulting to 64, group defaulting to :default and scene defaulting to (application-scene *app*).  The required arguments should be real numbers and start-angle and end-angle are in radians."
  (scene-add-2d-circular-arc scene group closed? line-thickness color
                             center-x center-y radius start-angle end-angle number-of-segments))

(defun draw-2d-circular-arc (center-x center-y radius start-angle end-angle
                             &key (closed? nil)
                               (color *default-color*)
                               (line-thickness *default-line-thickness*)
                               (number-of-segments *default-number-of-segments*)
                               (group :default)
			       (scene (application-scene *app*)))
  "Immediate-mode function, creates a polyline representing the arc of a circle, returns no values.  Calls scene-add-2d-circular-arc with closed? defaulting to nil, color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, number-of-segments defaulting to 64, group defaulting to :default and scene defaulting to (application-scene *app*).  The required arguments should be real numbers and start-angle and end-angle are in radians."
  (scene-draw-2d-circular-arc scene group closed? line-thickness color
                              center-x center-y radius start-angle end-angle number-of-segments))


(defun add-2d-circle-primitive (center-x center-y radius
                                &key (color *default-color*)
                                  (line-thickness *default-line-thickness*)
                                  (number-of-segments *default-number-of-segments*)
                                  (matrix nil)
				  (group nil)
				  (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, a polyline representing the outline of a circle, returns a handle.  Calls scene-add-2d-circle-primitive with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, number-of-segments defaulting to 64, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required arguments should be real numbers."
  (scene-add-2d-circle-primitive scene
                                 group matrix line-thickness color
                                 center-x center-y radius number-of-segments))

(defun add-2d-circle (center-x center-y radius
		      &key (color *default-color*)
			(line-thickness *default-line-thickness*)
			(number-of-segments *default-number-of-segments*)
                        (group :default)
			(scene (application-scene *app*)))
  "Retained-mode function, creates a  polyline representing the outline of a circle, returns a no values.  Calls scene-add-2d-circle with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, number-of-segments defaulting to 64,  group defaulting to :default and scene defaulting to (application-scene *app*).  The required arguments should be real numbers."
  (scene-add-2d-circle scene
		       group line-thickness color
		       center-x center-y radius number-of-segments))

(defun draw-2d-circle (center-x center-y radius
		       &key (color *default-color*)
			 (line-thickness *default-line-thickness*)
			 (number-of-segments *default-number-of-segments*)
                         (group :default)
			 (scene (application-scene *app*)))
  "Immediate-mode function, creates a  polyline representing the outline of a circle, returns a no values.  Calls scene-draw-2d-circle with color defaulting to *default-color*, line-thickness defaulting to *default-line-thickness*, number-of-segments defaulting to 64,  group defaulting to :default and scene defaulting to (application-scene *app*).  The required arguments should be real numbers."
  (scene-draw-2d-circle scene
			group line-thickness color
			center-x center-y radius number-of-segments))

(defun add-filled-2d-circle-primitive (center-x center-y radius
                                       &key (color *default-color*)
                                         (number-of-sectors *default-number-of-segments*)
					 (matrix nil)
					 (group nil)
					 (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, a filled 2d circle, returns a handle.  Calls scene-add-filled-2d-circle-primitive with color defaulting to *default-color*, number-of-sectors defaulting to 64, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required arguments should be real numbers."
  (scene-add-filled-2d-circle-primitive scene
					group matrix color center-x center-y radius number-of-sectors))

(defun add-filled-2d-circle (center-x center-y radius
                             &key (color *default-color*)
                               (number-of-sectors *default-number-of-segments*)
                               (group :default)
			       (scene (application-scene *app*)))
  "Retained-mode function, creates  a filled 2d circle, returns no values.  Calls scene-add-filled-2d-circle with color defaulting to *default-color*, number-of-sectors defaulting to 64,  group defaulting to :default and scene defaulting to (application-scene *app*).  The required arguments should be real numbers."
  (scene-add-filled-2d-circle scene
                              group color center-x center-y radius number-of-sectors))

(defun draw-filled-2d-circle (center-x center-y radius
                              &key (color *default-color*)
                                (number-of-sectors *default-number-of-segments*)
                                (group :default)
				(scene (application-scene *app*)))
  "Immediate-mode function, creates  a filled 2d circle, returns no values.  Calls scene-draw-filled-2d-circle with color defaulting to *default-color*, number-of-sectors defaulting to 64,  group defaulting to :default and scene defaulting to (application-scene *app*).  The required arguments should be real numbers."
  (scene-draw-filled-2d-circle scene group color center-x center-y radius number-of-sectors))


(defun add-multicolor-3d-polyline-primitive (vertices &key (closed? nil)
                                                        (line-thickness *default-line-thickness*)
							(matrix nil)
							(group nil)
							(scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, a multicolored 3d polyline, returns a handle.  Calls scene-add-multicolored-3d-polyline-primitive with closed? defaulting to nil, line-thickness defaulting to *default-line-thickness*, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 z0 color0 x1 y1 z1 color1 ... xn yn zn colorn) where the x y and z values should be real numbers and the color values should represent a color."
  (scene-add-multicolor-3d-polyline-primitive scene group matrix closed? line-thickness vertices))

(defun add-multicolor-3d-polyline (vertices &key (closed? nil)
                                              (line-thickness *default-line-thickness*)
                                              (group :default)
					      (scene (application-scene *app*)))
  "Retained-mode function, creates a multicolored 3d polyline, returns a no values.  Calls scene-add-multicolored-3d-polyline with closed? defaulting to nil, line-thickness defaulting to *default-line-thickness*,  group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 z0 color0 x1 y1 z1 color1 ... xn yn zn colorn) where the x y and z values should be real numbers and the color values should represent a color."
  (scene-add-multicolor-3d-polyline scene group closed? line-thickness vertices))

(defun draw-multicolor-3d-polyline (vertices &key (closed? nil)
                                               (line-thickness *default-line-thickness*)
                                               (group :default)
					       (scene (application-scene *app*)))
  "Immediate-mode function, creates a multicolored 3d polyline, returns a no values.  Calls scene-draw-multicolored-3d-polyline with closed? defaulting to nil, line-thickness defaulting to *default-line-thickness*,  group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 z0 color0 x1 y1 z1 color1 ... xn yn zn colorn) where the x y and z values should be real numbers and the color values should represent a color."
  (scene-draw-multicolor-3d-polyline scene group closed? line-thickness vertices))


(defun add-3d-polyline-primitive (vertices &key (color *default-color*)
                                             (closed? nil)
                                             (line-thickness *default-line-thickness*)
					     (matrix nil)
					     (group nil)
					     (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, a 3d polyline, returns a handle.  Calls scene-add-3d-polyline-primitive with color defaulting to *default-color*, closed? defaulting to nil, line-thickness defaulting to *default-line-thickness*, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn) where the x y and z values should be real numbers."
  (scene-add-3d-polyline-primitive scene group matrix closed? line-thickness color vertices))

(defun add-3d-polyline (vertices &key (color *default-color*)
                                   (closed? nil)
                                   (line-thickness *default-line-thickness*)
                                   (group :default)
				   (scene (application-scene *app*)))
  "Retained-mode function, creates a 3d polyline, returns a no values.  Calls scene-add-3d-polyline with color defaulting to *default-color*, closed? defaulting to nil, line-thickness defaulting to *default-line-thickness*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn) where the x y and z values should be real numbers."
  (scene-add-3d-polyline scene group closed? line-thickness color vertices))

(defun draw-3d-polyline (vertices &key (color *default-color*)
                                    (closed? nil)
                                    (line-thickness *default-line-thickness*)
                                    (group :default)
				    (scene (application-scene *app*)))
  "Immediate-mode function, creates a 3d polyline, returns a no values.  Calls scene-draw-3d-polyline with color defaulting to *default-color*, closed? defaulting to nil, line-thickness defaulting to *default-line-thickness*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn) where the x y and z values should be real numbers."
  (scene-draw-3d-polyline scene group closed? line-thickness color vertices))


(defun add-filled-2d-triangle-list-primitive (vertices &key (color *default-color*)
                                                         (matrix nil)
							 (group nil)
							 (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, a series of filled 2d triangles, returns a handle. Calls scene-add-filled-2d-triangle-list-primitive with color defaulting to *default-color*, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 x10 y10 x20 y20 x01 y01 x11 y11 x21 y21 ... x0n y0n x1n y1n x2n y2n) where the x and y values represent vertices of a triangle in a series of triangles."
  (scene-add-filled-2d-triangle-list-primitive scene group matrix color vertices))

(defun add-filled-2d-triangle-list (vertices &key (color *default-color*)
					       (group :default)
					       (scene (application-scene *app*)))
  "Retained-mode function, creates a series of filled 2d triangles, returns a no values.  Calls scene-add-filled-2d-triangle-list with color defaulting to *default-color*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 x10 y10 x20 y20 x01 y01 x11 y11 x21 y21 ... x0n y0n x1n y1n x2n y2n) where the x and y values represent vertices of a triangle in a series of triangles."
  (scene-add-filled-2d-triangle-list scene group color vertices))

(defun draw-filled-2d-triangle-list (vertices &key (color *default-color*)
                                                (group :default)
						(scene (application-scene *app*)))
  "Immediate-mode function, creates a series of filled 2d triangles, returns a no values.  Calls scene-draw-2d-triangle-list with color defaulting to *default-color*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00  x10 y10 x20 y20 x01 y01 x11 y11 x21 y21 ... x0n y0n x1n y1 x2n y2n) where the x and y values represent vertices of a triangle in a series of triangles."
  (scene-draw-filled-2d-triangle-list scene group color vertices))


(defun add-filled-2d-rectangle-list-primitive (vertices &key (color *default-color*)
                                                          (matrix nil)
							  (group nil)
							  (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, a series of filled 2d rectangles, returns a handle.  Calls scene-add-2d-rectangle-list-primitive with color defaulting to *default-color*, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 x10 y10 x01 y01 x11 y11 ... x0n y0n x1n y1n) where the x0's, and y0's and the x1's and y1's represent the top-left and bottom-right of a series of rectangles."
  (scene-add-filled-2d-rectangle-list-primitive scene group matrix color vertices))

(defun add-filled-2d-rectangle-list (vertices &key (color *default-color*)
						(group :default)
						(scene (application-scene *app*)))
  "Retained-mode function a series of filled 2d rectangles, returns no values.  Calls scene-add-2d-rectangle-list with color defaulting to *default-color*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 x10 y10 x01 y01 x11 y11 ... x0n y0n x1n y1n) where the x0's, and y0's and the x1's and y1's represent the top-left and bottom-right of a series of rectangles."
  (scene-add-filled-2d-rectangle-list scene group color vertices))

(defun draw-filled-2d-rectangle-list (vertices &key (color *default-color*)
						 (group :default)
						 (scene (application-scene *app*)))
  "Immediate-mode function a series of filled 2d rectangles, returns no values.  Calls scene-draw-2d-rectangle-list with color defaulting to *default-color*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 x10 y10 x01 y01 x11 y11 ... x0n y0n x1n y1n) where the x0's, and y0's and the x1's and y1's represent the top-left and bottom-right of a series of rectangles."
  (scene-draw-filled-2d-rectangle-list scene group color vertices))


(defun add-textured-2d-rectangle-list-primitive (vertices &key (color *default-color*)
                                                            (texture *white-texture*)
							    (matrix nil)
							    (group nil)
							    (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, a series of textured 2d rectangles, returns a handle.  Calls scene-add-textured-2d-rectangle-list-primitive with color defaulting to *default-color*, texture defaulting to *white-texture*, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 u00 v00 x10 y10 u10 v10 x01 y01 u01 v01 x11 y11 u11 v11 ... x0n y0n u0n v0n x1n y1n u1n v1n) where the x0's, and y0's and the x1's and y1's represent the top-left and bottom-right of a series of rectangles and the u0's and v0's are the normalized texture coordinates for the top-left corner and the u1's and v1's are the normalized texture coordinates for the bottom-right corner of each rectangle."
  (scene-add-textured-2d-rectangle-list-primitive scene group matrix texture color vertices))

(defun add-textured-2d-rectangle-list (vertices &key (color *default-color*)
                                                  (texture *white-texture*)
                                                  (group :default)
						  (scene (application-scene vk::*app*)))
  "Retained-mode function, creates  a series of textured 2d rectangles, returns no values.  Calls scene-add-textured-2d-rectangle-list with color defaulting to *default-color*, texture defaulting to *white-texture*, group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 u00 v00 x10 y10 u10 v10 x01 y01 u01 v01 x11 y11 u11 v11 ... x0n y0n u0n v0n x1n y1n u1n v1n) where the x0's, and y0's and the x1's and y1's represent the top-left and bottom-right of a series of rectangles and the u0's and v0's are the normalized texture coordinates for the top-left corner and the u1's and v1's are the normalized texture coordinates for the bottom-right corner. of each rectangle."
  (scene-add-textured-2d-rectangle-list scene group texture color vertices))

(defun draw-textured-2d-rectangle-list (vertices &key (color *default-color*)
                                                   (texture *white-texture*)
                                                   (group :default)
						   (scene (application-scene vk::*app*)))
  "Immediate-mode function, creates  a series of textured 2d rectangles, returns no values.  Calls scene-draw-textured-2d-rectangle-list with color defaulting to *default-color*, texture defaulting to *white-texture*, group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 u00 v00 x10 y10 u10 v10 x01 y01 u01 v01 x11 y11 u11 v11 ... x0n y0n u0n v0n x1n y1n u1n v1n) where the x0's, and y0's and the x1's and y1's represent the top-left and bottom-right of a series of rectangles and the u0's and v0's are the normalized texture coordinates for the top-left corner and the u1's and v1's are the normalized texture coordinates for the bottom-right corner. of each rectangle."
  (scene-draw-textured-2d-rectangle-list scene group texture color vertices))

(defun add-filled-2d-convex-polygon-primitive (vertices &key
							  (color *default-color*)
							  (matrix nil)
							  (group nil)
							  (scene (application-scene vk::*app*)))
  "Retained-mode function, creates a primitive, a filled 2d convex polygon, returns a handle.  Calls scene-add-filled-2d-convex-polygon-primitive with color defaulting to *default-color*, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 x1 y1 ... xn yn) where the x's, and y's represent a vertex of the polygon."
  (scene-add-filled-2d-convex-polygon-primitive scene group matrix color vertices))

(defun add-filled-2d-convex-polygon (vertices &key (color *default-color*)
                                                (group :default)
						(scene (application-scene *app*)))
  "Retained-mode function, creates a filled 2d convex polygon, returns a no values.  Calls scene-add-filled-2d-convex-polygon with color defaulting to *default-color*, group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 x1 y1 ... xn yn) where the x's, and y's represent a vertex of the polygon."
  (scene-add-filled-2d-convex-polygon scene group color vertices))

(defun draw-filled-2d-convex-polygon (vertices &key
						 (color *default-color*)
                                                 (group :default)
						 (scene (application-scene *app*)))
  "Immediate-mode function, creates a filled 2d convex polygon, returns a no values.  Calls scene-draw-filled-2d-convex-polygon with color defaulting to *default-color*, group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 x1 y1 ... xn yn) where the x's, and y's represent a vertex of the polygon."
  (scene-draw-filled-2d-convex-polygon scene group color vertices))

(defun add-filled-3d-triangle-list-primitive (vertices &key
                                                         (color *default-color*)
							 (shading-style :diffuse)
							 (light-position nil)
                                                         (matrix nil)
							 (group nil)
							 (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, a filled 3d triangle list, returns a handle.  Calls scene-add-filled-3d-triangle-list-primitive-flat or scene-add-filled-3d-triangle-list-primitive-diffuse depending on whether shading-style is :diffuse or :flat, with color defaulting to *default-color*, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 z00 x10 y10 z10 x20 y20 z20 x01 y01 z01 x11 y11 z11 x21 y21 z21... x0n y0n z0n x1n y1n z1n x2n y2n z2n) where the x, y and z values represent vertices of a triangle in a series of triangles.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is up."
  (ecase shading-style
    (:flat (scene-add-filled-3d-triangle-list-primitive-flat scene group matrix color vertices))
    (:diffuse (scene-add-filled-3d-triangle-list-primitive-diffuse scene group matrix color vertices light-position))))

(defun add-filled-3d-triangle-list (vertices &key (color *default-color*)
                                               (shading-style :diffuse)
                                               (group :default)
					       (scene (application-scene *app*)))
  "Retained-mode function, creates a filled 3d triangle list, returns a no-values.  Calls scene-add-filled-3d-triangle-list-flat or scene-add-filled-3d-triangle-list-diffuse depending on whether shading-style is :diffuse or :flat, with color defaulting to *default-color*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 z00 x10 y10 z10 x20 y20 z20 x01 y01 z01 x11 y11 z11 x21 y21 z21... x0n y0n z0n x1n y1n z1n x2n y2n z2n) where the x, y and z values represent vertices of a triangle in a series of triangles.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is up."
  (ecase shading-style
    (:flat (scene-add-filled-3d-triangle-list-flat scene group color vertices))
    (:diffuse (scene-add-filled-3d-triangle-list-diffuse scene group color vertices))))

(defun draw-filled-3d-triangle-list (vertices &key
                                                (color *default-color*)
						(shading-style :diffuse)
                                                (group :default)
						(scene (application-scene *app*)))
  "Immediate-mode function, creates a filled 3d triangle list, returns a no-values.  Calls scene-draw-filled-3d-triangle-list-flat or scene-draw-filled-3d-triangle-list-diffuse depending on whether shading-style is :diffuse or :flat, with color defaulting to *default-color*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 z00 x10 y10 z10 x20 y20 z20 x01 y01 z01 x11 y11 z11 x21 y21 z21... x0n y0n z0n x1n y1n z1n x2n y2n z2n) where the x, y and z values represent vertices of a triangle in a series of triangles.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is up."
  (ecase shading-style
    (:flat (scene-draw-filled-3d-triangle-list-flat scene group color vertices))
    (:diffuse (scene-draw-filled-3d-triangle-list-diffuse scene group color vertices))))

(defun add-filled-3d-triangle-strip-primitive (vertices &key
							  (color *default-color*)
							  (shading-style :diffuse)
							  (light-position nil)
							  (matrix nil)
							  (group nil)
							  (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, a filled 3d triangle strip, returns a handle.  Calls scene-add-filled-3d-triangle-strip-primitive-flat or scene-add-filled-3d-triangle-strip-primitive-diffuse depending on whether shading-style is :diffuse or :flat, with color defaulting to *default-color*, group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 z0 x1 y1 z1 x2 y2 z2 ... xn yn zn) where the x, y and z values represent vertices of a triangle in a strip of triangles."
  (ecase shading-style
    (:flat (scene-add-filled-3d-triangle-strip-primitive-flat scene group matrix color vertices))
    (:diffuse (scene-add-filled-3d-triangle-strip-primitive-diffuse scene group matrix color vertices
								    light-position))))

(defun draw-filled-3d-triangle-strip (vertices &key
                                                 (color *default-color*)
                                                 (shading-style :diffuse)
                                                 (group :default)
						 (scene (application-scene *app*)))
  "Immediate-mode function, creates a filled 3d triangle strip, returns a no values.  Calls scene-draw-filled-3d-triangle-strip-flat or scene-draw-filled-3d-triangle-strip-diffuse depending on whether shading-style is :diffuse or :flat, with color defaulting to *default-color*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 z0 x1 y1 z1 x2 y2 z2 ... xn yn zn) where the x, y and z values represent vertices of a triangle in a strip of triangles."
  (ecase shading-style
    (:flat (scene-draw-filled-3d-triangle-strip-flat scene group color vertices))
    (:diffuse (scene-draw-filled-3d-triangle-strip-diffuse scene group color vertices))))


(defun add-textured-3d-triangle-list-primitive (vertices &key (color *default-color*)
                                                           (texture *white-texture*)
                                                           (shading-style :diffuse)
                                                           (light-position nil)
                                                           (matrix nil)
							   (group nil)
							   (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, a textured 3d triangle list, returns a handle.  Calls scene-add-textured-3d-triangle-list-primitive-flat when shading-style is :flat, currently errors with shading-style :diffuse, with color defaulting to *default-color*, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 z00 u00 v00 x10 y10 z10 u01 v01 x20 y20 z20 u20 v20 x01 y01 z01 u01 v01 x11 y11 z11 x21 u11 v11 y21 z21 u21 v21... x0n y0n z0n u0n v0n x1n y1n z1n u1n v1n x2n y2n z2n u2n v2n) where the x, y and z values represent vertices of a triangle in a series of triangles and the u's and v's represent the normalized texture coordinates at that vertex.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is up."
  (declare (ignore light-position))
  (ecase shading-style
    (:flat (scene-add-textured-3d-triangle-list-primitive-flat scene group matrix texture color vertices))))

(defun add-textured-3d-triangle-list (vertices &key (color *default-color*)
                                                 (texture *white-texture*)
                                                 (shading-style :diffuse)
                                                 (group :default)
						 (scene (application-scene *app*)))
  "Retained-mode function, creates a textured 3d triangle list, returns no-values.  Calls scene-add-textured-3d-triangle-list-flat when shading-style is :flat, currently errors with shading-style :diffuse, with color defaulting to *default-color*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 z00 u00 v00 x10 y10 z10 u01 v01 x20 y20 z20 u20 v20 x01 y01 z01 u01 v01 x11 y11 z11 x21 u11 v11 y21 z21 u21 v21... x0n y0n z0n u0n v0n x1n y1n z1n u1n v1n x2n y2n z2n u2n v2n) where the x, y and z values represent vertices of a triangle in a series of triangles and the u's and v's represent the normalized texture coordinates at that vertex.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is up."
  (ecase shading-style
    (:flat (scene-add-textured-3d-triangle-list-flat
            scene group texture color vertices))))

(defun draw-textured-3d-triangle-list (vertices &key (color *default-color*)
                                                  (texture *white-texture*)
                                                  (shading-style :diffuse)
                                                  (group :default)
						  (scene (application-scene *app*)))
  "Immediate-mode function, creates a textured 3d triangle list, returns no-values.  Calls scene-draw-textured-3d-triangle-list-flat when shading-style is :flat, currently errors with shading-style :diffuse, with color defaulting to *default-color*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x00 y00 z00 u00 v00 x10 y10 z10 u01 v01 x20 y20 z20 u20 v20 x01 y01 z01 u01 v01 x11 y11 z11 x21 u11 v11 y21 z21 u21 v21... x0n y0n z0n u0n v0n x1n y1n z1n u1n v1n x2n y2n z2n u2n v2n) where the x, y and z values represent vertices of a triangle in a series of triangles and the u's and v's represent the normalized texture coordinates at that vertex.  Vertices should be oriented counter clockwise, according to the right-hand-rule, so that the front face is up."
  (ecase shading-style
    (:flat (scene-draw-textured-3d-triangle-list-flat scene group texture color vertices))))

(defun add-textured-3d-triangle-strip-primitive (vertices &key
                                                            (color *default-color*)
							    (texture *white-texture*)
							    (shading-style :diffuse)
							    (light-position nil)
							    (matrix nil)
							    (group nil)
							    (scene (application-scene *app*)))
  "Retained-mode function, creates a primitive, a textured 3d triangle strip, returns a handle.  Calls scene-add-textured-3d-triangle-strip-primitive-flat when shading-style is :flat, currently errors with shading-style :diffuse, with color defaulting to *default-color*, group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 z0 u0 v0 x1 y1 z1u1 v1  x2 y2 z2 u2 v2... xn yn zn un vn) where the x, y and z values represent successive vertices of a triangle in a strip of triangles, and the u and v values represent the normalized texture coordinates at the corresponding x, y and z.  vertices must contain at least three vertices."
  (declare (ignore light-position))
  (ecase shading-style
    (:flat (scene-add-textured-3d-triangle-strip-primitive-flat scene group matrix texture color vertices))))

(defun draw-textured-3d-triangle-strip (vertices &key
                                                   (color *default-color*)
                                                   (texture *white-texture*)
                                                   (shading-style :diffuse)
                                                   (group :default)
						   (scene (application-scene *app*)))
  "Immediate-mode function, creates a textured 3d triangle strip, returns a no values.  Calls scene-draw-textured-3d-triangle-strip-primitive-flat when shading-style is :flat, currently errors with shading-style :diffuse, with color defaulting to *default-color*, group defaulting to :default and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 z0 u0 v0 x1 y1 z1u1 v1  x2 y2 z2 u2 v2... xn yn zn un vn) where the x, y and z values represent successive vertices of a triangle in a strip of triangles, and the u and v values represent the normalized texture coordinates at the corresponding x, y and z.  vertices must contain at least three vertices."
  (ecase shading-style
    (:flat (scene-draw-textured-3d-triangle-strip-flat scene group texture color vertices))))

(defun add-filled-3d-convex-polygon-primitive (vertices &key
							  (color *default-color*)
							  (shading-style :diffuse)
							  (light-position nil)
							  (matrix nil)
							  (group nil)
							  (scene (application-scene vk::*app*)))
  "Retained-mode function, creates a primitive, a filled 3d convex polygon, returns a handle.  Calls scene-add-filled-3d-convex-polygon-primitive-diffuse or scene-draw-filled-3d-convex-polygon-flat, depending on whether shading style is :diffuse or :flat, light-position defaults to nil, color defaulting to *default-color*, matrix defaulting to nil (identity), group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 z0 x1 y1 z1 ... xn yn zn) where the x, y and z's represent a vertex of the polygon."
  (ecase shading-style
    (:diffuse (scene-add-filled-3d-convex-polygon-primitive-diffuse scene group matrix color vertices light-position))
    (:flat (scene-add-filled-3d-convex-polygon-primitive-flat scene group matrix color vertices))))

(defun add-filled-3d-convex-polygon (vertices &key (color *default-color*)
						(shading-style :diffuse)
                                                (group :default)
						(scene (application-scene *app*)))
  "Retained-mode function, creates a filled 2d convex polygon, returns no values.  Calls scene-add-filled-3d-convex-polygon-diffuse or scene-add-filled-3d-convex-polygon-flat, depending on whether shading-style is :diffuse or :flat with color defaulting to *default-color*, group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 x1 y1 ... xn yn) where the x's, and y's represent a vertex of the polygon."
  (ecase shading-style
    (:diffuse (scene-add-filled-3d-convex-polygon-diffuse scene group color vertices))
    (:flat (scene-add-filled-3d-convex-polygon-flat scene group color vertices))))

(defun draw-filled-3d-convex-polygon (vertices &key
						 (color *default-color*)
						 (shading-style :diffuse)
                                                 (group :default)
						 (scene (application-scene *app*)))
  "Immediate-mode function, creates a filled 3d convex polygon, returns no values.  Calls scene-draw-filled-3d-convex-polygon-diffuse or scene-draw-filled-3d-convex-polygon-flat, depending on whether shading-style is :diffuse or :flat, with color defaulting to *default-color*, group defaulting to nil (no group) and scene defaulting to (application-scene *app*).  The required argument, vertices, should be of the form (list x0 y0 x1 y1 ... xn yn) where the x's, and y's represent a vertex of the polygon."
  (ecase shading-style
    (:diffuse (scene-draw-filled-3d-convex-polygon-diffuse scene group color vertices))
    (:flat (scene-draw-filled-3d-convex-polygon-flat scene group color vertices))))

(defun add-filled-sphere-primitive (origin-x origin-y origin-z radius &key (color *default-color*)
									(resolution 64)
									(shading-style :diffuse)
									(light-position nil)
									(matrix nil)
									(group nil)
									(scene (application-scene *app*)))
  "Retained-mode function, creates a primitive of a filled sphere, returns a handle.  Calls scene-add-filled-sphere-primitive-diffuse when shading style is :diffuse, currently errors with any other shading style, with color defaulting to *default-color*, resolution defaulting to 64, light-position defaulting to nil, matrix defaulting to nil (identity), group defaulting to nil (no group), and scene defaulting to (application-scene *app*).  The required arguments should be real numbers. radius should be positive."
  (ecase shading-style
    (:diffuse (scene-add-filled-sphere-primitive-diffuse scene
							 group matrix color
							 origin-x origin-y origin-z radius
							 light-position resolution))))

(defun add-filled-sphere (origin-x origin-y origin-z radius &key (color *default-color*)
                                                              (resolution 64)
                                                              (shading-style :diffuse)
                                                              (group :default)
							      (scene (application-scene *app*)))
  "Retained-mode function, creates a filled sphere, returns a no values.  Calls scene-add-filled-sphere-diffuse when shading style is :diffuse, currently errors with any other shading style, with color defaulting to *default-color*, resolution defaulting to 64, group defaulting to :default, and scene defaulting to (application-scene *app*).  The required arguments should be real numbers.   radius should be positive."
  (ecase shading-style
    (:diffuse (scene-add-filled-sphere-diffuse scene
					       group color
					       origin-x origin-y origin-z radius resolution))))

(defun draw-filled-sphere (origin-x origin-y origin-z radius &key (color *default-color*)
                                                               (resolution 64)
                                                               (shading-style :diffuse)
                                                               (group :default)
							       (scene (application-scene *app*)))
  "Immediate-mode function, creates a filled sphere, returns a no values.  Calls scene-draw-filled-sphere-diffuse when shading style is :diffuse, currently errors with any other shading style, with color defaulting to *default-color*, resolution defaulting to 64, group defaulting to :default, and scene defaulting to (application-scene *app*).  The required arguments should be real numbers.   radius should be positive."
  (ecase shading-style
    (:diffuse (scene-draw-filled-sphere-diffuse scene
                                                group
						color
						origin-x origin-y origin-z radius
						resolution))))




(defun add-text-primitive (string pos-x pos-y &key (color *default-color*)
						(font *font*)
						(matrix nil)
						(group nil)
						(scene (application-scene *app*)))
  "Retained-mode function, creates a primitive of a text string, returns a handle.  Calls scene-add-text-primitive with color defaulting to *default-color*, font defaulting to *font*, matrix defaulting to nil (identity), group defaulting to nil (no group), and scene defaulting to (application-scene *app*).  The required arguments should be real numbers.  pos-x and pos-y represent the upper left corner of the text."
  (scene-add-text-primitive scene group matrix font color pos-x pos-y string))

(defun add-text (string pos-x pos-y &key (color *default-color*)
                                      (font *font*)
                                      (group :default)
				      (scene (application-scene *app*)))
  "Retained-mode function, creates text, returns a no values.  Calls scene-add-text with color defaulting to *default-color*, font defaulting to *font*, group defaulting to :default, and scene defaulting to (application-scene *app*).  The required arguments should be real numbers.  pos-x and pos-y represent the upper left corner of the text."
  (scene-add-text scene group font color pos-x pos-y string))

(defun draw-text (string pos-x pos-y &key (color *default-color*)
                                       (font *font*)
                                       (group :default)
				       (scene (application-scene *app*)))
  "Immediate-mode function, creates text, returns a no values.  Calls scene-draw-text with color defaulting to *default-color*, font defaulting to *font*, group defaulting to :default, and scene defaulting to (application-scene *app*).  The required arguments should be real numbers.  pos-x and pos-y represent the upper left corner of the text."
  (scene-draw-text scene group font color pos-x pos-y string))

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
      (sdf-bmfont:create-bmfont #+linux "/usr/share/fonts/liberation-mono/LiberationMono-Regular.ttf"
				#+darwin "/System/Library/Fonts/Monaco.ttf"
                                #+windows "C:/Windows/Fonts/Arial.ttf" "acache.json"
                                :size 16 :mode :msdf+a :type :json :spread 8))
    #-(or linux darwin)
    (unless (probe-file (asdf/system:system-relative-pathname :krma "tcache.json"))
      (sdf-bmfont:create-bmfont "C:/Windows/Fonts/Times.ttf" "tcache.json" :size 16 :mode :msdf+a :type :json :spread 8))


    (setq *font*
          (vulkan-make-font device queue sampler texture-dsl descriptor-pool command-buffer
                            :cache-file "acache.json"))
    #-(or linux darwin)
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

                (loop with lambda = nil
                      while (setq lambda (sb-concurrency:dequeue
                                          (draw-data-deletion-queue rm-draw-data)))
                      do (funcall lambda))

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

(defun run ()
  #+darwin(sb-int:set-floating-point-modes :traps nil)
  (let ((app (make-instance 'krma-test-application)))
    (main app)))
