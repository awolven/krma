(in-package :krma)

(defclass krma-view-mixin ()
  ((parent :initform nil :initarg :parent :accessor window-parent)
   (x :initarg :x :initform 0)
   (y :initarg :y :initform 0)
   (width :initarg :width :initform 200)
   (height :initarg :height :initform 100)
   (bgcolor :initform #x787878ff :accessor window-background-color)
   (children :initform nil :accessor window-children)))

(defvar *object-id-counter* 0)

(defun new-object-id ()
  (incf *object-id-counter*))

(defclass homemade-window (krma-view-mixin)
  ((group :initform :default :initarg :group :accessor window-group)
   (obj-id :initform (new-object-id) :reader object-id)
   (prim-ids :initform () :accessor window-primitive-ids)))

(defmethod window-position ((window homemade-window))
  (values (slot-value window 'x) (slot-value window 'y)))

(defmethod window-size ((window homemade-window))
  (values (slot-value window 'width) (slot-value window 'height)))

(defvar *object-id->object-table*
  (make-hash-table :test #'eq))

(defun (setf object-from-id) (object id)
  (setf (gethash id *object-id->object-table*) object))

(defun object-from-id (id)
  (gethash id *object-id->object-table*))

(defun scene-add-homemade-window (scene window)
  (multiple-value-bind (width height) (window-size window)
    (multiple-value-bind (x y) (window-position window)
	
	(let ((color (window-background-color window))
	      (model-matrix (nmtranslate (meye 4) (vec3 x y 0))))
      
	  (push
	   (scene-add-filled-2d-rectangle-list-primitive
	    scene (window-group window) model-matrix color (list 0 0 width height) (object-id window))
	   (window-primitive-ids window))
	  (values)))))

(defun make-test-window ()
  (let ((w (make-instance 'homemade-window :x 25 :y 50
					   :width 200 :height 100
					   :parent (main-window *app*)
					   :group :test-window)))
    (setf (object-from-id (object-id w)) w)
    w))

(defmethod clui:handle-event ((window homemade-window) event)
  (call-next-method)
  ;;(print (clui::event-timestamp event))
  ;;(finish-output)
  )




		      
  
