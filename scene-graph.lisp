(in-package :krma)

(defun safe-euclid (vec4)
  (if (<= (abs (vw vec4)) single-float-epsilon)
      (vec3 (vx vec4) (vy vec4) (vz vec4))
      (vec3 (/ (vx vec4) (vw vec4))
	    (/ (vy vec4) (vw vec4))
	    (/ (vz vec4) (vw vec4)))))

(defclass node-mixin ()
  ((parent :initarg :parent :initform nil :accessor node-parent)
   (children :initarg :children :initform nil :accessor node-children)
   (scene :initarg :scene :initform nil :accessor node-scene)))

(defmethod model-matrix ((node node-mixin))
  nil)

(defvar *object-id-counter* 0)

(defun new-object-id ()
  (incf *object-id-counter*))

(defvar *object-id->object-table*
  (make-hash-table :test #'eq :weak t))

(defun (setf object-from-id) (object id)
  (setf (gethash id *object-id->object-table*) object))

(defun object-from-id (id)
  (gethash id *object-id->object-table*))

(defmethod destroy-object (object)
  (remhash (object-id object) *object-id->object-table*))

(defclass selectable-mixin ()
  ((obj-id :initform (new-object-id) :reader object-id)))

(defmethod initialize-instance :after ((object selectable-mixin) &rest initargs)
  (declare (ignorable initargs))
  (setf (object-from-id (object-id object)) object)
  (values))

(defclass primitive-owner-mixin ()
  ((prim-handles :initform () :accessor object-primitive-handles)))

(defmethod delete-object-from-scene ((object primitive-owner-mixin))
  (if (next-method-p)
      (call-next-method)
      (let ((scene (node-scene object)))
	(delete-primitives scene (object-primitive-handles object))
	(setf (object-primitive-handles object) nil)
	(values))))

(defclass group-mixin ()
  ((group :initform :default :initarg :group :accessor object-group)))

(defvar *group-counter* 0)

(defun new-group ()
  (incf *group-counter*))

(defclass group-owner-mixin (group-mixin)
  ((matrix :initarg :matrix :initform (meye 4) :accessor model-matrix)
   (group :initform (new-group))))

(defmethod initialize-instance :after ((node group-owner-mixin) &rest initargs)
  (declare (ignore initargs))
  (scene-ensure-group (node-scene node) (object-group node))
  (group-set-model-matrix (node-scene node) (object-group node) (global-model-matrix node)))

(defmethod delete-object-from-scene ((object group-owner-mixin))
  (delete-group (node-scene object) (object-group object)))

(defmethod translate-node ((node node-mixin) (vec vec3))
  (error "Cannot translate node.  Node is static wrt group."))

(defmethod translate-node ((node group-owner-mixin) (vec vec3))
  (let ((matrix (model-matrix node)))
    (nmtranslate matrix vec)
    (group-set-model-matrix (node-scene node) (object-group node) (global-model-matrix node))
    ;; todo: group-set-model-matrix of all child group-owner-mixins
    (values)))

(defmethod global-model-matrix ((node group-owner-mixin))
  (m* (model-matrix node) (global-model-matrix (node-parent node))))

(defmethod global-model-matrix ((node node-mixin))
  (let ((parent (node-parent node)))
    (if parent
	(global-model-matrix (node-parent node))
	(meye 4))))

(defmethod group-owner ((self group-owner-mixin))
  self)

(defmethod group-owner ((node node-mixin))
  (group-owner (node-parent node)))

(defgeneric transform-coordinate (vec node)
  (:documentation "Return a vector which is the transformation of vec as represented in node's coordinates into the parent coordinate system."))

(defmethod transform-coordinate ((vec vec3) (node group-owner-mixin))
  (let ((vec4 (m* (vec4 (vx vec) (vy vec) (vz vec) 1) (model-matrix node))))
    (safe-euclid vec4)))

(defmethod transform-coordinate ((vec vec3) (node node-mixin))
  (transform-coordinate vec (node-parent node)))

(defmethod transform-coordinate ((vec vec3) (node null))
  vec)

(defgeneric untransform-coordinate (vec node)
  (:documentation "Take vec, which is a vector represented in the parent of the group owner's coordinate system, and untransform it into this node's coordinate system."))

(defmethod untransform-coordinate ((vec vec3) (node group-owner-mixin))
  (let ((vec4 (m* (vec4 (vx vec) (vy vec) (vz vec) 1) (minv (model-matrix node)))))
    (safe-euclid vec4)))

(defmethod untransform-coordinate ((vec vec3) (node node-mixin))
  (untransform-coordinate vec (node-parent node)))

(defmethod untransform-coordinate ((vec vec3) (node null))
  vec)
