(in-package :krma)

(eval-when (:compile-toplevel :load-toplevel)
  (when krma::*debug*
    (declaim (optimize (safety 3) (debug 3)))))

(defclass draw-list-mixin ()
  ((index-array
    :accessor draw-list-index-array
    :initform (make-index-array)
    :initarg :index-array)
   (vertex-array
    :reader draw-list-vertex-array
    :initarg :vertex-array)
   (cmd-vector
    :initform (make-array +draw-list-alloc-size+ :adjustable t :fill-pointer 0)
    :reader draw-list-cmd-vector
    :initarg :cmd-vector)
   (changed?
    :initform t
    :accessor draw-list-changed?)
   (needs-compaction?
    :initform nil
    :accessor draw-list-needs-compaction?)
   (index-memory
    :accessor draw-list-index-memory
    :initform nil
    :initarg :index-memory)
   (index-size-aligned
    :accessor draw-list-index-size-aligned
    :initform nil
    :initarg :index-size-aligned)
   (vertex-memory
    :accessor draw-list-vertex-memory
    :initform nil
    :initarg :vertex-memory)
   (vertex-size-aligned
    :accessor draw-list-vertex-size-aligned
    :initform nil
    :initarg :vertex-size-aligned)
   (texture
    :accessor draw-list-texture
    :initform nil
    :initarg :texture)
   (font
    :accessor draw-list-font
    :initform nil
    :initarg :font)
   (line-thickness
    :accessor draw-list-line-thickness
    :initform nil
    :initarg :line-thickness)
   (point-size
    :accessor draw-list-point-size
    :initform nil
    :initarg :point-size)
   (group :accessor draw-list-group
	  :initform nil
	  :initarg :group))
  (:documentation "The base class for draw lists in krma."))

;; we are using textured vertices for standard (non-textured) primitives
;; for two reasons:
;; - cuts down on the number of shader variants we have to implement/track
;; - textured and non-textured vertices can be stored in the same draw list
;;   - less draw lists, less buffers, less cmds, etc.
;; we will not use "textured-" in the name for brevity, "textured-...-vertices" are implied.
;; we also don't associate the draw-list with a particular primitive type,
;; since this is a function of the pipeline, not the draw-list
;; and different pipelines using different primitives can use the same draw-lists
(defclass 2d-vertex-draw-list-mixin (draw-list-mixin)
  ((vertex-array :initform (make-textured-2d-vertex-array)))
  (:documentation "The base class for draw lists using textured-2d-vertex as the vertex structure."))

(defclass 2d-vertex-draw-list (2d-vertex-draw-list-mixin)
  ()
  (:documentation "A concrete class based on 2d-vertex-draw-list-mixin."))

(defclass 3d-vertex-draw-list-mixin (draw-list-mixin)
  ((vertex-array
    :initform (make-textured-3d-vertex-array)))
  (:documentation "The base class for draw lists using textured-3d-vertex as the vertex structure."))

(defclass 3d-vertex-draw-list (3d-vertex-draw-list-mixin)
  ()
  (:documentation "A concrete class based on 3d-vertex-draw-list-mixin."))

(defclass 3d-vertex-with-normal-draw-list-mixin (draw-list-mixin)
  ((vertex-array
    :initform (make-textured-3d-vertex-with-normal-array)))
  (:documentation "A base class for draw lists using 3d-vertex-with-normal as the vertex structure.  This is the vertex structure used in diffuse/specular lighting."))

(defclass 3d-vertex-with-normal-draw-list (3d-vertex-with-normal-draw-list-mixin)
  ()
  (:documentation "A concrete class based on 3d-vertex-with-normal-draw-list-mixin."))
