(in-package :krma)

(defclass draw-list-mixin ()
  ((index-array
    :accessor draw-list-index-array
    :initform (make-index-array))
   (vertex-array
    :reader draw-list-vertex-array)
   (cmd-vector
    :initform (make-array +draw-list-alloc-size+ :adjustable t :fill-pointer 0)
    :reader draw-list-cmd-vector)
   (changed?
    :initform t
    :accessor draw-list-changed?)
   (needs-compaction?
    :initform nil
    :accessor draw-list-needs-compaction?)
   (index-memory
    :accessor draw-list-index-memory
    :initform nil)
   (index-size-aligned
    :accessor draw-list-index-size-aligned
    :initform 0)
   (vertex-memory
    :accessor draw-list-vertex-memory
    :initform nil)
   (vertex-size-aligned
    :accessor draw-list-vertex-size-aligned
    :initform 0)
   (texture
    :accessor draw-list-texture
    :initform nil :initarg :texture)
   (font
    :accessor draw-list-font
    :initform nil :initarg :font)
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
	  :initarg :group)))

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
  ((vertex-array
    :initform (make-textured-2d-vertex-array))))

(defclass 2d-vertex-draw-list (2d-vertex-draw-list-mixin)
  ())

(defclass 3d-vertex-draw-list-mixin (draw-list-mixin)
  ((vertex-array
    :initform (make-textured-3d-vertex-array))))

(defclass 3d-vertex-draw-list (3d-vertex-draw-list-mixin)
  ())

(defclass 3d-vertex-with-normal-draw-list-mixin (draw-list-mixin)
  ((vertex-array
    :initform (make-textured-3d-vertex-with-normal-array))))

(defclass 3d-vertex-with-normal-draw-list (3d-vertex-with-normal-draw-list-mixin)
  ())
