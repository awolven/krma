(in-package :krma)

;; A [2d|3d]-line-strip-draw-list is navigated by essential-draw-indexed-cmds
;; the first-index of each cmd is where in the index-array the vertex-index of the
;; first-[vertex-]index is, where in this particular case, the vertex-index==zero,
;; so where (the index of) the correct zero is in the index-array corresponding to the
;; vertex offset for this cmd (whew.)
;; the elem-count is the number of vertexes to process
;; and the vtx-offset is where in the vertex-array the vertex with index zero (first index) is
;; this vtx-offset gets added to the index internally by the draw indexed call of the
;; particular graphics language to get the global array index (which could be bigger than a ushort)
;; this allows for index arrays to be longer that 65536 for unsigned-short arrays

(defstruct (essential-draw-indexed-cmd
            (:conc-name "CMD-")
            (:constructor make-essential-draw-indexed-cmd (draw-list first-idx elem-count vtx-offset)))
  (first-idx 0 :type (unsigned-byte 32))
  (elem-count 0 :type (unsigned-byte 32))
  (vtx-offset 0 :type (unsigned-byte 32))
  (draw-list nil))

(defmethod print-object ((object essential-draw-indexed-cmd) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ "first-idx: " stream)
    (princ (cmd-first-idx object) stream)
    (princ " elem-count: " stream)
    (princ (cmd-elem-count object) stream)
    (princ " vtx-offset: " stream)
    (princ (cmd-vtx-offset object) stream))
  object)

(defstruct (standard-draw-indexed-cmd
            (:include essential-draw-indexed-cmd)
            (:conc-name "CMD-")
            (:constructor make-standard-draw-indexed-cmd
                (draw-list first-idx elem-count vtx-offset
                 &optional (model-mtx nil) (color-override nil) (texture *white-texture*)
                   (line-thickness 1.0f0) (point-size 1.0f0) (light-position nil))))
  (model-mtx)
  (color-override)
  (texture)
  (line-thickness)
  (point-size)
  (light-position))

(defstruct (text-draw-indexed-cmd
            (:include standard-draw-indexed-cmd)
            (:conc-name "CMD-")
            (:constructor make-text-draw-indexed-cmd
                (font draw-list first-idx elem-count vtx-offset model-mtx color-override
                 texture line-thickness)))
  (font))

;; cmd-constructor is the "make" function of the particular cmd you want to instantiate
;; all cmds must :include essential-draw-indexed-cmd

;; this "-1" version of draw-2d-polyline-internal doesn't require the num-vertices as an arg
;; but must check for each vertex being added that the vertex and index arrays are big enough
;; todo: implement a "-2" version of draw-[2d|3d]-polyline that doesn't do extra checks
;; polylines don't need textured or with-normal versions, but polygons will
;; you can copy this cmd and issue it with different model-matrices if you wish
;; rather than baking a copy into the draw list
;; the argument `vertices' is a list of x y color ... repeating
