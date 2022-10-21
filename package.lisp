(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-concurrency)
  (ql:quickload :cffi))

(defpackage :krma
  (:use :cl :cffi #-darwin :vk #-darwin :%vk :%glfw :3d-vectors :3d-matrices)
  (:export #:unsigned-short-index-array
           #:make-unsigned-short-index-array
           #:unsigned-int-index-array
           #:make-unsigned-int-index-array
           #:foreign-array-ptr
           #:foreign-array-fill-pointer
           #:foreign-array-foreign-type
           #:foreign-array-foreign-type-size
           #:index-array-push-extend
           #:textured-2d-vertex
           #:textured-2d-vertex-array
           #:make-textured-2d-vertex-array
           #:textured-2d-vertex-array-push-extend
           #:standard-2d-vertex-array-push-extend
           #:textured-3d-vertex
           #:textured-3d-vertex-array
           #:make-textured-3d-vertex-array
           #:textured-3d-vertex-array-push-extend
           #:standard-3d-vertex-array-push-extend
           #:textured-3d-vertex-with-normal
           #:textured-3d-vertex-with-normal-array
           #:standard-3d-vertex-with-normal-array-push-extend
           #:textured-3d-vertex-with-normal-array-push-extend

           #:standard-draw-data
           #:3d-point-list-draw-list
           #:3d-line-list-draw-list
           #:3d-line-strip-draw-list
           #:3d-triangle-list-draw-list
           #:3d-triangle-strip-draw-list
           #:2d-point-list-draw-list
           #:2d-line-list-draw-list
           #:2d-line-strip-draw-list
           #:2d-triangle-list-draw-list
           #:2d-triangle-strip-draw-list

           #:draw-data-3d-point-list-draw-list
           #:draw-data-3d-line-list-draw-list
           #:draw-data-3d-line-strip-draw-list
           #:draw-data-3d-triangle-list-draw-list
           #:draw-data-3d-triangle-strip-draw-list
           #:draw-data-2d-point-list-draw-list
           #:draw-data-2d-line-list-draw-list
           #:draw-data-2d-line-strip-draw-list
           #:draw-data-2d-triangle-list-draw-list
           #:draw-data-2d-triangle-strip-draw-list
           #:draw-data-work-queue

           #:im-draw-data
           #:rm-draw-data

           #:draw-list-index-array
           #:draw-list-vertex-array
           #:draw-list-index-buffer
           #:draw-list-vertex-buffer
           #:draw-list-cmd-vector

           #:2d-vertex-small-draw-list
           #:2d-vertex-large-draw-list
           #:3d-vertex-small-draw-list
           #:3d-vertex-large-draw-list

           #:essential-draw-indexed-cmd
           #:standard-draw-indexed-cmd

           #:cmd-first-idx
           #:cmd-elem-count
           #:cmd-vtx-offset
           #:cmd-draw-list

           #:cmd-model-mtx
           #:cmd-color-override
           #:cmd-texture
           #:cmd-line-thickness
           #:cmd-point-size

           #:prim-reserve-standard-2d-small
           #:prim-reserve-standard-2d-large
           #:prim-reserve-textured-2d-small
           #:prim-reserve-textured-2d-large
           #:%prim-reserve

           #:make-textured-2d-draw-indexed-cmd

           #:essential-scene-mixin
           #:application-with-essential-scene-mixin
           #:scene
           #:application-scene

           #:add-2d-point ;; working
           #:draw-2d-point
           #:add-3d-point
           #:draw-3d-point
           #:add-2d-line ;; working
           #:add-3d-line ;; working
           #:add-multicolor-2d-polyline ;; working
           #:draw-multicolor-2d-polyline
           #:add-2d-polyline ;; working
           #:add-2d-triangle ;; working
           #:draw-2d-triangle
           #:add-2d-rectangle ;; working
           #:draw-2d-rectangle
           #:add-2d-circular-arc ;; working
           #:add-2d-circle ;; working
           #:add-multicolor-3d-polyline
           #:add-3d-polyline
           #:add-filled-2d-triangle-list ;; working
           #:add-filled-2d-triangle-strip
           #:add-textured-2d-triangle-list
           #:add-textured-2d-triangle-strip
           #:add-filled-2d-rectangle-list ;; working
           #:add-textured-2d-rectangle-list ;; so far so good.
           #:add-filled-2d-polygon ;; working
           #:add-filled-3d-triangle-list
           #:add-filled-3d-triangle-strip
           #:add-filled-3d-triangle-list-with-normals
           #:add-filled-3d-triangle-strip-with-normals
           #:add-textured-3d-triangle-list
           #:add-textured-3d-triangle-strip

           #:*white-texture*
           #:*default-point-size*
           #:*default-line-thickness*
           #:*multiply-matrix-function*

           #:scene-add-2d-point
           #:scene-draw-2d-point
           #:scene-add-3d-point
           #:scene-draw-3d-point
           #:scene-add-2d-line
           #:scene-add-3d-line
           #:scene-add-multicolor-2d-polyline-1
           #:scene-draw-multicolor-2d-polyline-1
           #:scene-add-2d-polyline-1
           #:scene-add-2d-triangle
           #:scene-draw-2d-triangle
           #:scene-add-2d-rectangle
           #:scene-draw-2d-rectangle
           #:scene-add-2d-circular-arc
           #:scene-add-2d-circle
           #:scene-add-multicolor-3d-polyline-1
           #:scene-add-3d-polyline-1
           #:scene-add-filled-2d-triangle-list
           #:scene-add-filled-2d-triangle-strip
           #:scene-add-textured-2d-triangle-list
           #:scene-add-textured-2d-triangle-strip
           #:scene-add-filled-2d-rectangle-list
           #:scene-add-textured-2d-rectangle-list
           #:scene-add-filled-2d-polygon
           #:scene-add-filled-3d-triangle-list
           #:scene-add-filled-3d-triangle-strip
           #:scene-add-filled-3d-triangle-list-with-normals
           #:scene-add-filled-3d-triangle-strip-with-normals
           #:scene-add-textured-3d-triangle-list
           #:scene-add-textured-3d-triangle-strip

           #:reinstance-primitive
           #:primitive-set-color
           #:primitive-set-transform
           #:primitive-apply-transform
           #:primitive-set-line-thickness
           #:delete-primitive

           #:compact-draw-list
           ))
