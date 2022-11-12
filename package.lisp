(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-concurrency)
  (ql:quickload :cffi))

(defpackage :krma
  (:use :cl :cffi :vk :%vk :$glfw :3d-vectors :3d-matrices)
  (:import-from :vk #:main)
  (:export #:mortho-vulkan
           #:mperspective-vulkan
           #:main

           #:im-draw-data
           #:rm-draw-data

           #:krma-application-mixin
           #:krma-essential-scene-mixin
           #:krma-test-application
           #:maybe-defer-debug
           #:update-2d-camera
           #:update-3d-camera
           #:render-scene
           #:pipeline-store-mixin
           #:application-scene
           #:application-pipeline-store
           #:scene-light-position
           #:main-window-width
           #:main-window-height
           #:immediate-mode-work-function-1
           #:application-exit?
           #:scene-class

           ;; easy add/draw functions
           #:add-2d-point-primitive
           #:add-2d-point
           #:draw-2d-point

           #:add-3d-point-primitive
           #:add-3d-point
           #:draw-3d-point

           #:add-2d-line-primitive
           #:add-2d-line
           #:draw-2d-line

           #:add-3d-line-primitive
           #:add-3d-line
           #:draw-3d-line

           #:add-multicolor-2d-polyline-primitive
           #:add-multicolor-2d-polyline
           #:draw-multicolor-2d-polyline

           #:add-2d-polyline-primitive
           #:add-2d-polyline
           #:draw-2d-polyline

           #:add-2d-triangle-primitive
           #:add-2d-triangle
           #:draw-2d-triangle

           #:add-2d-rectangle-primitive
           #:add-2d-rectangle
           #:draw-2d-rectangle

           #:add-2d-circular-arc-primitive
           #:add-2d-circular-arc
           #:draw-2d-circular-arc

           #:add-2d-circle-primitive
           #:group-add-2d-circle
           #:draw-2d-circle

           #:add-filled-2d-circle-primitive
           #:add-filled-2d-circle
           #:draw-filled-2d-circle

           #:add-multicolor-3d-polyline-primitive
           #:add-multicolor-3d-polyline
           #:draw-mutlicolor-3d-polyline

           #:add-3d-polyline-primitive
           #:add-3d-polyline
           #:draw-3d-polyline

           #:add-filled-2d-triangle-list-primitive
           #:add-filled-2d-triangle-list
           #:draw-filled-2d-triangle-list

           #:add-filled-2d-rectangle-list-primitive
           #:add-filled-3d-rectangle-list
           #:draw-filled-2d-rectangle-list

           #:add-textured-2d-triangle-list-primitive
           #:add-textured-2d-rectangle-list
           #:draw-textured-2d-rectangle-list

           #:add-filled-2d-convex-polygon-primitive
           #:add-filled-2d-convex-polygon
           #:draw-filled-2d-convex-polygon

           #:add-filled-3d-triangle-list-primitive
           #:add-filled-3d-triangle-list
           #:draw-filled-3d-triangle-list

           #:add-filled-3d-triangle-strip-primitive
           #:draw-filled-3d-triangle-strip

           #:add-textured-3d-triangle-list-primitive
           #:add-textured-3d-triangle-list
           #:draw-textured-3d-triangle-list

           #:add-textured-3d-triangle-strip-primitive
           #:draw-textured-3d-triangle-strip

           #:add-filled-sphere-primitive
           #:add-filled-sphere
           #:draw-filled-sphere

           #:add-text-primitive
           #:add-text
           #:draw-text

           #:*white-texture*
           #:*default-point-size*
           #:*default-line-thickness*
           #:*default-light-position*
           #:*multiply-matrix-function*

           ;; add/draw functions with positional arguments
           #:scene-add-2d-point
           #:scene-add-2d-point-primitive
           #:scene-draw-2d-point

           #:scene-add-3d-point
           #:scene-add-3d-point-primitive
           #:scene-draw-3d-point

           #:scene-add-2d-line
           #:scene-add-2d-line-primitive
           #:scene-draw-2d-line

           #:scene-add-3d-line
           #:scene-add-3d-line-primitive
           #:scene-draw-3d-line

           #:scene-add-2d-polyline
           #:scene-add-2d-polyline-primitive
           #:scene-draw-2d-polyline

           #:scene-add-2d-triangle
           #:scene-add-2d-triangle-primitive
           #:scene-draw-2d-triangle

           #:scene-add-2d-rectangle
           #:scene-add-2d-rectangle-primitive
           #:scene-draw-2d-rectangle

           #:scene-add-multicolor-2d-polyline
           #:scene-add-multicolor-2d-polyline-primitive
           #:scene-draw-multicolor-2d-polyline

           #:scene-add-2d-circular-arc
           #:scene-add-2d-circular-arc-primitive
           #:scene-draw-2d-circular-arc

           #:scene-add-2d-circle
           #:scene-add-2d-circle-primitive
           #:scene-draw-2d-circle

           #:scene-add-3d-polyline
           #:scene-add-3d-polyline-primitive
           #:scene-draw-3d-polyline

           #:scene-add-multicolor-3d-polyline
           #:scene-add-multicolor-3d-polyline-primitive
           #:scene-draw-multicolor-3d-polyline

           #:scene-add-filled-2d-triangle-list
           #:scene-add-filled-2d-triangle-list-primitive
           #:scene-draw-filled-2d-triangle-list

           #:scene-draw-filled-2d-triangle-strip
           #:scene-draw-filled-2d-triangle-strip

           #:scene-add-filled-2d-rectangle-list
           #:scene-add-filled-2d-rectangle-list-primitive
           #:scene-draw-filled-2d-rectangle-list

           #:scene-add-textured-2d-rectangle-list
           #:scene-add-textured-2d-rectangle-list-primitive
           #:scene-draw-textured-2d-rectangle-list

           #:scene-add-filled-2d-triangle-strip
           #:scene-add-textured-2d-triangle-list
           #:scene-add-textured-2d-triangle-list-primitive
           #:scene-draw-textured-2d-triangle-list
           #:scene-add-textured-2d-triangle-strip

           #:scene-add-filled-2d-convex-polygon
           #:scene-add-filled-2d-convex-polygon-primitive
           #:scene-draw-filled-2d-convex-polygon

           #:scene-add-filled-2d-circle
           #:scene-add-filled-2d-circle-primitive
           #:scene-draw-filled-2d-circle

           #:scene-add-filled-3d-triangle-list-flat
           #:scene-add-filled-3d-triangle-list-primitive-flat
           #:scene-draw-filled-3d-triangle-list-flat

           #:scene-add-filled-3d-triangle-list-diffuse
           #:scene-add-filled-3d-triangle-list-primitive-diffuse
           #:scene-draw-filled-3d-triangle-list-diffuse

           #:scene-add-filled-3d-triangle-strip-flat
           #:scene-draw-filled-3d-triangle-strip-flat

           #:scene-add-filled-3d-triangle-strip-diffuse
           #:scene-draw-filled-3d-triangle-strip-diffuse

           #:scene-add-filled-3d-convex-polygon-diffuse
           #:scene-add-filled-3d-convex-polygon-primitive-diffuse
           #:scene-draw-filled-3d-convex-polygon-diffuse

           #:scene-add-filled-3d-convex-polygon-flat
           #:scene-add-filled-3d-convex-polygon-primitive-flat
           #:scene-draw-filled-3d-convex-polygon-flat

           #:scene-add-multicolor-3d-convex-polygon-diffuse
           #:scene-add-multicolor-3d-convex-polygon-primitive-diffuse
           #:scene-draw-multicolor-3d-convex-polygon-diffuse

           #:scene-add-multicolor-3d-convex-polygon-flat
           #:scene-add-multicolor-3d-convex-polygon-primitive-flat
           #:scene-draw-multicolor-3d-convex-polygon-flat

           #:scene-add-textured-3d-triangle-list-flat
           #:scene-add-textured-3d-triangle-list-primitive-flat
           #:scene-draw-textured-3d-triangle-list-flat

           #:scene-add-textured-3d-triangle-strip-flat
           #:scene-draw-textured-3d-triangle-strip-flat

           #:scene-add-filled-sphere-diffuse
           #:scene-add-filled-sphere-primitive-diffuse
           #:scene-draw-filled-sphere-diffuse

           #:scene-add-text
           #:scene-add-text-primitive
           #:scene-draw-text

           #:reinstance-primitive
           #:reinstance-primitive-1
           #:primitive-set-color
           #:primitive-set-color-1
           #:primitive-set-transform
           #:primitive-set-transform-1
           #:primitive-apply-transform
           #:primitive-apply-transform-1
           #:primitive-set-line-thickness
           #:primitive-set-line-thickness-1
           #:delete-primitives
           #:delete-primitive
           #:delete-primitive-1

           #:delete-groups
           #:delete-groups-1
           #:delete-group

           #:group-set-color-override
           #:group-set-color-override-1

           #:group-set-model-matrix
           #:group-set-model-matrix-1

           #:group-apply-model-matrix
           #:group-apply-model-matrix-1

           #:group-set-light-position
           #:group-set-light-position-1
           ))
