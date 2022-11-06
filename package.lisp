(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-concurrency)
  (ql:quickload :cffi))

(defpackage :krma
  (:use :cl :cffi #-darwin :vk #-darwin :%vk :$glfw :3d-vectors :3d-matrices)
  (:import-from :vk #:main)
  (:export #:mortho-vulkan
           #:mperspective-vulkan

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

           ;; easy add/draw functions
           #:add-2d-point ;;
           #:group-add-2d-point
           #:draw-2d-point

           #:add-3d-point
           #:group-add-3d-point
           #:draw-3d-point

           #:add-2d-line ;;
           #:group-add-2d-line
           #:draw-2d-line

           #:add-3d-line ;;
           #:group-add-3d-line
           #:draw-3d-line

           #:add-multicolor-2d-polyline ;;
           #:group-add-multicolor-2d-polyline
           #:draw-multicolor-2d-polyline

           #:add-2d-polyline ;;
           #:group-add-2d-polyline
           #:draw-2d-polyline

           #:add-2d-triangle ;;
           #:group-add-2d-triangle
           #:draw-2d-triangle

           #:add-2d-rectangle ;;
           #:group-add-2d-rectangle
           #:draw-2d-rectangle

           #:add-2d-circular-arc ;;
           #:group-add-2d-circular-arc
           #:draw-2d-circular-arc

           #:add-2d-circle ;;
           #:group-add-2d-circle
           #:draw-2d-circle

           #:add-filled-2d-circle
           #:group-add-filled-2d-circle
           #:draw-filled-2d-circle

           #:add-multicolor-3d-polyline
           #:group-add-multicolor-3d-polyline
           #:draw-mutlicolor-3d-polyline

           #:add-3d-polyline
           #:group-add-3d-polyline
           #:draw-3d-polyline

           #:add-filled-2d-triangle-list ;;
           #:group-add-filled-2d-triangle-list
           #:draw-filled-2d-triangle-list

           #:add-filled-2d-rectangle-list
           #:group-add-filled-3d-rectangle-list
           #:draw-filled-2d-rectangle-list

           #:add-textured-2d-triangle-list
           #:group-add-textured-2d-rectangle-list
           #:draw-textured-2d-rectangle-list

           #:add-filled-2d-convex-polygon
           #:group-add-filled-2d-convex-polygon
           #:draw-filled-2d-convex-polygon

           #:add-filled-3d-triangle-list
           #:group-add-filled-3d-triangle-list
           #:draw-filled-3d-triangle-listo

           #:add-filled-3d-triangle-strip
           #:draw-filled-3d-triangle-strip

           #:add-textured-3d-triangle-list
           #:group-add-textured-3d-triangle-list
           #:draw-textured-3d-triangle-list

           #:add-textured-3d-triangle-strip
           #:draw-textured-3d-triangle-strip

           #:add-filled-sphere
           #:group-add-filled-sphere
           #:draw-filled-sphere

           #:add-text
           #:group-add-text
           #:scene-draw-text

           #:*white-texture*
           #:*default-point-size*
           #:*default-line-thickness*
           #:*default-light-position*
           #:*multiply-matrix-function*

           ;; add/draw functions with positional arguments
           #:scene-add-2d-point
           #:scene-add-2d-point-to-group
           #:scene-draw-2d-point

           #:scene-add-3d-point
           #:scene-draw-3d-point
           #:scene-draw-3d-point-to-group

           #:scene-add-2d-line
           #:scene-add-2d-line-to-group
           #:scene-draw-2d-line

           #:scene-add-3d-line
           #:scene-add-3d-line-to-group
           #:scene-draw-3d-line

           #:scene-add-2d-polyline
           #:scene-add-2d-polyline-to-group
           #:scene-draw-2d-polyline

           #:scene-add-2d-triangle
           #:scene-add-2d-triangle-to-group
           #:scene-draw-2d-triangle

           #:scene-add-2d-rectangle
           #:scene-add-2d-rectangle-to-group
           #:scene-draw-2d-rectangle

           #:scene-add-multicolor-2d-polyline
           #:scene-add-multicolor-2d-polyline-to-group
           #:scene-draw-multicolor-2d-polyline

           #:scene-add-2d-circular-arc
           #:scene-add-2d-circular-arc-to-group
           #:scene-draw-2d-circulat-arc

           #:scene-add-2d-circle
           #:scene-add-2d-circle-to-group
           #:scene-draw-2d-circle

           #:scene-add-3d-polyline
           #:scene-add-3d-polyline-to-group
           #:scene-draw-3d-polyline

           #:scene-add-multicolor-3d-polyline
           #:scene-add-multicolor-3d-polyline-to-group
           #:scene-draw-multicolor-3d-polyline

           #:scene-add-filled-2d-triangle-list
           #:scene-add-filled-2d-triangle-list-to-group
           #:scene-draw-filled-2d-triangle-list

           #:scene-draw-filled-2d-triangle-strip
           #:scene-draw-filled-2d-triangle-strip

           #:scene-add-filled-2d-rectangle-list
           #:scene-add-filled-2d-rectangle-list-to-group
           #:scene-draw-filled-2d-rectangle-list

           #:scene-add-textured-2d-rectangle-list
           #:scene-add-textured-2d-rectangle-list-to-group
           #:scene-draw-textured-2d-rectangle-list

           #:scene-add-filled-2d-triangle-strip
           #:scene-add-textured-2d-triangle-list
           #:scene-add-textured-2d-triangle-list-to-group
           #:scene-draw-textured-2d-triangle-list
           #:scene-add-textured-2d-triangle-strip

           #:scene-add-filled-2d-convex-polygon
           #:scene-add-filled-2d-convex-polygon-to-group
           #:scene-draw-filled-2d-convex-polygon

           #:scene-add-filled-2d-circle
           #:scene-add-filled-2d-circle-to-group
           #:scene-draw-filled-2d-circle

           #:scene-add-filled-3d-triangle-list-flat
           #:scene-add-filled-3d-triangle-list-flat-to-group
           #:scene-draw-filled-3d-triangle-list-flat

           #:scene-add-filled-3d-triangle-list-diffuse
           #:scene-add-filled-3d-triangle-list-diffuse-to-group
           #:scene-draw-filled-3d-triangle-list-diffuse

           #:scene-add-filled-3d-triangle-strip-flat
           #:scene-draw-filled-3d-triangle-strip-flat

           #:scene-add-filled-3d-triangle-strip-diffuse
           #:scene-draw-filled-3d-triangle-strip-diffuse

           #:scene-add-filled-3d-convex-polygon-diffuse
           #:scene-add-filled-3d-convex-polygon-diffuse-to-group
           #:scene-draw-filled-2d-convex-polygon-diffuse

           #:scene-add-filled-3d-convex-polygon-flat
           #:scene-add-filled-3d-convex-polygon-flat-to-group
           #:scene-draw-filled-2d-convex-polygon-flat

           #:scene-add-multicolor-3d-convex-polygon-diffuse
           #:scene-add-mutlicolor-3d-convex-polygon-diffuse-to-group
           #:scene-draw-multicolor-2d-convex-polygon-diffuse

           #:scene-add-multicolor-3d-convex-polygon-flat
           #:scene-add-mutlicolor-3d-convex-polygon-flat-to-group
           #:scene-draw-multicolor-2d-convex-polygon-flat

           #:scene-add-textured-3d-triangle-list-flat
           #:scene-add-textured-3d-triangle-list-flat-to-group
           #:scene-draw-textured-3d-triangle-list-flat

           #:scene-add-textured-3d-triangle-strip-flat
           #:scene-draw-textured-3d-triangle-strip-flat

           #:scene-add-filled-sphere-diffuse
           #:scene-add-filled-sphere-diffuse-to-group
           #:scene-draw-filled-sphere-diffuse

           #:scene-add-text
           #:scene-add-text-to-group
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
