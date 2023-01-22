(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-concurrency)
  (ql:quickload :cffi))

(defpackage :krma
  (:use :cl :cffi :vk :%vk #-noglfw :$glfw :3d-vectors :3d-matrices)
  (:import-from :vk #:main
		#:*app*
		#:exit?
		#:application-exit?
		#:default-application-class-for-window
		#:default-window-class-for-application
		#:os-window-should-close?
		#:poll-application-events
		#:wait-application-events
		#:os-window-title
		#:get-os-window-pos
		#:set-os-window-pos
		#:get-os-window-cursor-pos
		#:get-os-window-size
		#:focus-os-window
		#:hide-os-window
		#:show-os-window
		#:maximize-os-window
		#:restore-os-window
		#:iconify-os-window
		#:get-os-window-frame-size
		#:get-os-window-framebuffer-size
		#:set-os-window-size
		#:set-os-window-aspect-ratio
		#:get-os-window-content-scale
		#:set-os-window-size-limits
		#:destroy-os-window
		#:shutdown-application)
  
  (:export #:*default-application-class*
	   #:default-application-class-for-window
	   #:default-window-class-for-application
	   #:2pi
	   #:*default-znear*
	   #:*default-zfar*
	   #:*white-texture*
	   #:*default-color*
	   #:*identity-matrix*
	   #:*default-point-size*
	   #:*default-line-thickness*
	   #:*default-number-of-segments*
	   #:*default-light-position*
	   #:*default-diffuse-color*
	   #:*default-specular-color*
	   #:*default-constant-attenuation*
	   #:*default-linear-attenuation*
	   #:*default-quadratic-attenuation*
	   #:*default-spot-cutoff*
	   #:*default-spot-exponent*
	   #:*default-spot-direction*
	   #:*default-scene-ambient*
	   #:*default-material*
	   #:*compact-trigger*

	   #:d2r
	   #:mperspective-vulkan	   
	   #:mortho-vulkan
	   #:canonicalize-color
	   #:color-p

	   #:maybe-defer-debug
	   #:gen-rm-handle
	   #:rm-dispatch-to-render-thread

	   #:light-mixin
	   #:directional-light
	   #:point-light
	   #:spot-light

	   #:material-mixin
	   #:standard-material
	   #:make-material

	   #:draw-list-mixin
	   #:2d-vertex-draw-list-mixin
	   #:2d-vertex-draw-list
	   #:3d-vertex-draw-list-mixin
	   #:3d-vertex-draw-list
	   #:3d-vertex-with-normal-draw-list-mixin
	   #:3d-vertex-with-normal-draw-list

	   #:textured-2d-vertex
	   #:textured-2d-vertex-array-push-extend
	   #:standard-2d-vertex-array-push-extend
	   #:textured-3d-vertex
	   #:textured-3d-vertex-array-push-extend
	   #:standard-3d-vertex-array-push-extend
	   #:textured-3d-vertex-with-normal
	   #:textured-3d-vertex-with-normal-array-push-extend
	   #:standard-3d-vertex-with-normal-array-push-extend

	   #:application-scene
	   #:pipeline-store-mixin
	   #:standard-pipeline-store
	   #:krma-application-mixin
	   #:krma-test-application
	   #:scene-class

	   #:main
	   #:run-1
	   #:run

	   #:im-draw-data
           #:rm-draw-data

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

	   #:add-filled-3d-convex-polygon-primitive
	   #:add-filled-3d-convex-polygon
	   #:draw-filled-3d-convex-polygon

           #:add-filled-sphere-primitive
           #:add-filled-sphere
           #:draw-filled-sphere

           #:add-text-primitive
           #:add-text
           #:draw-text

	   #:krma-essential-scene-mixin
	   #:update-2d-camera
	   #:update-3d-camera
	   #:render-scene

	   #:scene-add-2d-point-primitive
	   #:scene-add-2d-point
	   #:scene-draw-2d-point

	   #:scene-add-3d-point-primitive
           #:scene-add-3d-point           
           #:scene-draw-3d-point

	   #:scene-add-2d-line-primitive
           #:scene-add-2d-line         
           #:scene-draw-2d-line

	   #:scene-add-3d-line-primitive
           #:scene-add-3d-line
           #:scene-draw-3d-line

	   #:scene-add-2d-polyline-primitive
           #:scene-add-2d-polyline
           #:scene-draw-2d-polyline

	   #:scene-add-2d-triangle-primitive
	   #:scene-add-2d-triangle
           #:scene-draw-2d-triangle

	   #:scene-add-2d-rectangle-primitive
           #:scene-add-2d-rectangle           
           #:scene-draw-2d-rectangle

	   #:scene-add-multicolor-2d-polyline-primitive
           #:scene-add-multicolor-2d-polyline           
           #:scene-draw-multicolor-2d-polyline

	   #:scene-add-2d-circular-arc-primitive
           #:scene-add-2d-circular-arc           
           #:scene-draw-2d-circular-arc

	   #:scene-add-2d-circle-primitive
           #:scene-add-2d-circle           
           #:scene-draw-2d-circle

	   #:scene-add-3d-polyline-primitive
           #:scene-add-3d-polyline
           #:scene-draw-3d-polyline

	   #:scene-add-multicolor-3d-polyline-primitive
           #:scene-add-multicolor-3d-polyline
           #:scene-draw-multicolor-3d-polyline

	   #:scene-add-filled-2d-triangle-list-primitive
           #:scene-add-filled-2d-triangle-list           
           #:scene-draw-filled-2d-triangle-list

	   #:scene-add-filled-2d-triangle-strip-primitive
           #:scene-draw-filled-2d-triangle-strip

	   #:scene-add-filled-2d-rectangle-list-primitive
           #:scene-add-filled-2d-rectangle-list           
           #:scene-draw-filled-2d-rectangle-list

	   #:scene-add-textured-2d-rectangle-list-primitive
           #:scene-add-textured-2d-rectangle-list           
           #:scene-draw-textured-2d-rectangle-list

	   #:scene-add-textured-2d-triangle-list-primitive
	   #:scene-add-textured-2d-triangle-list
	   #:scene-draw-textured-2d-triangle-list

	   #:scene-add-filled-2d-convex-polygon-primitive
           #:scene-add-filled-2d-convex-polygon           
           #:scene-draw-filled-2d-convex-polygon

	   #:scene-add-filled-2d-circle-primitive
           #:scene-add-filled-2d-circle           
           #:scene-draw-filled-2d-circle

	   #:scene-add-filled-3d-triangle-list-primitive-flat
           #:scene-add-filled-3d-triangle-list-flat
           #:scene-draw-filled-3d-triangle-list-flat

	   #:scene-add-filled-3d-triangle-list-primitive-diffuse
           #:scene-add-filled-3d-triangle-list-diffuse           
           #:scene-draw-filled-3d-triangle-list-diffuse

           #:scene-add-filled-3d-triangle-strip-primitive-flat
           #:scene-draw-filled-3d-triangle-strip-flat

           #:scene-add-filled-3d-triangle-strip-primitive-diffuse
           #:scene-draw-filled-3d-triangle-strip-diffuse

	   #:scene-add-filled-3d-convex-polygon-primitive-diffuse
           #:scene-add-filled-3d-convex-polygon-diffuse
           #:scene-draw-filled-3d-convex-polygon-diffuse

	   #:scene-add-filled-3d-convex-polygon-primitive-flat
           #:scene-add-filled-3d-convex-polygon-flat           
           #:scene-draw-filled-3d-convex-polygon-flat

	   #:scene-add-multicolor-3d-convex-polygon-primitive-diffuse
           #:scene-add-multicolor-3d-convex-polygon-diffuse           
           #:scene-draw-multicolor-3d-convex-polygon-diffuse

	   #:scene-add-multicolor-3d-convex-polygon-primitive-flat
           #:scene-add-multicolor-3d-convex-polygon-flat           
           #:scene-draw-multicolor-3d-convex-polygon-flat

	   #:scene-add-textured-3d-triangle-list-primitive-flat
           #:scene-add-textured-3d-triangle-list-flat           
           #:scene-draw-textured-3d-triangle-list-flat

           #:scene-add-textured-3d-triangle-strip-primitve-flat
           #:scene-draw-textured-3d-triangle-strip-flat

	   #:scene-add-filled-sphere-primitive-diffuse
           #:scene-add-filled-sphere-diffuse           
           #:scene-draw-filled-sphere-diffuse

	   #:scene-add-text-primitive
           #:scene-add-text           
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
