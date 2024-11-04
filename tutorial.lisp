(in-package :krma)

;;scene-add-2d-point-primitive

;; these point functions are not working correctly with AMD driver

(scene-add-2d-point-primitive (default-scene) :default nil 10 #xffff 100 100)

;;scene-add-2d-point

(scene-add-2d-point (default-scene) :default 20 #xffffff 200 100)

;;scene-draw-2d-point

(defun scene-draw-2d-point-test ()
  (scene-draw-2d-point (default-scene) :default 30 #xff0000ff 300 100))

(setf (immediate-mode-work-function-6 (default-display))
      #'scene-draw-2d-point-test)

;;scene-add-3d-point-primitive

(scene-add-3d-point-primitive (default-scene) :default nil 40 #xfffff 0 0 0)

;;scene-add-3d-point

(scene-add-3d-point (default-scene) :default 15 #xffffff 50 50 50)

;;scene-draw-3d-point

(defun scene-draw-3d-point-test ()
  (scene-draw-3d-point (default-scene) :default 25 #xf00000ff -50 50 50))

(setf (immediate-mode-work-function-6 (default-display))
      #'scene-draw-3d-point-test)

;;scene-add-2d-line-primitive

(scene-add-2d-line-primitive (default-scene) :default nil 5 #xf0ffff 300 300 400 400)

;;scene-add-2d-line

(scene-add-2d-line (default-scene) :default 5 #x88ffff 400 400 500 400)

;;scene-draw-2d-line

(defun scene-draw-2d-line-test ()
  (scene-draw-2d-line (default-scene) :default 15 #xffff 400 500 500 500))

(setf (immediate-mode-work-function-6 (default-display))
      #'scene-draw-2d-line-test)

;;scene-add-3d-line-primitive

(scene-add-3d-line-primitive (default-scene) :default nil 25 #x88ffff 0 0 0 100 100 100)

;;scene-add-3d-line

(scene-add-3d-line (default-scene) :default 30 #xff0000ff 0 0 0 -100 100 -100)

;;scene-draw-3d-line

(let ((scene (default-scene)))
  (defun scene-draw-3d-line-test ()
    (scene-draw-3d-line scene :default 10 #xddf000ff 0 0 0 -100 -100 100)))

(setf (immediate-mode-work-function-6 (default-display))
      #'scene-draw-3d-line-test)

;;scene-add-2d-polyline-primitive

(scene-add-2d-polyline-primitive (default-scene) :default nil t 8 #xffff (list 100 100 200 100 200 200 100 200) 0 10)
;; this time we added an elevation of 10
;; lets add a green polyline behind it:
(scene-add-2d-polyline-primitive (default-scene) :default nil t 16 #xfff00ff (list 100 100 200 100 200 200 100 200) 0 5)

;;scene-add-2d-polyline

(scene-add-2d-polyline (default-scene) :default t 24 #xff0000ff (list 100 100 200 100 200 200 100 200))

;;scene-draw-2d-polyline

(defun scene-draw-2d-polyline-test ()
  (scene-draw-2d-polyline (default-scene) :default t 2 #xffffff (list 100 100 200 100 200 200 100 200) 0 15))

(setf (immediate-mode-work-function-6 (default-display))
      #'scene-draw-2d-polyline-test)

;;scene-add-2d-triangle-primitive

(scene-add-2d-triangle-primitive (default-scene) :default nil 3 #xfff00ff 300 100 400 300 200 300)

;;scene-add-2d-triangle

(scene-add-2d-triangle (default-scene) :default 5 #xf0ff00ff 500 100 600 500 400 500)

;;scene-draw-2d-triangle

(defun scene-draw-2d-triangle-test ()
  (scene-draw-2d-triangle (default-scene) :default 1.5 #xffff 500 100 600 500 400 500 0 20))

(setf (immediate-mode-work-function-6 (default-display))
      #'scene-draw-2d-triangle-test)

;;scene-add-2d-rectangle-primitive

(scene-add-2d-rectangle-primitive (default-scene) :default nil 5 #xff00ff 700 700 800 800)

;;scene-add-2d-rectangle

(scene-add-2d-rectangle (default-scene) :default 10 #xfff00ff 900 700 1000 800)

;;scene-draw-2d-rectangle

(defun scene-draw-2d-rectangle-test ()
  (scene-draw-2d-rectangle (default-scene) :default 20 #xffff 1010 400 1110 500))

(setf (immediate-mode-work-function-6 (default-display))
      #'scene-draw-2d-rectangle-test)

;;scene-add-multicolor-2d-polyline-primitive

(scene-add-multicolor-2d-polyline-primitive (default-scene) :default nil t 7 (list 600 100 #xff0000ff 700 200 #xfff000ff 500 200 #xffff00ff))

;;scene-add-multicolor-2d-polyline

(scene-add-multicolor-2d-polyline (default-scene) :default t 8 (list 800 100 #xffff 900 200 #xfffff 700 200 #xffffff))

;;scene-draw-multicolor-2d-polyline

(defun scene-draw-multicolor-2d-polyline-test ()
  (scene-draw-multicolor-2d-polyline (default-scene) :default t 10 (list 1000 100 #xff00ff 1100 200 #xf000ff 900 200 #xfffff)))

(setf (immediate-mode-work-function-6 (default-display))
      #'scene-draw-multicolor-2d-polyline-test)

;;scene-add-multicolor-2d-instanced-line-primitive

(scene-add-multicolor-2d-instanced-line-primitive (default-scene) :default nil t 20 (list 1200 100 #xff0000ff 1300 300 #xfff000ff 1100 300 #xffff00ff))

;;scene-add-filled-3d-instanced-tube-primitive

(scene-add-filled-3d-instanced-tube-primitive (default-scene) :default nil t 40 #x8fffff
					       (list 100 100 100 300 100 -100 200 0 -50))

;;scene-add-2d-circular-arc-primitive

(scene-add-2d-circular-arc-primitive (default-scene) :default nil nil 7 #xffff 100 200 50 0 (* 3 (/ pi 2)) 90)
;; remember, the y coordinate is increasing in the downward direction in krma
;; and the delta theta is a counter clockwise rotation, but since the y coordinate is positive
;; in the downward direction, 2d circular arcs appear clockwise

(scene-add-2d-circular-arc-primitive (default-scene) :default nil nil 7 #xffff 100 400 50 (/ pi 2) (* 3 (/ pi 2)) 90)

(scene-add-2d-circular-arc-primitive (default-scene) :default nil nil 7 #xffff 100 600 50 (- (/ pi 2)) (/ pi 2) 90)

;; a closed arc:
(scene-add-2d-circular-arc-primitive (default-scene) :default nil t 7 #xffff 100 800 50 (- (/ pi 2)) (/ pi 2) 90)

;;scene-add-2d-circular-arc

(scene-add-2d-circular-arc (default-scene) :default t 7 #xffff 500 500 50 (- (/ pi 3)) (/ pi 2) 90)

;;scene-draw-2d-circular-arc

(defun scene-draw-2d-circular-arc-test ()
  (scene-draw-2d-circular-arc (default-scene) :default t 7 #xffff 500 700 50 (- (/ pi 3)) (/ pi 2) 90))

(setf (immediate-mode-work-function-6 (default-display))
      #'scene-draw-2d-circular-arc-test)

;;scene-add-2d-circle-primitive

(scene-add-2d-circle-primitive (default-scene) :default nil 7 #xffff 100 300 50 180)

;;scene-add-2d-circle

(scene-add-2d-circle (default-scene) :default 7 #xffff 100 100 50 180)

;;scene-draw-2d-circle

(defun scene-draw-2d-circle-test ()
  (scene-draw-2d-circle (default-scene) :default 8 #xfffff 100 500 50 180))

(setf (immediate-mode-work-function-6 (default-display))
      #'scene-draw-2d-circle-test)

;;scene-add-3d-polyline-primitive

(scene-add-3d-polyline-primitive (default-scene) :default nil t 8 #xffffff
				  (list 100 100 100 100 200 100 200 200 100 200 100 -100))

;;scene-add-3d-polyline

(scene-add-3d-polyline (default-scene) :default t 10 #xff (list 0 0 0 50 50 50 50 50 -50 50 -50 -50))

;;scene-draw-3d-polyline

(defun scene-draw-3d-polyline-test ()
  (scene-draw-3d-polyline (default-scene) :default t 10 #xfff (list 0 0 0 -50 -50 -50 -50 50 -50 50 50)))

(setf (immediate-mode-work-function-6 (default-display))
      #'scene-draw-3d-polyline-test)

;;scene-add-multicolor-3d-polyline-primitive

(scene-add-multicolor-3d-polyline-primitive (default-scene) :default nil t 10 (list -100 -100 -100 #xffff 75 75 -75 #xffffff 0 50 0 #xff00ff))

;;scene-add-multicolor-3d-polyline

(scene-add-multicolor-3d-polyline (default-scene) :default t 10 (list 100 100 100 #xffff -75 -75 75 #xffffff 0 -50 0 #xff00ff))

;;scene-draw-multicolor-3d-polyline

(defun scene-draw-multicolor-3d-polyline-test () (scene-draw-multicolor-3d-polyline (default-scene) :default t 10 (list 200 100 150 #xff00ffff 75 -75 75 #xddffffff 0 50 100 #xff0000ff)))

(setf (immediate-mode-work-function-6 (default-display))
      #'scene-draw-multicolor-3d-polyline-test)

;;scene-add-filled-2d-triangle-list-primitive

(scene-add-filled-2d-triangle-list-primitive (default-scene)
					      :default nil #xff8800ff
					      (list 500 100 600 500 400 500))

;;scene-add-filled-2d-triangle-list

(scene-add-filled-2d-triangle-list (default-scene) :default #xff0044ff
				    (list 300 100 400 300 200 300))

;;scene-draw-filled-2d-triangle-list

(defun scene-draw-filled-2d-triangle-list-test ()
  (scene-draw-filled-2d-triangle-list (default-scene) :default #xff4400ff
				      (list 300 500 400 700 200 700 500 500 600 700 300 700)))

(setf (immediate-mode-work-function-6 (default-display))
      #'scene-draw-filled-2d-triangle-list-test)

;;scene-add-filled-2d-triangle-strip-primitive

(scene-add-filled-2d-triangle-strip-primitive
 (default-scene) :default nil #x888800ff
 (list 1200 1300 1215 1320 1195 1320 1180 1300 1170 1325 1195 1320))

;;scene-add-filled-2d-rectangle-list-primitive

(scene-add-filled-2d-rectangle-list-primitive
 (default-scene) :default nil #xff7777ff
 (list 1200 1400 1215 1420 1300 1400 1350 1423 1400 1400 1470 1430))

;;scene-add-filled-2d-rectangle-list

(scene-add-filled-2d-rectangle-list
 (default-scene) :default #x777722ff
 (list 1200 1500 1215 1520 1300 1500 1350 1523 1400 1500 1470 1530))

;;scene-draw-filled-2d-rectangle-list

(defun scene-draw-filled-2d-rectangle-list-test ()
  (scene-draw-filled-2d-rectangle-list
   (default-scene) :default #x7777ddff
   (list 1200 1600 1215 1620 1300 1600 1350 1623 1400 1600 1470 1630)))

(setf (immediate-mode-work-function-6 (default-display))
      #'scene-draw-filled-2d-rectangle-list-test)  

;;scene-add-textured-2d-rectangle-list-primitive

(defvar *earth-texture*
  (make-vulkan-texture-from-image-file
   (default-display)
   (asdf/system:system-relative-pathname :krma "land_ocean_ice_2048.png")))

(scene-add-textured-2d-rectangle-list-primitive
 (default-scene) :default nil *earth-texture* #xffffffff
 (list 100 100 0.0 0.0 1100 600 1.0 1.0))

;;scene-add-textured-2d-rectangle-list

(scene-add-textured-2d-rectangle-list
 (default-scene) :default *earth-texture* #xffffffff
 (list 100 100 0.0 0.0 1100 600 1.0 1.0))

;;scene-draw-textured-2d-rectangle-list

(defun scene-draw-textured-2d-rectangle-list-test ()
  (scene-draw-textured-2d-rectangle-list
   (default-scene) :default *earth-texture* #xffffffff
   (list 100 100 0.0 0.0 1100 600 1.0 1.0)))

(setf (immediate-mode-work-function-6 (default-display))
      #'scene-draw-textured-2d-rectangle-list-test)

;;scene-add-textured-sphere-primitive-diffuse

(scene-add-textured-sphere-primitive-diffuse
 (default-scene) :default nil *earth-texture* #xffffffff 0 0 0 100 nil 200)

;; todo: add these next 3 to krma:
;;scene-add-textured-2d-triangle-list-primitive
;;scene-add-textured-2d-triangle-list
;;scene-draw-textured-2d-triangle-list

;;scene-add-filled-2d-convex-polygon-primitive

(scene-add-filled-2d-convex-polygon-primitive
 (default-scene) :default nil #xffff00ff
 (list 1200 1200 1220 1220 1230 1250 1215 1270 1195 1260 1180 1225))

;;scene-add-filled-2d-convex-polygon

(scene-add-filled-2d-convex-polygon
 (default-scene) :default #xffdd00ff
 (list 1300 1200 1320 1220 1330 1250 1315 1270 1295 1260 1280 1225))

;;scene-draw-filled-2d-convex-polygon

(defun scene-draw-filled-2d-convex-polygon-test ()
  (scene-draw-filled-2d-convex-polygon
   (default-scene) :default #x88dd00ff
   (list 1400 1200 1420 1220 1430 1250 1415 1270 1395 1260 1380 1225)))
  
(setf (immediate-mode-work-function-6 (default-display))
      #'scene-draw-filled-2d-convex-polygon-test)

;;scene-add-filled-2d-circle-primitive

(scene-add-filled-2d-circle-primitive
 (default-scene) :default nil #xff00ffff 900 900 50 60)

;;scene-add-filled-2d-circle

(scene-add-filled-2d-circle
 (default-scene) :default #xfff0ffff 1000 900 50 60)

;;scene-draw-filled-2d-circle

(defun scene-draw-filled-2d-circle-test ()
  (scene-draw-filled-2d-circle
   (default-scene) :default #x888888ff 1200 900 50 60))

(setf (immediate-mode-work-function-6 (default-display))
      #'scene-draw-filled-2d-circle-test)

;;scene-add-filled-3d-triangle-list-primitive-flat

(defun cube4 ()
  (let ((right (scene-add-filled-3d-triangle-list-primitive-flat
		(default-scene) :default nil #xff0000ff
		(list 100 100 100
		      100 -100 100
		      100 -100 -100
		      100 100 100
		      100 100 -100
		      100 -100 -100)))
	(left (scene-add-filled-3d-triangle-list-primitive-flat
		(default-scene) :default nil #xff00ffff
		(list -100 100 100
		      -100 -100 100
		      -100 -100 -100
		      -100 100 100
		      -100 100 -100
		      -100 -100 -100)))
	(front (scene-add-filled-3d-triangle-list-primitive-flat
		(default-scene) :default nil #xff00ff
		(list -100 -100 100
		      100 -100 100
		      100 -100 -100
		      -100 -100 100
		      -100 -100 -100
		      100 -100 -100)))
	(rear (scene-add-filled-3d-triangle-list-primitive-flat
	       (default-scene) :default nil #xffff00ff
	       (list -100 100 100
		     100 100 100
		     100 100 -100
		     -100 100 100
		     -100 100 -100
		     100 100 -100)))
	(top (scene-add-filled-3d-triangle-list-primitive-flat
	      (default-scene) :default nil #xffff
	      (list -100 100 100
		    100 100 100
		    100 -100 100
		    -100 100 100
		    -100 -100 100
		    100 -100 100)))
	(bottom (scene-add-filled-3d-triangle-list-primitive-flat
		 (default-scene) :default nil #xffffff
		 (list -100 100 -100
		       100 100 -100
		       100 -100 -100
		       -100 100 -100
		       -100 -100 -100
		       100 -100 -100))))
    (list right left front rear top bottom)))
	       
(cube4)

;;scene-add-filled-3d-triangle-list-flat

(defun cube5 ()
  (let ((group :cube))
    (scene-add-filled-3d-triangle-list-flat
     (default-scene) group #xff0000ff
     (list 100 100 100
	   100 -100 100
	   100 -100 -100
	   100 100 100
	   100 100 -100
	   100 -100 -100))
    (scene-add-filled-3d-triangle-list-flat
     (default-scene) group #xff00ffff
     (list -100 100 100
	   -100 -100 100
	   -100 -100 -100
	   -100 100 100
	   -100 100 -100
	   -100 -100 -100))
    (scene-add-filled-3d-triangle-list-flat
     (default-scene) group #xff00ff
     (list -100 -100 100
	   100 -100 100
	   100 -100 -100
	   -100 -100 100
	   -100 -100 -100
	   100 -100 -100))
    (scene-add-filled-3d-triangle-list-flat
     (default-scene) group #xffff00ff
     (list -100 100 100
	   100 100 100
	   100 100 -100
	   -100 100 100
	   -100 100 -100
	   100 100 -100))
    (scene-add-filled-3d-triangle-list-flat
     (default-scene) group #xffff
     (list -100 100 100
	   100 100 100
	   100 -100 100
	   -100 100 100
	   -100 -100 100
	   100 -100 100))
    (scene-add-filled-3d-triangle-list-flat
     (default-scene) group #xffffff
     (list -100 100 -100
	   100 100 -100
	   100 -100 -100
	   -100 100 -100
	   -100 -100 -100
	   100 -100 -100))
    group))

(cube5)

;;scene-draw-filled-3d-triangle-list-flat

(defun cube6 ()
  (let ((group :cube))
    (scene-draw-filled-3d-triangle-list-flat
     (default-scene) group #xff0000ff
     (list 100 100 100
	   100 -100 100
	   100 -100 -100
	   100 100 100
	   100 100 -100
	   100 -100 -100))
    (scene-draw-filled-3d-triangle-list-flat
     (default-scene) group #xff00ffff
     (list -100 100 100
	   -100 -100 100
	   -100 -100 -100
	   -100 100 100
	   -100 100 -100
	   -100 -100 -100))
    (scene-draw-filled-3d-triangle-list-flat
     (default-scene) group #xff00ff
     (list -100 -100 100
	   100 -100 100
	   100 -100 -100
	   -100 -100 100
	   -100 -100 -100
	   100 -100 -100))
    (scene-draw-filled-3d-triangle-list-flat
     (default-scene) group #xffff00ff
     (list -100 100 100
	   100 100 100
	   100 100 -100
	   -100 100 100
	   -100 100 -100
	   100 100 -100))
    (scene-draw-filled-3d-triangle-list-flat
     (default-scene) group #xffff
     (list -100 100 100
	   100 100 100
	   100 -100 100
	   -100 100 100
	   -100 -100 100
	   100 -100 100))
    (scene-draw-filled-3d-triangle-list-flat
     (default-scene) group #xffffff
     (list -100 100 -100
	   100 100 -100
	   100 -100 -100
	   -100 100 -100
	   -100 -100 -100
	   100 -100 -100))
    group))

(setf (immediate-mode-work-function-6 (default-display))
      #'cube6)
      
;;scene-add-filled-3d-triangle-list-primitive-diffuse

(let ((m (meye 4)))
  ;; z is up in the default camera of krma
  (scene-add-filled-3d-triangle-list-primitive-diffuse
   (default-scene) :default (mrotate m (vec3 1 0 0) (/ pi 2)) #xffffff *teapot-vertices* nil))

;;scene-add-filled-3d-triangle-list-diffuse

(scene-add-filled-3d-triangle-list-diffuse
 (default-scene) :default #xff0000ff *teapot-vertices*)

;;scene-draw-filled-3d-triangle-list-diffuse

(defun scene-draw-filled-3d-triangle-list-diffuse-test ()
  (scene-draw-filled-3d-triangle-list-diffuse
   (default-scene) :default #xff0000ff *teapot-vertices*))

(setf (immediate-mode-work-function-6 (default-display))
      #'scene-draw-filled-3d-triangle-list-diffuse-test)

;;scene-add-filled-3d-triangle-strip-primitive-flat

(defun cube7 ()
  (let ((right (scene-add-filled-3d-triangle-strip-primitive-flat
		(default-scene) :default nil #xff0000ff
		(list 100 100 100
		      100 100 -100
		      100 -100 100
		      100 -100 -100
		      100 100 -100)))
	(left (scene-add-filled-3d-triangle-strip-primitive-flat
		(default-scene) :default nil #xff00ffff
		(list -100 100 100
		      -100 100 -100
		      -100 -100 100
		      -100 -100 -100
		      -100 100 -100)))
	(front (scene-add-filled-3d-triangle-strip-primitive-flat
		(default-scene) :default nil #xff00ff
		(list 100 -100 100
		      100 -100 -100
		      -100 -100 100
		      -100 -100 -100
		      100 -100 -100)))
	(rear (scene-add-filled-3d-triangle-strip-primitive-flat
	       (default-scene) :default nil #xffff00ff
	       (list 100 100 100
		     -100 100 100
		     100 100 -100
		     -100 100 -100
		     -100 100 100)))
	(top (scene-add-filled-3d-triangle-strip-primitive-flat
	      (default-scene) :default nil #xffff
	      (list 100 100 100
		    100 -100 100
		    -100 100 100
		    -100 -100 100
		    100 -100 100)))
	(bottom (scene-add-filled-3d-triangle-strip-primitive-flat
		 (default-scene) :default nil #xffffff
		 (list 100 100 -100
		       100 -100 -100
		       -100 100 -100
		       -100 -100 -100
		       100 -100 -100))))
    (list right left front rear top bottom)))

(cube7)

;;scene-add-filled-3d-triangle-strip-primitive-diffuse ;; used in parakeet

(defun cube8 ()
  (let ((right (scene-add-filled-3d-triangle-strip-primitive-diffuse
		(default-scene) :default nil #xff0000ff
		(list 100 100 100 1 0 0
		      100 100 -100 1 0 0
		      100 -100 100 1 0 0
		      100 -100 -100 1 0 0
		      100 100 -100 1 0 0)
		nil))
	(left (scene-add-filled-3d-triangle-strip-primitive-diffuse
		(default-scene) :default nil #xff00ffff
		(list -100 100 100 -1 0 0
		      -100 100 -100 -1 0 0
		      -100 -100 100 -1 0 0
		      -100 -100 -100 -1 0 0
		      -100 100 -100 -1 0 0)
		nil))
	(front (scene-add-filled-3d-triangle-strip-primitive-diffuse
		(default-scene) :default nil #xff00ff
		(list 100 -100 100 0 1 0
		      100 -100 -100 0 1 0
		      -100 -100 100 0 1 0
		      -100 -100 -100 0 1 0
		      100 -100 -100 0 1 0)
		nil))
	(rear (scene-add-filled-3d-triangle-strip-primitive-diffuse
	       (default-scene) :default nil #xffff00ff
	       (list 100 100 100 0 -1 0
		     -100 100 100 0 -1 0
		     100 100 -100 0 -1 0
		     -100 100 -100 0 -1 0
		     -100 100 100 0 -1 0)
	       nil))
	(top (scene-add-filled-3d-triangle-strip-primitive-diffuse
	      (default-scene) :default nil #xffff
	      (list 100 100 100 0 0 1
		    100 -100 100 0 0 1
		    -100 100 100 0 0 1
		    -100 -100 100 0 0 1
		    100 -100 100 0 0 1)
	      nil))
	(bottom (scene-add-filled-3d-triangle-strip-primitive-diffuse
		 (default-scene) :default nil #xffffff
		 (list 100 100 -100 0 0 -1
		       100 -100 -100 0 0 -1
		       -100 100 -100 0 0 -1
		       -100 -100 -100 0 0 -1
		       100 -100 -100 0 0 -1)
		 nil)))
    (list right left front rear top bottom)))

(cube8)

;;Scene-add-filled-3d-convex-polygon-primitive-diffuse

(defun cube-diffuse1 ()
  (let ((right (scene-add-filled-3d-convex-polygon-primitive-diffuse
		(default-scene) :default nil #xff0000ff
		(list 100 100 100 1 0 0 100 -100 100 1 0 0 100 -100 -100 1 0 0 100 100 -100 1 0 0)
		nil))
	(left (scene-add-filled-3d-convex-polygon-primitive-diffuse
	       (default-scene) :default nil #xff00ffff
	       (list -100 100 100 -1 0 0 -100 -100 100 -1 0 0 -100 -100 -100 -1 0 0 -100 100 -100 -1 0 0)
	       nil))
	(front (scene-add-filled-3d-convex-polygon-primitive-diffuse
		(default-scene) :default nil #xff00ff
		(list -100 -100 100 0 1 0 100 -100 100 0 1 0 100 -100 -100 0 1 0 -100 -100 -100 0 1 0)
		nil))
	(rear (scene-add-filled-3d-convex-polygon-primitive-diffuse
	       (default-scene) :default nil #xffff00ff
	       (list -100 100 100 0 -1 0 100 100 100 0 -1 0 100 100 -100 0 -1 0 -100 100 -100 0 -1 0)
	       nil))
	(top (scene-add-filled-3d-convex-polygon-primitive-diffuse
	      (default-scene) :default nil #xffff
	      (list -100 100 100 0 0 1 100 100 100 0 0 1 100 -100 100 0 0 1 -100 -100 100 0 0 1)
	      nil))
	(bottom (scene-add-filled-3d-convex-polygon-primitive-diffuse
		 (default-scene) :default nil #xffffff
		 (list -100 100 -100 0 0 -1 100 100 -100 0 0 -1 100 -100 -100 0 0 -1 -100 -100 -100 0 0 -1)
		 nil)))

    (list right left front rear top bottom)))

(cube-diffuse1)

;;scene-add-filled-3d-convex-polygon-diffuse

(defun cube-diffuse2 ()
  (scene-add-filled-3d-convex-polygon-diffuse
   (default-scene) :default #xff0000ff
   (list 100 100 100 1 0 0 100 -100 100 1 0 0 100 -100 -100 1 0 0 100 100 -100 1 0 0))
  (scene-add-filled-3d-convex-polygon-diffuse
   (default-scene) :default #xff00ffff
   (list -100 100 100 -1 0 0 -100 -100 100 -1 0 0 -100 -100 -100 -1 0 0 -100 100 -100 -1 0 0))
  (scene-add-filled-3d-convex-polygon-diffuse
   (default-scene) :default #xff00ff
   (list -100 -100 100 0 1 0 100 -100 100 0 1 0 100 -100 -100 0 1 0 -100 -100 -100 0 1 0))
  (scene-add-filled-3d-convex-polygon-diffuse
   (default-scene) :default #xffff00ff
   (list -100 100 100 0 -1 0 100 100 100 0 -1 0 100 100 -100 0 -1 0 -100 100 -100 0 -1 0))
  (scene-add-filled-3d-convex-polygon-diffuse
   (default-scene) :default #xffff
   (list -100 100 100 0 0 1 100 100 100 0 0 1 100 -100 100 0 0 1 -100 -100 100 0 0 1))
  (scene-add-filled-3d-convex-polygon-diffuse
   (default-scene) :default #xffffff
   (list -100 100 -100 0 0 -1 100 100 -100 0 0 -1 100 -100 -100 0 0 -1 -100 -100 -100 0 0 -1))
  (values))

(cube-diffuse2)

;;scene-draw-filled-3d-convex-polygon-diffuse

(defun cube-diffuse3 ()
  (scene-draw-filled-3d-convex-polygon-diffuse
   (default-scene) :default #xff0000ff
   (list 100 100 100 1 0 0 100 -100 100 1 0 0 100 -100 -100 1 0 0 100 100 -100 1 0 0))
  (scene-draw-filled-3d-convex-polygon-diffuse
   (default-scene) :default #xff00ffff
   (list -100 100 100 -1 0 0 -100 -100 100 -1 0 0 -100 -100 -100 -1 0 0 -100 100 -100 -1 0 0))
  (scene-draw-filled-3d-convex-polygon-diffuse
   (default-scene) :default #xff00ff
   (list -100 -100 100 0 1 0 100 -100 100 0 1 0 100 -100 -100 0 1 0 -100 -100 -100 0 1 0))
  (scene-draw-filled-3d-convex-polygon-diffuse
   (default-scene) :default #xffff00ff
   (list -100 100 100 0 -1 0 100 100 100 0 -1 0 100 100 -100 0 -1 0 -100 100 -100 0 -1 0))
  (scene-draw-filled-3d-convex-polygon-diffuse
   (default-scene) :default #xffff
   (list -100 100 100 0 0 1 100 100 100 0 0 1 100 -100 100 0 0 1 -100 -100 100 0 0 1))
  (scene-draw-filled-3d-convex-polygon-diffuse
   (default-scene) :default #xffffff
   (list -100 100 -100 0 0 -1 100 100 -100 0 0 -1 100 -100 -100 0 0 -1 -100 -100 -100 0 0 -1))
  (values))

(setf (immediate-mode-work-function-6 (default-display))
      #'cube-diffuse3)

;;scene-add-filled-3d-convex-polygon-primitive-flat

;; In this example, we create a retained-mode cube as primitives
;; and we spin it from the lisp listener thread by updating the
;; model matrix on each of the primitives
;; even though it is mapping the primitives, it efficiently uses
;; memory as the draw-lists already exist and we are just adding
;; primitive cmd references to them
(defun cube1 ()
  (let ((right (scene-add-filled-3d-convex-polygon-primitive-flat
		(default-scene) :default nil #xff0000ff
		(list 100 100 100 100 -100 100 100 -100 -100 100 100 -100)))
	(left (scene-add-filled-3d-convex-polygon-primitive-flat
	       (default-scene) :default nil #xff00ffff
	       (list -100 100 100 -100 -100 100 -100 -100 -100 -100 100 -100)))
	(front (scene-add-filled-3d-convex-polygon-primitive-flat
		(default-scene) :default nil #xff00ff
		(list -100 -100 100 100 -100 100 100 -100 -100 -100 -100 -100)))
	(rear (scene-add-filled-3d-convex-polygon-primitive-flat
	       (default-scene) :default nil #xffff00ff
	       (list -100 100 100 100 100 100 100 100 -100 -100 100 -100)))
	(top (scene-add-filled-3d-convex-polygon-primitive-flat
	      (default-scene) :default nil #xffff
	      (list -100 100 100 100 100 100 100 -100 100 -100 -100 100)))
	(bottom (scene-add-filled-3d-convex-polygon-primitive-flat
		 (default-scene) :default nil #xffffff
		 (list -100 100 -100 100 100 -100 100 -100 -100 -100 -100 -100))))

    (list right left front rear top bottom)))

(defun spin-cube1 ()
  (let ((handles (cube1)))
    (loop repeat 600
	  do
	     (mapcar (lambda (handle)
		       (primitive-apply-transform
			(default-scene) handle (mrotate (meye 4) (vec3 1 1 1) (/ 2pi 600))))
		     handles)
	     (sleep 0.016))))

(spin-cube1)

;;scene-add-filled-3d-convex-polygon-flat

;; In this example, we create a retained-mode cube as a draw list
;; assigned to the group :cube, and we spin it from the lisp listener
;; thread by updating the model matrix on the group
;; only one model matrix is updated in this example, which is more
;; convenient and faster than mapping a list of primitives
;; but it takes up more memory, including video ram, because it's
;; creating a new draw list just for :cube
(defun cube2 ()
  (let ((group :cube))
    (scene-add-filled-3d-convex-polygon-flat
     (default-scene) group #xff0000ff
     (list 100 100 100 100 -100 100 100 -100 -100 100 100 -100))
    (scene-add-filled-3d-convex-polygon-flat
     (default-scene) group #xff00ffff
     (list -100 100 100 -100 -100 100 -100 -100 -100 -100 100 -100))
    (scene-add-filled-3d-convex-polygon-flat
     (default-scene) group #xff00ff
     (list -100 -100 100 100 -100 100 100 -100 -100 -100 -100 -100))
    (scene-add-filled-3d-convex-polygon-flat
     (default-scene) group #xffff00ff
     (list -100 100 100 100 100 100 100 100 -100 -100 100 -100))
    (scene-add-filled-3d-convex-polygon-flat
     (default-scene) group #xffff
     (list -100 100 100 100 100 100 100 -100 100 -100 -100 100))
    (scene-add-filled-3d-convex-polygon-flat
     (default-scene) group #xffffff
     (list -100 100 -100 100 100 -100 100 -100 -100 -100 -100 -100))
    group))

(defun spin-cube2 ()
  (let ((group (cube2)))
    (loop repeat 600
	  do (group-apply-transform (default-scene) group (mrotate (meye 4) (vec3 1 1 1) (/ 2pi 600)))
	     (sleep 0.016))))

(spin-cube2)

;;scene-draw-filled-3d-convex-polygon-flat

;; this example is like the cube2 example in that
;; draw lists are created for the cube, but in this case
;; the cube is immediate-mode, meaning it is created
;; new every frame, and should show the smoothest
;; animation performance.
;; note: the draw lists themselves should persist between
;; frames, even though the data in the draw list is erased
;; every frame, so it should not incur a performance
;; penalty every frame allocating the draw-list
;; at some point I will add the necessary functionality
;; to purge no longer used draw lists from the immediate
;; mode draw data so the user can avoid stale draw lists
;; which may be created when a lot of different group ids
;; are used

;; the main purpose of having immediate mode at all
;; is because there is often a case in animation to
;; need to change the mesh data often and smoothly
;; and this is combersome with retained-mode data
;; consider the example of animating a face of a
;; character while it is talking

;; but for computer aided design, retained-mode
;; graphics is king

(defun cube3 ()
  (let ((group :cube))
    (scene-draw-filled-3d-convex-polygon-flat
     (default-scene) group #xff0000ff
     (list 100 100 100 100 -100 100 100 -100 -100 100 100 -100))
    (scene-draw-filled-3d-convex-polygon-flat
     (default-scene) group #xff00ffff
     (list -100 100 100 -100 -100 100 -100 -100 -100 -100 100 -100))
    (scene-draw-filled-3d-convex-polygon-flat
     (default-scene) group #xff00ff
     (list -100 -100 100 100 -100 100 100 -100 -100 -100 -100 -100))
    (scene-draw-filled-3d-convex-polygon-flat
     (default-scene) group #xffff00ff
     (list -100 100 100 100 100 100 100 100 -100 -100 100 -100))
    (scene-draw-filled-3d-convex-polygon-flat
     (default-scene) group #xffff
     (list -100 100 100 100 100 100 100 -100 100 -100 -100 100))
    (scene-draw-filled-3d-convex-polygon-flat
     (default-scene) group #xffffff
     (list -100 100 -100 100 100 -100 100 -100 -100 -100 -100 -100))
    group))

(let* ((scene (default-scene))
       (draw-data (im-draw-data scene)))
  (defun spin-cube3 ()
    (let ((group (cube3)))
      (group-apply-transform-1 draw-data group (mrotate (meye 4) (vec3 1 1 1) (/ 2pi 600.0))))))

(setf (immediate-mode-work-function-6 (default-display))
      #'spin-cube3)  

;;scene-add-multicolor-3d-convex-polygon-primitive-diffuse

(defun cube-multicolor-diffuse1 ()
  (let ((right (scene-add-multicolor-3d-convex-polygon-primitive-diffuse
		(default-scene) :default nil
		(list 100 100 100 1 0 0  #xff0000ff
		      100 -100 100 1 0 0 #xff00ff
		      100 -100 -100 1 0 0 #xff00ffff
		      100 100 -100 1 0 0 #xffff)
		nil))
	(left (scene-add-multicolor-3d-convex-polygon-primitive-diffuse
	       (default-scene) :default nil 
	       (list -100 100 100 -1 0 0 #xffffff
		     -100 -100 100 -1 0 0 #xffffffff
		     -100 -100 -100 -1 0 0 #xffff00ff
		     -100 100 -100 -1 0 0 #x888888ff)
	       nil))
	(front (scene-add-multicolor-3d-convex-polygon-primitive-diffuse
		(default-scene) :default nil 
		(list -100 -100 100 0 1 0 #xffffffff
		      100 -100 100 0 1 0 #xff00ff
		      100 -100 -100 0 1 0 #xff00ffff
		      -100 -100 -100 0 1 0 #xffff00ff)
		nil))
	(rear (scene-add-multicolor-3d-convex-polygon-primitive-diffuse
	       (default-scene) :default nil 
	       (list -100 100 100 0 -1 0 #xffffff
		     100 100 100 0 -1 0  #xff0000ff
		     100 100 -100 0 -1 0 #xffff
		     -100 100 -100 0 -1 0 #x888888ff)
	       nil))
	(top (scene-add-multicolor-3d-convex-polygon-primitive-diffuse
	      (default-scene) :default nil 
	      (list -100 100 100 0 0 1 #xffffff
		    100 100 100 0 0 1  #xff0000ff
		    100 -100 100 0 0 1 #xff00ff
		    -100 -100 100 0 0 1 #xffffffff)
	      nil))
	(bottom (scene-add-multicolor-3d-convex-polygon-primitive-diffuse
		 (default-scene) :default nil 
		 (list -100 100 -100 0 0 -1 #x888888ff
		       100 100 -100 0 0 -1 #xffff
		       100 -100 -100 0 0 -1 #xff00ffff
		       -100 -100 -100 0 0 -1 #xffff00ff)
		 nil)))

    (list right left front rear top bottom)))

(cube-multicolor-diffuse1)

;;scene-add-multicolor-3d-convex-polygon-diffuse

(defun cube-multicolor-diffuse2 ()
  (let ((group :cube))
    (scene-add-multicolor-3d-convex-polygon-diffuse
     (default-scene) group
     (list 100 100 100 1 0 0  #xff0000ff
	   100 -100 100 1 0 0 #xff00ff
	   100 -100 -100 1 0 0 #xff00ffff
	   100 100 -100 1 0 0 #xffff))
    (scene-add-multicolor-3d-convex-polygon-diffuse
     (default-scene) group
     (list -100 100 100 -1 0 0 #xffffff
	   -100 -100 100 -1 0 0 #xffffffff
	   -100 -100 -100 -1 0 0 #xffff00ff
	   -100 100 -100 -1 0 0 #x888888ff))
    (scene-add-multicolor-3d-convex-polygon-diffuse
     (default-scene) group
     (list -100 -100 100 0 1 0 #xffffffff
	   100 -100 100 0 1 0 #xff00ff
	   100 -100 -100 0 1 0 #xff00ffff
	   -100 -100 -100 0 1 0 #xffff00ff))
    (scene-add-multicolor-3d-convex-polygon-diffuse
     (default-scene) group
     (list -100 100 100 0 -1 0 #xffffff
	   100 100 100 0 -1 0  #xff0000ff
	   100 100 -100 0 -1 0 #xffff
	   -100 100 -100 0 -1 0 #x888888ff))
    (scene-add-multicolor-3d-convex-polygon-diffuse
     (default-scene) group
     (list -100 100 100 0 0 1 #xffffff
	   100 100 100 0 0 1  #xff0000ff
	   100 -100 100 0 0 1 #xff00ff
	   -100 -100 100 0 0 1 #xffffffff))
    (scene-add-multicolor-3d-convex-polygon-diffuse
     (default-scene) group
     (list -100 100 -100 0 0 -1 #x888888ff
	   100 100 -100 0 0 -1 #xffff
	   100 -100 -100 0 0 -1 #xff00ffff
	   -100 -100 -100 0 0 -1 #xffff00ff))
    group))

(cube-multicolor-diffuse2)

;;scene-draw-multicolor-3d-convex-polygon-diffuse

(defun cube-multicolor-diffuse3 ()
  (let ((group :cube))
    (scene-draw-multicolor-3d-convex-polygon-diffuse
     (default-scene) group
     (list 100 100 100 1 0 0  #xff0000ff
	   100 -100 100 1 0 0 #xff00ff
	   100 -100 -100 1 0 0 #xff00ffff
	   100 100 -100 1 0 0 #xffff))
    (scene-draw-multicolor-3d-convex-polygon-diffuse
     (default-scene) group
     (list -100 100 100 -1 0 0 #xffffff
	   -100 -100 100 -1 0 0 #xffffffff
	   -100 -100 -100 -1 0 0 #xffff00ff
	   -100 100 -100 -1 0 0 #x888888ff))
    (scene-draw-multicolor-3d-convex-polygon-diffuse
     (default-scene) group
     (list -100 -100 100 0 1 0 #xffffffff
	   100 -100 100 0 1 0 #xff00ff
	   100 -100 -100 0 1 0 #xff00ffff
	   -100 -100 -100 0 1 0 #xffff00ff))
    (scene-draw-multicolor-3d-convex-polygon-diffuse
     (default-scene) group
     (list -100 100 100 0 -1 0 #xffffff
	   100 100 100 0 -1 0  #xff0000ff
	   100 100 -100 0 -1 0 #xffff
	   -100 100 -100 0 -1 0 #x888888ff))
    (scene-draw-multicolor-3d-convex-polygon-diffuse
     (default-scene) group
     (list -100 100 100 0 0 1 #xffffff
	   100 100 100 0 0 1  #xff0000ff
	   100 -100 100 0 0 1 #xff00ff
	   -100 -100 100 0 0 1 #xffffffff))
    (scene-draw-multicolor-3d-convex-polygon-diffuse
     (default-scene) group
     (list -100 100 -100 0 0 -1 #x888888ff
	   100 100 -100 0 0 -1 #xffff
	   100 -100 -100 0 0 -1 #xff00ffff
	   -100 -100 -100 0 0 -1 #xffff00ff))
    group))

(setf (immediate-mode-work-function-6 (default-display))
      #'cube-multicolor-diffuse3)


;;scene-add-multicolor-3d-convex-polygon-primitive-flat

(defun cube-multicolor-flat1 ()
  (let ((right (scene-add-multicolor-3d-convex-polygon-primitive-flat
		(default-scene) :default nil
		(list 100 100 100 #xff0000ff
		      100 -100 100 #xff00ff
		      100 -100 -100 #xff00ffff
		      100 100 -100 #xffff)))
	(left (scene-add-multicolor-3d-convex-polygon-primitive-flat
	       (default-scene) :default nil 
	       (list -100 100 100 #xffffff
		     -100 -100 100 #xffffffff
		     -100 -100 -100 #xffff00ff
		     -100 100 -100 #x888888ff)))
	(front (scene-add-multicolor-3d-convex-polygon-primitive-flat
		(default-scene) :default nil 
		(list -100 -100 100 #xffffffff
		      100 -100 100 #xff00ff
		      100 -100 -100 #xff00ffff
		      -100 -100 -100 #xffff00ff)))
	(rear (scene-add-multicolor-3d-convex-polygon-primitive-flat
	       (default-scene) :default nil 
	       (list -100 100 100 #xffffff
		     100 100 100  #xff0000ff
		     100 100 -100 #xffff
		     -100 100 -100 #x888888ff)))
	(top (scene-add-multicolor-3d-convex-polygon-primitive-flat
	      (default-scene) :default nil 
	      (list -100 100 100 #xffffff
		    100 100 100  #xff0000ff
		    100 -100 100 #xff00ff
		    -100 -100 100 #xffffffff)))
	(bottom (scene-add-multicolor-3d-convex-polygon-primitive-flat
		 (default-scene) :default nil 
		 (list -100 100 -100 #x888888ff
		       100 100 -100 #xffff
		       100 -100 -100 #xff00ffff
		       -100 -100 -100 #xffff00ff))))

    (list right left front rear top bottom)))

(cube-multicolor-flat1)

;;scene-add-multicolor-3d-convex-polygon-flat

(defun cube-multicolor-flat2 ()
  (let ((group :cube))
    (scene-add-multicolor-3d-convex-polygon-flat
     (default-scene) group
     (list 100 100 100 #xff0000ff
	   100 -100 100 #xff00ff
	   100 -100 -100 #xff00ffff
	   100 100 -100 #xffff))
    (scene-add-multicolor-3d-convex-polygon-flat
     (default-scene) group 
     (list -100 100 100 #xffffff
	   -100 -100 100 #xffffffff
	   -100 -100 -100 #xffff00ff
	   -100 100 -100 #x888888ff))
    (scene-add-multicolor-3d-convex-polygon-flat
     (default-scene) group 
     (list -100 -100 100 #xffffffff
	   100 -100 100 #xff00ff
	   100 -100 -100 #xff00ffff
	   -100 -100 -100 #xffff00ff))
    (scene-add-multicolor-3d-convex-polygon-flat
     (default-scene) group 
     (list -100 100 100 #xffffff
	   100 100 100  #xff0000ff
	   100 100 -100 #xffff
	   -100 100 -100 #x888888ff))
    (scene-add-multicolor-3d-convex-polygon-flat
     (default-scene) group 
     (list -100 100 100 #xffffff
	   100 100 100  #xff0000ff
	   100 -100 100 #xff00ff
	   -100 -100 100 #xffffffff))
    (scene-add-multicolor-3d-convex-polygon-flat
     (default-scene) group 
     (list -100 100 -100 #x888888ff
	   100 100 -100 #xffff
	   100 -100 -100 #xff00ffff
	   -100 -100 -100 #xffff00ff))
    group))

(cube-multicolor-flat2)

;;scene-draw-multicolor-3d-convex-polygon-flat

(defun cube-multicolor-flat3 ()
  (let ((group :cube))
    (scene-draw-multicolor-3d-convex-polygon-flat
     (default-scene) group
     (list 100 100 100 #xff0000ff
	   100 -100 100 #xff00ff
	   100 -100 -100 #xff00ffff
	   100 100 -100 #xffff))
    (scene-draw-multicolor-3d-convex-polygon-flat
     (default-scene) group 
     (list -100 100 100 #xffffff
	   -100 -100 100 #xffffffff
	   -100 -100 -100 #xffff00ff
	   -100 100 -100 #x888888ff))
    (scene-draw-multicolor-3d-convex-polygon-flat
     (default-scene) group 
     (list -100 -100 100 #xffffffff
	   100 -100 100 #xff00ff
	   100 -100 -100 #xff00ffff
	   -100 -100 -100 #xffff00ff))
    (scene-draw-multicolor-3d-convex-polygon-flat
     (default-scene) group 
     (list -100 100 100 #xffffff
	   100 100 100  #xff0000ff
	   100 100 -100 #xffff
	   -100 100 -100 #x888888ff))
    (scene-draw-multicolor-3d-convex-polygon-flat
     (default-scene) group 
     (list -100 100 100 #xffffff
	   100 100 100  #xff0000ff
	   100 -100 100 #xff00ff
	   -100 -100 100 #xffffffff))
    (scene-draw-multicolor-3d-convex-polygon-flat
     (default-scene) group 
     (list -100 100 -100 #x888888ff
	   100 100 -100 #xffff
	   100 -100 -100 #xff00ffff
	   -100 -100 -100 #xffff00ff))
    (values)))

(setf (immediate-mode-work-function-6 (default-display))
      #'cube-multicolor-flat3)

;;scene-add-textured-3d-triangle-list-primitive-flat
;;scene-add-textured-3d-triangle-list-flat           
;;scene-draw-textured-3d-triangle-list-flat
;;scene-add-textured-3d-triangle-strip-primitive-flat

;;scene-add-filled-sphere-primitive-diffuse

(scene-add-filled-sphere-primitive-diffuse (default-scene) :default nil #xfffff 0 0 0 50 nil 200)

;;scene-add-filled-ellipsoid-primitive-diffuse

(scene-add-filled-ellipsoid-primitive-diffuse (default-scene) :default nil #xffffff 100 100 100 50 60 70 nil 200)

;;scene-add-filled-sphere-diffuse

(scene-add-filled-sphere-diffuse (default-scene) :default #xfff000ff -100 -100 100 50 200)

;;scene-draw-filled-sphere-diffuse

(defun scene-draw-filled-sphere-diffuse-test ()
  (scene-draw-filled-sphere-diffuse (default-scene) :default #xff00ffff -100 100 100 50 200))

(setf (immediate-mode-work-function-6 (default-display))
      #'scene-draw-filled-sphere-diffuse-test)

;;scene-add-text-primitive

(scene-add-text-primitive (default-scene) :default nil (frame-manager-default-font clim:*default-frame-manager*) #xff00ffff 100 100 "The quick brown fox jumped over the lazy dog.")

;;scene-add-text

(scene-add-text (default-scene) :default (frame-manager-default-font clim:*default-frame-manager*) #xfff00fff 100 200 "Foo Bar Baz.")

;;scene-draw-text

(let ((font (frame-manager-default-font clim:*default-frame-manager*))
      (scene (default-scene)))
  (defun scene-draw-text-test ()
    (scene-draw-text scene :default font #x8ff00dff 100 300 "Baz Bar Foo.")))

(setf (immediate-mode-work-function-6 (default-display))
      #'scene-draw-text-test)
