(in-package :krma)

(defun icosahedron-vertices1 (radius)
  (let* ((h-angle (* (/ pi 180) 72))
	 (v-angle (atan 1/2))
	 (vertices (make-list (* 12 3)))
	 (h-angle1 (- (- (/ pi 2)) (/ h-angle 2)))
	 (h-angle2 (- (/ pi 2)))
	 (i1)
	 (i2))

    (setf (elt vertices 0) 0)
    (setf (elt vertices 1) 0)
    (setf (elt vertices 2) radius)

    (loop for i from 1 to 5
	  with z
	  with xy
	  do (setq i1 (* i 3))
	     (setq i2 (* (+ i 5) 3))

	     (setq z (* radius (sin v-angle)))
	     (setq xy (* radius (cos v-angle)))

	     (setf (elt vertices i1) (* xy (cos h-angle1)))
	     (setf (elt vertices i2) (* xy (cos h-angle2)))
	     (setf (elt vertices (1+ i1)) (* xy (sin h-angle1)))
	     (setf (elt vertices (1+ i2)) (* xy (cos h-angle2)))
	     (setf (elt vertices (+ i1 2)) z)
	     (setf (elt vertices (+ i2 2)) (- z))

	     (incf h-angle1 h-angle)
	     (incf h-angle2 h-angle))

    (setq i1 (+ 3 (* 2 i1)))
    (setf (elt vertices i1) 0)
    (setf (elt vertices (1+ i1)) 0)
    (setf (elt vertices (+ i1 2)) (- radius))

    vertices))
