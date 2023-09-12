(in-package :krma)

(defun add-ellipse (center major minor start-angle end-angle)
  (flet ((ellipse-point (angle)
	   (v+ center (vec2 (* major (cos angle)) (- (* minor (sin angle)))))))
    (let ((dtheta (/ (- end-angle start-angle) 64.0d0)))
      (add-2d-polyline (loop for i from 0 to 64
			  append (let ((point (ellipse-point (+ start-angle (* i dtheta)))))
				   (list (vx point) (vy point))))))))

(defun add-ellipse-by-foci (focus1 focus2 major start-angle end-angle)
  (let* ((center (v* 0.5d0 (v+ focus1 focus2)))
	 (vec (v- focus2 focus1))
	 (focal-distance (vlength vec))
	 (minor (- (sqrt (* major major)) (sqrt (* focal-distance focal-distance))))
	 (rotation (atan (vy vec) (vx vec)))
	 (mat (3dm.d::mat2 (cos rotation) (- (sin rotation))
			   (sin rotation)    (cos rotation))))
    (flet ((ellipse-point (angle)
	     (v+ center (m* mat (vec2 (* major (cos angle)) (- (* minor (sin angle))))))))
      (let ((dtheta (/ (- end-angle start-angle) 64.0d0)))
	(add-2d-polyline (loop for i from 0 to 64
			    append (let ((point (ellipse-point (+ start-angle (* i dtheta)))))
				     (list (vx point) (vy point)))))))))
      
    
