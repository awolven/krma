(in-package :krma)

(declaim (optimize (safety 3) (speed 3) (debug 3))
         (sb-ext:muffle-conditions sb-ext:compiler-note))

(cffi:defcfun ("memcpy" memcpy) :pointer
  (dst :pointer)
  (src :pointer)
  (count size-t))

(declaim (type double-float d2r))
(defun d2r (d)
  (declare (type real d))
  (* d #.(/ pi 180)))

(defun lookat-rh (eye target up) ;; sanity check, 3d-matrices:mlookat produces same
  (let* ((zaxis (vunit (v- eye target)))
         (xaxis (vunit (vc up zaxis)))
         (yaxis (vc zaxis xaxis))
         (orientation
           (mat (vx xaxis) (vy xaxis) (vz xaxis) 0
                (vx yaxis) (vy yaxis) (vz yaxis) 0
                (vx zaxis) (vy zaxis) (vz zaxis) 0
                0          0          0          1))
         (translation
           (mat 1 0 0 (- (vx eye))
                0 1 0 (- (vy eye))
                0 0 1 (- (vz eye))
                0 0 0 1)))
    (m* orientation translation)))

(defun lookat-lh (eye target up)
  (let* ((zaxis (vunit (v- target eye)))
         (xaxis (vunit (vc up zaxis)))
         (yaxis (vc zaxis xaxis))
         (orientation
           (mat (vx xaxis) (vy xaxis) (vz xaxis) 0
                (vx yaxis) (vy yaxis) (vz yaxis) 0
                (vx zaxis) (vy zaxis) (vz zaxis) 0
                0          0          0          1))
         (translation
           (mat 1 0 0 (- (vx eye))
                0 1 0 (- (vy eye))
                0 0 1 (- (vz eye))
                0 0 0 1)))
    (m* orientation translation)))

(defun lookat-lh-2 (eye target up)
  "https://learn.microsoft.com/en-us/previous-versions/windows/desktop/bb281710(v=vs.85)"
  (let* ((zaxis (vunit (v- target eye)))
         (xaxis (vunit (vc up zaxis)))
         (yaxis (vc zaxis xaxis)))
    (mat (vx xaxis)         (vy xaxis)         (vz xaxis)         0
         (vx yaxis)         (vy yaxis)         (vz yaxis)         0
         (vx zaxis)         (vy zaxis)         (vz zaxis)         0
         (- (v. xaxis eye)) (- (v. yaxis eye)) (- (v. zaxis eye)) 1)))


  

(defun mperspective-vulkan (fovy aspect-ratio near far)
  (declare (type real fovy aspect-ratio near far))
  "https://github.com/PacktPublishing/Vulkan-Cookbook/blob/master/Library/Source%20Files/10%20Helper%20Recipes/04%20Preparing%20a%20perspective%20projection%20matrix.cpp"
  ;; switched from column major to row major
  (setq far (coerce far '#.*read-default-float-format*))
  (setq near (coerce near '#.*read-default-float-format*))
  (setq aspect-ratio (coerce aspect-ratio '#.*read-default-float-format*))
  (let* ((zero 0.0)
         (f (/ 1.0 (coerce (tan (cl:the double-float (* 0.5 (d2r fovy)))) '#.*read-default-float-format*))) ;; focal length
	     (near-far (- near far))
	     (A (/ far near-far))
	     (B (/ (* near far) near-far))
	     (x (/ f aspect-ratio))
	     (y (- f)))

    (3d-matrices::mat x      zero zero   zero
                      zero   y    zero   zero
                      zero   zero A      B
                      zero   zero -1.0   zero)))

(defun mortho-vulkan (left right bottom top near far)
  "https://github.com/PacktPublishing/Vulkan-Cookbook/blob/master/Library/Source%20Files/10%20Helper%20Recipes/05%20Preparing%20an%20orthographic%20projection%20matrix.cpp"
  (let ((f2 2)
	(f0 0)
	(f1 1)
	(r right)
	(l left)
	(b bottom)
	(u top)
	(n near)
	(f far))

    (3d-matrices::mat (/ f2 (- r l)) f0             f0              (- (/ (+ r l) (- r l)))
                      f0             (/ f2 (- b u)) f0              (- (/ (+ b u) (- b u)))
                      f0             f0             (/ f1 (- n f))  (/ n (- n f))
                      f0             f0             f0              f1)))

(declaim (inline clampf) (type single-float clampf))
(defun clampf (number)
  "Clamp real number to single-float limits."
  (block nil
    (when (typep number 'single-float)
      (return number))
    (etypecase number
      (real
       (etypecase number
	 (double-float
	  (when (= number 0.0d0)
	    (return 0.0f0))
	  (when (< (cl:the double-float (load-time-value (/ least-negative-single-float 2.0d0)))
		   number
		   (cl:the double-float (load-time-value (/ least-positive-single-float 2.0d0))))
	    (return 0.0f0))
	  (when (< number 0.0d0)
	    (when (> number (cl:the single-float least-negative-single-float))
	      (return least-negative-single-float))
	    (when (< number (cl:the single-float most-negative-single-float))
	      (return most-negative-single-float))
	    (return (coerce number 'single-float)))
	  (when (< number (cl:the single-float least-positive-single-float))
	    (return least-positive-single-float))
	  (when (> number (cl:the single-float most-positive-single-float))
	    (return most-positive-single-float))
	  (coerce number 'single-float))
      
	 (integer
	  (when (= number 0)
	    (return 0.0f0))
	  (when (< number (cl:the single-float most-negative-single-float))
	    (return most-negative-single-float))
	  (when (> number (cl:the single-float most-positive-single-float))
	    (return most-positive-single-float))
	  (coerce number 'single-float))
	 (rational
	  (when (< (cl:the double-float (load-time-value (/ least-negative-single-float 2.0d0)))
		   number
		   (cl:the double-float (load-time-value (/ least-positive-single-float 2.0d0))))
	    (return 0.0f0))
	  (when (< number 0)
	    (when (> number (cl:the single-float least-negative-single-float))
	      (return least-negative-single-float))
	    (return (coerce number 'single-float)))
	  (when (< number (cl:the single-float least-positive-single-float))
	    (return least-positive-single-float))
	  (coerce number 'single-float)))))))
    
(declaim (inline canonicalize-color) (type (unsigned-byte 32) color))
(defun canonicalize-color (color)
  (etypecase color
    ((unsigned-byte 32) color)
    (vector
     (let ((r (cl:the (integer 0 #xff) (round (* #xff (aref color 0)))))
	   (g (cl:the (integer 0 #xff) (round (* #xff (aref color 1)))))
	   (b (cl:the (integer 0 #xff) (round (* #xff (aref color 2)))))
	   (a (cl:the (integer 0 #xff) (round (* #xff (aref color 3))))))
       (cl:the (unsigned-byte 32) (logior (ash r 24) (ash g 16) (ash b 8) a))))))
