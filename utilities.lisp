(in-package :krma)

(defun d2r (d)
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


  

(defun mperspective-vulkan (fovy aspect-ratio near far)
  "https://github.com/PacktPublishing/Vulkan-Cookbook/blob/master/Library/Source%20Files/10%20Helper%20Recipes/04%20Preparing%20a%20perspective%20projection%20matrix.cpp"
  ;; switched from column major to row major
  (let* ((zero 0.0)
         (f (/ 1.0 (coerce (tan (* 0.5 (d2r fovy))) *read-default-float-format*))) ;; focal length
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

(declaim (inline clampf))
(defun clampf (number)
  "Clamp real number to single-float limits."
  (declare (type real number))
  (block nil
    (when (typep number 'single-float)
      (return number))
    (when (zerop number)
      (return 0.0f0))
    (when (< (cl:the double-float (load-time-value (/ least-negative-single-float 2.0d0)))
             number
             (cl:the double-float (load-time-value (/ least-positive-single-float 2.0d0))))
      (return 0.0f0))
    (when (minusp number)
      (when (> number (cl:the single-float least-negative-single-float))
        (return least-negative-single-float))
      (when (< number (cl:the single-float most-negative-single-float))
        (return most-negative-single-float))
      (return (coerce number 'single-float)))
    (when (< number (cl:the single-float least-positive-single-float))
      (return least-positive-single-float))
    (when (> number (cl:the single-float most-positive-single-float))
      (return most-positive-single-float))
    (coerce number 'single-float)))
