(in-package :krma)


(defun mperspective-vulkan (fovy aspect near far)
  "https://github.com/PacktPublishing/Vulkan-Cookbook/blob/master/Library/Source%20Files/10%20Helper%20Recipes/04%20Preparing%20a%20perspective%20projection%20matrix.cpp"
  ;; switched from column major to row major
  (let* ((zero 0.0)
	     (focal-length (/ 1.0 (coerce (tan (* 0.5 (d2r fovy))) *read-default-float-format*)))
	     (n-f (- near far))
	     (A (/ far n-f))
	     (B (/ (* near far) n-f))
	     (x (/ focal-length aspect))
	     (y (- focal-length)))

    (3d-matrices::mat x      zero zero   zero
                      zero   y    zero   zero
                      zero   zero A      B
                      zero   zero -1.0 1.0))) ;; is this last digit supposed to be zero or is it supposed to be one?

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
