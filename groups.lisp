(in-package :krma)

(defstruct (group
	    (:conc-name "GROUP-")
	    (:constructor make-group
		(name
		 &optional (model-matrix nil)
		   (color-override nil) (material (make-material "default")))))
  "Internal structure for storing properties of a group in krma."
  (name)
  (color-override nil)
  (model-matrix nil)
  (material nil))
