(in-package :krma)

(defclass material-mixin ()
  ((name :initform "default" :initarg :name :accessor material-name :type (or string symbol))
   (ambient :initform #(1.0 0.5 0.31 1.0) :initarg :ambient :accessor material-ambient :type color)
   (diffuse :initform #(1.0 0.5 0.31 1.0) :initarg :diffuse :accessor material-diffuse :type color)
   (specular :initform #(0.5 0.5 0.5 1.0) :initarg :specular :accessor material-specular :type color)
   (shininess :initform 32.0f0 :initarg :shininess :accessor material-shininess :type real))
  (:documentation "Abstract base class for all material types in krma."))

(defclass standard-material (material-mixin)
  ()
  (:documentation "Concrete class for materials based on material-mixin."))

(defun make-material (name &optional
			     (ambient #(1.0 0.5 0.31 1.0))
			     (diffuse #(1.0 0.5 0.31 1.0))
			     (specular #(0.5 0.5 0.5 1.0))
			     (shininess 32.0f0))
  "Function creates a standard-material object. The required argument `name' should be a string or symbol.  The keyword arguments `ambient', `diffuse', and `specular' should be values which satisfy krma:color-p.  Shininess must be a real number."
  (make-instance 'standard-material
		 :name name
		 :ambient ambient
		 :diffuse diffuse
		 :specular specular
		 :shininess shininess))

(when (not (boundp '*default-material*))
  (setq *default-material* (make-material "default")))
