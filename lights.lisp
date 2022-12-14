(in-package :krma)

(defcstruct light
  (pos-x :float)
  (pos-y :float)
  (pos-z :float)
  (pos-w :float)
  (spot-direction-x :float)
  (spot-direction-y :float)
  (spot-direction-z :float)
  (spot-direction-w :float)
  (diffuse :unsigned-int)
  (specular :unsigned-int)
  (constant-attenuation :float)
  (linear-attenuation :float)
  (quadratic-attenuation :float)
  (spot-cutoff :float)
  (spot-exponent :float)
  (padding :float))

(defclass light-mixin ()
  ((position :initform *default-light-position* :initarg :position :accessor light-position :type (or vec3 vec4))
   (diffuse :initform *default-diffuse-color* :initarg :diffuse :accessor light-diffuse :type color)
   (specular :initform *default-specular-color* :initarg :specular :accessor light-specular :type color)
   (constant-attenuation :initform *default-constant-attenuation* :initarg :constant-attenuation :accessor light-constant-attenuation :type real)
   (linear-attenuation :initform *default-linear-attenuation* :initarg :linear-attenuation :accessor light-linear-attenuation :type real)
   (quadratic-attenuation :initform *default-quadratic-attenuation* :initarg :quadratic-attenuation :accessor light-quadratic-attenuation :type real)
   (spot-cutoff :initform *default-spot-cutoff* :initarg :spot-cutoff :accessor light-spot-cutoff :type real)
   (spot-exponent :initform *default-spot-exponent* :initarg :spot-exponent :accessor light-spot-exponent :type real)
   (spot-direction :initform *default-spot-direction* :initarg :spot-direction :accessor light-spot-direction :type (or vec3 vec4)))
  (:documentation "Abstract base class for different types of lights in krma."))


(defclass directional-light (light-mixin)
  ()
  (:documentation "A light class based on light-mixin."))

(defclass point-light (light-mixin)
  ()
  (:documentation "A light class based on light-mixin."))
  
(defclass spot-light (light-mixin)
  ()
  (:documentation "A light class based on light-mixin."))
