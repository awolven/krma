(in-package :krma)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *debug* vk::*debug*)
  (defvar *muffle-compilation-notes* t))

(defvar *default-application-class* 'krma-test-application)
(defvar *default-window-class* 'krma-window)

(defconstant +buffer-alignment+ 256) ;; todo: query non-coherent-atom-size of physical device
(defconstant +select-box-depth+ 128)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (eq 8 (cffi:foreign-type-size :pointer))
      (cffi:defctype size-t :uint64)
      (if (eq 4 (cffi:foreign-type-size :pointer))
          (cffi:defctype size-t :uint32)
          (error "don't know how to define size-t"))))

(defconstant +draw-list-alloc-size+ 64)
(defparameter +nullptr+ (cffi-sys:null-pointer))

(defconstant 2pi #.(* 2.0d0 pi))

(defconstant +tex-white-pixel-u+ 0.0f0)
(defconstant +tex-white-pixel-v+ 0.0f0)

(defvar *default-znear* 0.001)
(defvar *default-zfar* 3000.0)

(defvar *white-texture*)
(defvar *default-color* #xffffffff)
(defvar *identity-matrix* (3d-matrices::meye 4))
(defvar *default-point-size* 4.0f0)
(defvar *default-line-thickness* 2.0f0)
(defvar *default-number-of-segments* 64)
(defvar *default-light-position* (vec3 10000 10000 10000))
(defvar *default-diffuse-color* #xffffffff)
(defvar *default-specular-color* #xffffffff)
(defvar *default-constant-attenuation* 0.0f0)
(defvar *default-linear-attenuation* 1.0f0)
(defvar *default-quadratic-attenuation* 0.0f0)
(defvar *default-spot-cutoff* 180.0f0)
(defvar *default-spot-exponent* 10.0f0)
(defvar *default-spot-direction* (vec3 0 1 0))
(defvar *default-scene-ambient* #xffffffff)
(defvar *default-material*)
(defvar *compact-trigger* 1/3) ;; must be real number between 0 and 1
