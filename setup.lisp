(in-package :krma)

(defvar *identity-matrix*)
(defvar *multiply-matrix-function*)

(defconstant +buffer-alignment+ 128)

(defvar *sampler*)

(setq *multiply-matrix-function* #'3d-matrices::m*)
(setq *identity-matrix* (3d-matrices::meye 4))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (eq 8 (cffi:foreign-type-size :pointer))
      (cffi:defctype size-t :uint64)
      (if (eq 4 (cffi:foreign-type-size :pointer))
          (cffi:defctype size-t :uint32)
          (error "don't know how to define size-t"))))

(defconstant +draw-list-alloc-size+ 512)
(defparameter +nullptr+ (cffi-sys:null-pointer))

(defconstant 2pi #.(* 2.0d0 pi))

(defconstant +tex-white-pixel-u+ 0.0f0)
(defconstant +tex-white-pixel-v+ 0.0f0)

(defvar *white-texture*)
(defvar *default-color* #xffffffff)
(defvar *identity-matrix*)
(defvar *default-point-size* 4.0f0)
(defvar *default-line-thickness* 2.0f0)
(defvar *default-number-of-segments* 64)
(defvar *default-light-position* (vec3 10000 10000 10000))
