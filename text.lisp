(in-package :krma)

(eval-when (:compile-toplevel :load-toplevel)
  (when krma::*debug*
    (declaim (optimize (safety 3) (debug 3)))))

(defclass font ()
  ((cache-file :initarg :cache-file :accessor font-cache-file)
   (px-range :accessor font-px-range)
   (data :initarg :data :accessor font-data)
   (atlas :accessor font-atlas)))

(defun relative-pathname (pathname)
  (if (pathname-type pathname)
      (pathname (concatenate 'string (pathname-name pathname) "." (pathname-type pathname)))
      (pathname (pathname-name pathname))))

(defun vulkan-make-font (device queue sampler descriptor-set-layout descriptor-pool
                         command-buffer &key (cache-file "rm16cache.json")
                                          (bpp 4))
  (uiop/filesystem:with-current-directory
      ((submodule-file "krma-fonts/"))
    (with-open-file (stream cache-file :external-format #+ccl :utf-8 #-ccl :utf8)
      (let ((font (make-instance 'font :data (3b-bmfont-json:read-bmfont-json stream)
                                       :cache-file cache-file)))
	(setf (font-px-range font) (float (getf (3b-bmfont:distance-field (font-data font))
						:distance-range)
                                          0.0f0))
	(let ((file (concatenate 'string (pathname-name cache-file) ".png")))
          (pngload:with-png-in-static-vector (png file :flip-y nil)
            (let* ((bitmap (make-array (* (pngload:width png) (pngload:height png) 4)
                                       :element-type '(unsigned-byte 8)
                                       :initial-element #xff))
                   (b2 (make-array (list (pngload:width png) (pngload:height png) 4)
                                   :element-type '(unsigned-byte 8)
                                   :displaced-to bitmap))
                   (b3 (make-array (list (pngload:width png) (pngload:height png))
                                   :element-type '(unsigned-byte 8)
                                   :displaced-to (pngload:data png))))

              (loop for j from 0 below (pngload:height png)
                    do (loop for i from 0 below (pngload:width png)
                             do (setf (aref b2 i j 3) (aref b3 i j))))

              (setf (font-atlas font)
                    (make-vulkan-texture device queue sampler descriptor-set-layout descriptor-pool
					 command-buffer bpp
					 (pngload:data png) (pngload:width png) (pngload:height png)))))
          font)))))
