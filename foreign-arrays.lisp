(in-package :krma)

(defstruct (foreign-adjustable-array
            (:conc-name "FOREIGN-ARRAY-"))
  (ptr +nullptr+ :type sb-sys:system-area-pointer)
  (fill-pointer 0 :type fixnum)
  (allocated-count +draw-list-alloc-size+ :type fixnum)
  (foreign-type nil)
  (foreign-type-size 1 :type fixnum))

(defstruct (index-array
            (:include foreign-adjustable-array)
            (:constructor make-index-array
                (&optional
                   (allocated-count +draw-list-alloc-size+)
                   (foreign-type :unsigned-short)
                   (fill-pointer 0)
                   (ptr (foreign-alloc :unsigned-short :count allocated-count))
                   (foreign-type-size (foreign-type-size foreign-type))))))

(defun index-array-push-extend (index-array index)
  (declare (type index-array index-array))
  (declare (type (unsigned-byte 32) index))
  (let ((err nil))
    (flet ((upgrade-index-array! ()
             (when err
               (error "unknown type error in index-array-push-extend"))
             (with-slots (ptr
                          allocated-count
                          fill-pointer
                          foreign-type
                          foreign-type-size)
                 index-array
               (let ((new-ptr (foreign-alloc :unsigned-int :count allocated-count)))
                 (loop for i from 0 below fill-pointer ;; copy the indexes
                       do (setf (mem-aref new-ptr :unsigned-int i)
                                (mem-aref ptr :unsigned-short i)))
                 (foreign-free ptr)
                 (setf foreign-type :unsigned-int
                       foreign-type-size (load-time-value (foreign-type-size :unsigned-int))
                       ptr new-ptr
                       err t)
                 (values)))))
    (with-slots (ptr
                 allocated-count
                 fill-pointer
                 foreign-type
                 foreign-type-size)
        index-array

      (unless (< fill-pointer allocated-count)
        (let* ((new-count (* 2 allocated-count))
               (new-array (foreign-alloc foreign-type :count new-count))
               (old-array ptr))
          (memcpy new-array old-array (* fill-pointer foreign-type-size))
          (setf ptr new-array)
          (setf allocated-count new-count)
          (foreign-free old-array)))

      (tagbody
       retry
         (handler-case
             (setf (mem-aref (foreign-array-ptr index-array)
                             (foreign-array-foreign-type index-array)
                             (foreign-array-fill-pointer index-array))
                   index)
           (type-error ()
             (format t "~&upgrading index array from unsigned-short to unsigned-int.")
             (finish-output)
             (upgrade-index-array!)
             (go retry)))
         (incf fill-pointer))))))

(defstruct (vertex-array (:include foreign-adjustable-array)))

;; we use a textured vertex for non-textured (standard) use to be able to use the same shader and draw-lists for simplicity
;; we simply put the tex coordinates of a white pixel and blend as if we were drawing text
(defcstruct textured-2d-vertex
  (x :float)
  (y :float)
  (u :float)
  (v :float)
  (col :uint32))

(defstruct (textured-2d-vertex-array
            (:include vertex-array)
            (:constructor make-textured-2d-vertex-array
                (&optional
                   (allocated-count +draw-list-alloc-size+)
                   (foreign-type '(:struct textured-2d-vertex))
                   (fill-pointer 0)
                   (ptr (foreign-alloc foreign-type :count allocated-count))
                   (foreign-type-size (foreign-type-size foreign-type))))))

(defun textured-2d-vertex-array-push-extend (textured-2d-vertex-array x y u v color)
  (declare (type textured-2d-vertex-array textured-2d-vertex-array))
  (declare (type real x y))
  (declare (type (unsigned-byte 32) color))
  (let ((vertex-type '(:struct textured-2d-vertex))
        (vertex-type-size (load-time-value (foreign-type-size '(:struct textured-2d-vertex)))))
    (with-slots (ptr fill-pointer allocated-count) textured-2d-vertex-array
      (unless (< fill-pointer allocated-count)
        (let* ((new-count (* 2 allocated-count))
               (new-array (foreign-alloc vertex-type :count new-count))
               (old-array ptr))
          (memcpy new-array old-array (* fill-pointer vertex-type-size))
          (setf ptr new-array)
          (setf allocated-count new-count)
          (foreign-free old-array)))
      (let ((ptr (mem-aptr ptr vertex-type fill-pointer)))
        (setf (foreign-slot-value ptr vertex-type 'x) (clampf x)
              (foreign-slot-value ptr vertex-type 'y) (clampf y)
              (foreign-slot-value ptr vertex-type 'u) (clampf u)
              (foreign-slot-value ptr vertex-type 'v) (clampf v)
              (foreign-slot-value ptr vertex-type 'col) color)
        (prog1 fill-pointer
          (incf fill-pointer))))))

(defun standard-2d-vertex-array-push-extend (vertex-array x y color)
  (declare (type textured-2d-vertex-array vertex-array))
  (declare (type real x y))
  (declare (type (unsigned-byte 32) color))
  (textured-2d-vertex-array-push-extend vertex-array x y +tex-white-pixel-u+ +tex-white-pixel-v+ color))

(defcstruct textured-3d-vertex
  (x :float)
  (y :float)
  (z :float)
  (u :float)
  (v :float)
  (col :uint32))

(defstruct (textured-3d-vertex-array
            (:include vertex-array)
            (:constructor make-textured-3d-vertex-array
                (&optional
                   (allocated-count +draw-list-alloc-size+)
                   (foreign-type '(:struct textured-3d-vertex))
                   (fill-pointer 0)
                   (ptr (foreign-alloc foreign-type :count allocated-count))
                   (foreign-type-size (foreign-type-size foreign-type))))))

(defun textured-3d-vertex-array-push-extend (vertex-array x y z u v color)
  (declare (type textured-3d-vertex-array vertex-array))
  (declare (type real x y z u v))
  (declare (type (unsigned-byte 32) color))
  (let ((vertex-type '(:struct textured-3d-vertex))
        (vertex-type-size (load-time-value (foreign-type-size '(:struct textured-3d-vertex)))))
    (with-slots (ptr fill-pointer allocated-count) vertex-array
      (unless (< fill-pointer allocated-count)
        (let* ((new-count (* 2 allocated-count))
               (new-array (foreign-alloc vertex-type :count new-count))
               (old-array ptr))
          (memcpy new-array old-array (* fill-pointer vertex-type-size))
          (setf ptr new-array)
          (setf allocated-count new-count)
          (foreign-free old-array)))
      (let ((ptr (mem-aptr ptr vertex-type fill-pointer)))
        (setf (foreign-slot-value ptr vertex-type 'x) (clampf x)
              (foreign-slot-value ptr vertex-type 'y) (clampf y)
              (foreign-slot-value ptr vertex-type 'z) (clampf z)
              (foreign-slot-value ptr vertex-type 'u) (clampf u)
              (foreign-slot-value ptr vertex-type 'v) (clampf v)
              (foreign-slot-value ptr vertex-type 'col) color)
        (prog1 fill-pointer
          (incf fill-pointer))))))

(defun standard-3d-vertex-array-push-extend (vertex-array x y z color)
  (declare (type textured-3d-vertex-array vertex-array))
  (declare (type real x y z))
  (declare (type (unsigned-byte 32) color))
  (textured-3d-vertex-array-push-extend vertex-array x y z +tex-white-pixel-u+ +tex-white-pixel-v+ color))

(defcstruct textured-3d-vertex-with-normal
  (x :float)
  (y :float)
  (z :float)
  (u :float)
  (v :float)
  (col :unsigned-int)
  (nx :float)
  (ny :float)
  (nz :float))

(defstruct (textured-3d-vertex-with-normal-array
            (:include vertex-array)
            (:constructor make-textured-3d-vertex-with-normal-array
                (&optional
                   (allocated-count +draw-list-alloc-size+)
                   (foreign-type '(:struct textured-3d-vertex-with-normal))
                   (fill-pointer 0)
                   (ptr (foreign-alloc foreign-type :count allocated-count))
                   (foreign-type-size (foreign-type-size foreign-type))))))

(defun textured-3d-vertex-with-normal-array-push-extend (vertex-array x y z nx ny nz u v color)
  (declare (type textured-3d-vertex-with-normal-array vertex-array))
  (declare (type real x y z nx ny nz u v))
  (declare (type (unsigned-byte 32) color))
  (let ((vertex-type '(:struct textured-3d-vertex-with-normal))
        (vertex-type-size (load-time-value (foreign-type-size '(:struct textured-3d-vertex-with-normal)))))
    (with-slots (ptr fill-pointer allocated-count) vertex-array
      (unless (< fill-pointer allocated-count)
        (let* ((new-count (* 2 allocated-count))
               (new-array (foreign-alloc vertex-type :count new-count))
               (old-array ptr))
          (memcpy new-array old-array (* fill-pointer vertex-type-size))
          (setf ptr new-array)
          (setf allocated-count new-count)
          (foreign-free old-array)))
      (let ((ptr (mem-aptr ptr vertex-type fill-pointer)))
        (setf (foreign-slot-value ptr vertex-type 'x) (clampf x)
              (foreign-slot-value ptr vertex-type 'y) (clampf y)
              (foreign-slot-value ptr vertex-type 'z) (clampf z)
              (foreign-slot-value ptr vertex-type 'u) (clampf u)
              (foreign-slot-value ptr vertex-type 'v) (clampf v)
              (foreign-slot-value ptr vertex-type 'nx) (clampf nx)
              (foreign-slot-value ptr vertex-type 'ny) (clampf ny)
              (foreign-slot-value ptr vertex-type 'nz) (clampf nz)
              (foreign-slot-value ptr vertex-type 'col) color)
        (incf fill-pointer)))))

(defun standard-3d-vertex-with-normal-array-push-extend (vertex-array x y z nx ny nz color)
  (textured-3d-vertex-with-normal-array-push-extend vertex-array x y z nx ny nz +tex-white-pixel-u+ +tex-white-pixel-v+ color))

;; todo: implement upgrade-index-array to migrate unsigned-short index-arrays to unsigned-long.
