(in-package :krma)

(eval-when (:compile-toplevel :load-toplevel)
  (when krma::*debug*
    (declaim (optimize (safety 3) (debug 3)))))

#+SBCL
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-concurrency))

(defmacro krma ()
  `*krma*)

(defmacro maybe-defer-debug ((app) &body body)
  "Sets up `ignore' restart, and records error message and backtrace in application object."
  (let ((app-sym (gensym)))
    `(let ((,app-sym ,app))
       (restart-bind ((ignore (lambda (&optional c)
                                (declare (ignorable c))
                                (throw :ignore nil))))
         (catch :ignore
           (handler-bind ((error (lambda (c)
                                   (record-error-msg ,app-sym c)
                                   (record-backtrace ,app-sym))))
             ,@body))))))

(defmacro rm-dispatch-to-render-thread-with-handle ((resource draw-data-var handle-var) &body body)
  (let ((dd0-sym (gensym))
        (dd1-sym (gensym))
        (ddvec-sym (gensym)))
    `(let* ((,ddvec-sym (rm-draw-data ,resource)))
       (declare (type vector ,ddvec-sym))
       (let ((,dd0-sym (svref ,ddvec-sym 0))
             (,dd1-sym (svref ,ddvec-sym 1)))
         (declare (type retained-mode-draw-data ,dd0-sym ,dd1-sym))
         (let ((,handle-var (gen-rm-handle)))
           (let ((,draw-data-var ,dd0-sym))
             (#-sbcl lparallel.queue:push-queue
	      #+SBCL sb-concurrency:enqueue
              #'(lambda () ,@body)
              (draw-data-work-queue ,dd0-sym)))
           (let ((,draw-data-var ,dd1-sym))
             (#+SBCL sb-concurrency:enqueue
	      #-sbcl lparallel.queue:push-queue
              #'(lambda () ,@body)
              (draw-data-work-queue ,dd1-sym)))
           ,handle-var)))))

(defmacro rm-dispatch-to-render-thread ((resource draw-data-var) &body body)
  (let ((dd0-sym (gensym))
        (dd1-sym (gensym))
        (ddvec-sym (gensym)))
    `(let* ((,ddvec-sym (rm-draw-data ,resource)))
       (declare (type vector ,ddvec-sym))
       (let ((,dd0-sym (svref ,ddvec-sym 0))
             (,dd1-sym (svref ,ddvec-sym 1)))
         (declare (type retained-mode-draw-data ,dd0-sym ,dd1-sym))
         (let ((,draw-data-var ,dd0-sym))
           (#+SBCL sb-concurrency:enqueue
	    #-sbcl lparallel.queue:push-queue
            #'(lambda () ,@body)
            (draw-data-work-queue ,dd0-sym)))
         (let ((,draw-data-var ,dd1-sym))
           (#+SBCL sb-concurrency:enqueue
	    #-sbcl lparallel.queue:push-queue
            #'(lambda () ,@body)
            (draw-data-work-queue ,dd1-sym)))
	 (values)))))

(defmacro with-graphics-queue-and-command-buffer ((dpy queue-var command-buffer-var) &body body)
  (let ((helper-window-sym (gensym))
	(device-sym (gensym))
	(index-sym (gensym))
	(command-pool-sym (gensym))
	(dpy-sym (gensym)))
    `(let* ((,dpy-sym ,dpy)
	    (,helper-window-sym (clui::helper-window ,dpy-sym))
	    (,device-sym (default-logical-device ,dpy-sym))
	    (,index-sym (queue-family-index (render-surface ,helper-window-sym)))
	    (,queue-var (find-queue ,device-sym ,index-sym))
	    (,command-pool-sym (find-command-pool ,device-sym ,index-sym))
	    (,command-buffer-var (elt (command-buffers ,command-pool-sym) 0)))

       ,@body)))
