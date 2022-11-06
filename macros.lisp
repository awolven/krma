(in-package :krma)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-concurrency))

(defun gen-rm-handle ()
  "generate retained-mode primitive handle"
  (sb-ext:atomic-incf (car (retained-mode-handle-count-cons *app*)))
  (car (retained-mode-handle-count-cons *app*)))

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
               (sb-concurrency:enqueue
                #'(lambda () ,@body)
                (draw-data-work-queue ,dd0-sym)))
             (let ((,draw-data-var ,dd1-sym))
               (sb-concurrency:enqueue
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
           (sb-concurrency:enqueue
            #'(lambda () ,@body)
            (draw-data-work-queue ,dd0-sym)))
         (let ((,draw-data-var ,dd1-sym))
           (sb-concurrency:enqueue
            #'(lambda () ,@body)
            (draw-data-work-queue ,dd1-sym)))))))
