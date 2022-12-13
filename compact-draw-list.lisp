(in-package :krma)

(eval-when (:compile-toplevel :load-toplevel)
  (when krma::*debug*
    (declaim (optimize (safety 3) (debug 3)))))

(defun compact-draw-list (draw-list &optional (cmd-constructor #'make-standard-draw-indexed-cmd))
  (block nil
    (if (not (draw-list-needs-compaction? draw-list))

        (return draw-list)

        (let* ((cmd-vector (draw-list-cmd-vector draw-list))
               (new-draw-list (make-instance (class-of draw-list)))
               (new-index-array (draw-list-index-array new-draw-list))
               (new-vertex-array (draw-list-vertex-array new-draw-list))
               (old-index-array (draw-list-index-array draw-list))
               (old-vertex-array (draw-list-vertex-array draw-list))
               (fp-old-vertex-array (foreign-array-fill-pointer old-vertex-array))
               (vertex-type-size (foreign-array-foreign-type-size old-vertex-array))
               (index-type (foreign-array-foreign-type old-index-array)))

          (%prim-reserve new-draw-list
                         fp-old-vertex-array
                         (foreign-array-fill-pointer old-index-array)
                         (foreign-array-foreign-type-size old-vertex-array)
                         (foreign-array-foreign-type-size old-index-array))


          (cond ((zerop (fill-pointer cmd-vector))
                 ;; case #1: (for point list and line list)
                 ;; it does not have cmds (and never did) (the drawindexed parameters are stored elsewhere)
                 ;; memcpy segments and recreate index-array

                 (let ((i 0)
                       (prev-index nil)
                       (this-index nil)
                       (count 0)
                       (old-array-segment-start-offset nil)
                       (new-array-segment-start-offset nil))

                   (tagbody
                    again
                      (when (eq i fp-old-vertex-array)
                        (go exit))

                      ;; these index arrays are in possibly non-consecutive increasing order
                      ;; and we put them in consecutive increasing order
                      (setq this-index (mem-aref old-index-array index-type i))

                      (unless new-array-segment-start-offset
                        (setq new-array-segment-start-offset i))
                      (unless old-array-segment-start-offset
                        (setq old-array-segment-start-offset this-index))

                      (when prev-index
                        (incf count)
                        (unless (eq prev-index (1- this-index))
                          ;; discontinuity, copy the segment
                          (memcpy (inc-pointer (foreign-array-ptr new-vertex-array)
                                               (* new-array-segment-start-offset vertex-type-size))
                                  (inc-pointer (foreign-array-ptr old-vertex-array)
                                               (* old-array-segment-start-offset vertex-type-size))
                                  (* count vertex-type-size))

                          (incf new-array-segment-start-offset count)
                          (setq old-array-segment-start-offset this-index)

                          (setq count 0)
                          (setq prev-index nil)
                          (go again)))

                      ;; no discontinuity
                      (setq prev-index this-index) ;; same as (incf prev-index).
                      (index-array-push-extend new-index-array i)
                      (incf i)
                      (go again)

                    exit
                      (foreign-free (foreign-array-ptr old-index-array))
                      (foreign-free (foreign-array-ptr old-vertex-array))
                      (return new-draw-list))))



                (t ;; case #2: usually for line_strip, triangle_strip, triangle_list or any draw-list with cmds
                 (loop for cmd across cmd-vector
                       with new-vtx-offset = -1
                       with translation-table-table = (make-hash-table :test #'eql)
                       with translation-table = nil
                       when cmd
                         do ;; cmd-first-index clues us in to whether this is a reinstanced cmd
                            ;; if so use the translation table which exists from previous instance
                            (setq translation-table (gethash (cmd-first-idx cmd) translation-table-table))
                            (unless translation-table
                              (setf (gethash (cmd-first-idx cmd) translation-table-table)
                                    (setq translation-table (make-hash-table :test #'eql))))
                            (loop for i from (cmd-first-idx cmd)
                                  repeat (cmd-elem-count cmd)
                                  with cmd-vtx-offset = (cmd-vtx-offset cmd)
                                  with translation = nil
                                  with orig-idx
                                  do (setq orig-idx (mem-aref (foreign-array-ptr old-index-array) index-type i))
                                  when (setq translation (gethash orig-idx translation-table))
                                    do ;; if there is a translation of index, add it to index-array at fill-pointer
                                       (index-array-push-extend new-index-array translation)
                                  unless translation
                                    do ;; copy one vertex into new vertex array at index (1+ new-vtx-offset)
                                       ;; no vertex should be copied twice, but some may not be copied at all
                                       ;; it doesn't matter what the vertex-array fill-pointer is. we'll adjust
                                       ;; that at the end
                                       (memcpy (inc-pointer (foreign-array-ptr new-vertex-array)
                                                            (* (incf new-vtx-offset) vertex-type-size))
                                               (inc-pointer (foreign-array-ptr old-vertex-array)
                                                            (* (+ cmd-vtx-offset orig-idx) vertex-type-size))
                                               (* 1 vertex-type-size))
                                       ;; put new index into new index array at fill-pointer
                                       (index-array-push-extend new-index-array new-vtx-offset)
                                       ;; record the translation
                                       (setf (gethash orig-idx translation-table) new-vtx-offset)
                                  finally ;; copy the cmd, except update the first-index and vtx-offset
                                          (vector-push-extend (funcall cmd-constructor
                                                                       new-draw-list
                                                                       (foreign-array-fill-pointer new-index-array)
                                                                       (cmd-elem-count cmd)
                                                                       ;; the vertex array fp hasn't been updated yet
                                                                       ;; and we want to use it as is anyway
                                                                       ;; we'll update it below for use in the next cmd
                                                                       (foreign-array-fill-pointer new-vertex-array)
                                                                       (cmd-model-mtx cmd)
                                                                       (cmd-color-override cmd)
                                                                       (cmd-texture cmd)
                                                                       (cmd-line-thickness cmd)
                                                                       (cmd-light-position cmd))
                                                              (draw-list-cmd-vector new-draw-list))
                                          ;; finally update the vertex array fp so that vertex-pushes work in the future
                                          (setf (foreign-array-fill-pointer new-vertex-array) (1+ new-vtx-offset)))
                       finally
                          (foreign-free (foreign-array-ptr old-index-array))
                          (foreign-free (foreign-array-ptr old-vertex-array))
                          (return new-draw-list))))))))
