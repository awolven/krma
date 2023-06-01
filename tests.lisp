(in-package :krma)

(defun test ()
  (let* ((scene (application-scene *app*))
	 (font (application-default-font *app*))
         (data (font-data font))
         (w (3b-bmfont-common:scale-w data))
         (h (3b-bmfont-common:scale-h data)))
    (rm-dispatch-to-render-thread-with-handle (scene draw-data handle)
					      (%draw-data-add-text-quad-list-primitive draw-data handle 0 :foo
										       nil font #x000000ff 0.0f0
										       (list 0 0 0 0 w h 1 1)))))

(defvar *handles*)

(defun 2d-point-test-1 ()
  (push (add-text-primitive "2d point test #1: Do you see a red 4px square point at (100, 125)? (y/n)" 100 75) *handles*)
  (push (add-2d-point-primitive 100 125 :color #xff0000ff) *handles*)
  (y-or-n-p))

(defun 2d-point-test-2 ()
  (add-text "2d point test #2: Do you see a red 4px square point at (100, 125)? (y/n)" 100 75 :group :foo)
  (add-2d-point 100 125 :color #xff0000ff :group :foo)
  (y-or-n-p))

(defun 2d-line-test-1 ()
  (push (add-text-primitive "2d line test #1: Do you see a red horizontal 2px thick line segment below this text? (y/n)" 100 175)
        *handles*)
  (push (add-2d-line-primitive 100 225 300 225 :color #xff0000ff) *handles*)
  (y-or-n-p))

(defun 2d-line-test-3 ()
  (add-text "2d line test #3: Do you see a red horizontal 2px thick line segment below this text? (y/n)" 100 175 :group :foo)
  (add-2d-line 100 225 300 225 :color #xff0000ff :group :foo)
  (y-or-n-p))

(defun 2d-line-test-2 ()
  (push (add-text-primitive "2d line test #2: Do you see a blue horizontal 5px thick line segment below this text? (y/n" 100 275)
        *handles*)
  (push (add-2d-line-primitive 100 325 300 325 :color #x0000ffff :line-thickness 5) *handles*)
  (y-or-n-p))

(defun 2d-line-test-4 ()
  (add-text "2d line test #4: Do you see a blue horizontal 5px thick line segment below this text? (y/n)" 100 275 :group :foo)
  (add-2d-line 100 325 300 325 :color #x0000ffff :line-thickness 5 :group :foo)
  (y-or-n-p))

(defun 3d-point-test-1 ()
  (push (add-text-primitive "3d point test #1: Do you see a red 4px square point at (100, 425)? (y/n)" 100 375) *handles*)
  (push (add-3d-point-primitive 100 425 0 :color #xff0000ff) *handles*)
  (y-or-n-p))

(defun 3d-point-test-2 ()
  (add-text "3d point test #2: Do you see a red 14px square point at (100, 425)? (y/n)" 100 375 :group :foo)
  (add-3d-point 100 425 0 :color #xff0000ff :point-size 14 :group :bar)
  (y-or-n-p))

(defun 3d-line-test-1 ()
  (push (add-text-primitive "3d line test #1: Do you see a blue horizontal 2px thick line segment below this text? (y/n)"
			    100 475) *handles*)
  (push (add-3d-line-primitive 100 525 0 300 525 0 :color #x0000ffff) *handles*)
  (y-or-n-p))

(defun 3d-line-test-2 ()
  (add-text "3d line test #2: Do you see a blue horizontal 2px thick line segment below this text? (y/n)"
            100 475 :group :foo)
  (add-3d-line 100 525 0 300 525 0 :color #x0000ffff :group :bar)
  (y-or-n-p))

(defun multicolor-2d-polyline-test-1 ()
  (push (add-text-primitive "Multicolor 2d polyline test #1:" 100 575) *handles*)
  (push (add-text-primitive "Do you see a closed multicolor polyline below this text? (y/n)" 100 625) *handles*)
  (push (add-multicolor-2d-polyline-primitive (list 100 675 #xff0000ff
                                                    100 875 #xffff00ff
                                                    500 875 #x00ff00ff
                                                    500 675 #x00ffffff)
                                              :closed? t) *handles*)
  (y-or-n-p))

(defun multicolor-2d-polyline-test-3 ()
  (add-text "Multicolor 2d polyline test #3:" 100 575 :group :foo)
  (add-text "Do you see a closed multicolor 5px thick polyline below this text? (y/n)" 100 625 :group :foo)
  (add-multicolor-2d-polyline (list 100 675 #xff0000ff
                                    100 875 #xffff00ff
                                    500 875 #x00ff00ff
                                    500 675 #x00ffffff)
                              :closed? t
                              :line-thickness 5
                              :group :foo)
  (y-or-n-p))

(defun multicolor-2d-polyline-test-2 ()
  (push (add-text-primitive "Multicolor 2d polyline test #1:" 100 575) *handles*)
  (push (add-text-primitive "Do you see an open multicolor polyline below this text? (y/n)" 100 890) *handles*)
  (push (add-multicolor-2d-polyline-primitive (list 100 935 #xff0000ff
                                                    100 1135 #xffff00ff
                                                    500 1135 #x00ff00ff
                                                    500 935 #x00ffffff)) *handles*)
  (y-or-n-p))

(defun multicolor-2d-polyline-test-4 ()
  (add-text "Multicolor 2d polyline test #4:" 100 575 :group :foo)
  (add-text "Do you see an open multicolor 10px thick polyline below this text? (y/n)" 100 890 :group :foo)
  (add-multicolor-2d-polyline (list 100 935 #xff0000ff
                                    100 1135 #xffff00ff
                                    500 1135 #x00ff00ff
                                    500 935 #x00ffffff)
                              :line-thickness 10 :group :foo)
  (y-or-n-p))

(defun filled-circle-test-1 ()
  (push (add-text-primitive "Do you see a filled blue circle?" 100 1200) *handles*)
  (push (add-filled-2d-circle-primitive 120 1270 20 :color #x0000ffff) *handles*)
  (y-or-n-p))

(defun filled-circle-test-2 ()
  (add-text "Do you see a filled blue circle?" 100 1200 :group :foo)
  (add-filled-2d-circle 120 1270 20 :color #x0000ffff :group :foo)
  (y-or-n-p))

(defun sphere-test-1 ()
  (push (add-text-primitive "do you see a white sphere in the middle of the screen?" 100 1320) *handles*)
  (push (add-filled-sphere-primitive 0 0 0 100) *handles*)
  (y-or-n-p))

(defun sphere-test-2 ()
  (add-text "do you see a white sphere in the middle of the screen?" 100 1320 :group :foo)
  (add-filled-sphere 0 0 0 100 :group :bar)
  (y-or-n-p))


(defun test2 ()
  (let ((*handles* ()))
    (declare (special *handles*))
    (prog1 (and (2d-point-test-1)
                (2d-line-test-1)
                (2d-line-test-2)
                (3d-point-test-1)
                (3d-line-test-1)
                (multicolor-2d-polyline-test-1)
                (multicolor-2d-polyline-test-2)
                (filled-circle-test-1)
                (sphere-test-1))
      (delete-primitives (application-scene *app*) *handles*))))

(defun test3 ()
  (prog1 (and (2d-point-test-2)
              (2d-line-test-3)
              (2d-line-test-4)
              (3d-point-test-2)
              (3d-line-test-2)
              (multicolor-2d-polyline-test-3)
              (multicolor-2d-polyline-test-4)
              (filled-circle-test-2)
              (sphere-test-2))
    (delete-groups (application-scene *app*) (list :foo :bar))))

(defun test-im ()
  (let ((scene (application-scene *app*)))
    (setf (immediate-mode-work-function-3 (clui:default-display))
          #'(lambda ()
              (draw-2d-point 1000 1000 :color #xffff00ff :group :foo)
              (scene-draw-3d-point scene :bar 5 #x00ff00ff 900 900 -20)
              (scene-draw-2d-line scene :foo 2 #x0000ffff 0 0 100 100)
              (scene-draw-3d-line scene :bar 2 #xffff00ff 100 100 -100 200 200 -200)
              (scene-draw-multicolor-2d-polyline scene :foo t 2.0f0
						 (list 100 935 #xff0000ff
                                                       100 1135 #xffff00ff
                                                       500 1135 #x00ff00ff
                                                       500 935 #x00ffffff))
              (scene-draw-2d-triangle scene :foo 2 #xff0000ff 300 300 300 400 400 400)
              (scene-draw-2d-rectangle scene :foo 2 #x00ff00ff 300 500 400 600)
              (scene-draw-2d-circular-arc scene :foo nil 2 #x0000ffff 160 1270 20 0 pi 32)
              (scene-draw-2d-circle scene :foo 2 #x00ffffff 210 1270 20 32)
	      (scene-draw-text scene :foo (application-default-font *app*)
			       #xff0000ff 1000 100 "hullabaloo2")))))
