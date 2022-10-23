(in-package :krma)

(defun test ()
  (let* ((scene (application-scene *app*))
         (data (font-data *font*))
         (texture (font-atlas *font*))
         (w (3b-bmfont-common:scale-w data))
         (h (3b-bmfont-common:scale-h data)))
    (rm-dispatch-to-render-thread (scene draw-data handle)
      (%draw-data-add-text-quad-list-cmd draw-data handle
                                         *identity-matrix*
                                         texture #xffffffff
                                         (list 0 0 0 0 w h 1 1)
                                         *font*))))

(defun 2d-point-test-1 ()
  (add-text "2d point test #1: Do you see a red 4px square point at (100, 125)? (y/n)" 100 75)
  (add-2d-point 100 125 :color #xff0000ff)
  (y-or-n-p))

(defun 2d-line-test-1 ()
  (add-text "2d line test #1: Do you see a red horizontal 2px thick line segment below this text? (y/n)" 100 175)
  (add-2d-line 100 225 300 225 :color #xff0000ff)
  (y-or-n-p))

#+NOTYET
(defun 2d-line-test-2 ()
  (add-text "2d line test #2: Do you see a blue horizontal 5px thick line segment below this text? (y/n" 100 275)
  (add-2d-line 100 325 300 325 :color #x0000ffff :line-width 5)
  (y-or-n-p))

(defun 3d-point-test-1 ()
  (add-text "3d point test #1: Do you see a red 4px square point at (100, 425)? (y/n)" 100 375)
  (add-3d-point 100 425 0 :color #xff0000ff)
  (y-or-n-p))

(defun 3d-line-test-1 ()
  (add-text "3d line test #1: Do you see a blue horizontal 2px thick line segment below this text? (y/n)"
            100 475)
  (add-3d-line 100 525 0 300 525 0 :color #x0000ffff)
  (y-or-n-p))

(defun multicolor-2d-polyline-test-1 ()
  (add-text
   "Multicolor 2d polyline test #1:" 100 575)
  (add-text
   "Do you see a closed multicolor polyline below this text? (y/n)" 100 625)
  (add-multicolor-2d-polyline (list 100 675 #xff0000ff
                                    100 875 #xffff00ff
                                    500 875 #x00ff00ff
                                    500 675 #x00ffffff)
                              :closed? t)
  (y-or-n-p))

(defun multicolor-2d-polyline-test-2 ()
  (add-text
   "Multicolor 2d polyline test #1:" 100 575)
  (add-text
   "Do you see an open multicolor polyline below this text? (y/n)" 100 890)
  (add-multicolor-2d-polyline (list 100 935 #xff0000ff
                                    100 1135 #xffff00ff
                                    500 1135 #x00ff00ff
                                    500 935 #x00ffffff))
  (y-or-n-p))

(defun filled-circle-test-1 ()
  (add-text "Do you see a filled blue circle?" 100 1200)
  (add-filled-2d-circle 120 1270 20 :color #x0000ffff)
  (y-or-n-p))


(defun test2 ()
  (and (2d-point-test-1)
       (2d-line-test-1)
       (3d-point-test-1)
       (3d-line-test-1)
       (multicolor-2d-polyline-test-1)
       (multicolor-2d-polyline-test-2)
       (filled-circle-test-1)))

(defun test-im ()
  (let ((scene (application-scene *app*)))
    (setf (immediate-mode-work-function-1 *app*)
          #'(lambda ()
              (draw-2d-point 1000 1000 :color #xffff00ff)
              (scene-draw-3d-point scene #x00ff00ff 900 900 -20)
              (scene-draw-2d-line scene #x0000ffff 0 0 100 100)
              (scene-draw-3d-line scene #xffff00ff 100 100 -100 200 200 -200)
              (scene-draw-multicolor-2d-polyline-1 scene t 2.0f0
                                                   (list 100 935 #xff0000ff
                                                         100 1135 #xffff00ff
                                                         500 1135 #x00ff00ff
                                                         500 935 #x00ffffff))
              (scene-draw-2d-triangle scene #xff0000ff 300 300 300 400 400 400)
              (scene-draw-2d-rectangle scene #x00ff00ff 300 500 400 600)
              (scene-draw-2d-circular-arc scene nil #x0000ffff 160 1270 20 0 pi)
              (scene-draw-2d-circle scene #x00ffffff 210 1270 20)))))
