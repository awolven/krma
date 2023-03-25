(in-package :krma)

(defmethod global-model-matrix ((window clui::window-mixin))
  (meye 4))

(defmethod transform-coordinate ((vec vec2) (window clui::window-mixin))
  (multiple-value-bind (x y) (clui:window-position window)
    (vec3 (+ x (vx vec)) (+ y (vy vec)) 0)))

(defmethod transform-coordinate ((vec vec3) (window clui::window-mixin))
  (multiple-value-bind (x y) (clui:window-position window)
    (vec3 (+ x (vx vec)) (+ y (vy vec)) (vz vec))))

(defmethod untransform-coordinate ((vec vec2) (window clui::window-mixin))
  (multiple-value-bind (x y) (clui:window-position window)
    (vec3 (- (vx vec) x) (- (vy vec) y) 0)))

(defmethod untransform-coordinate ((vec vec3) (window clui::window-mixin))
  (multiple-value-bind (x y) (clui:window-position window)
    (vec3 (- (vx vec) x) (- (vy vec) y) (vz vec))))

(defun font-size (font)
  (3b-bmfont-common:size (slot-value font 'krma::data)))

(defun calc-text-size (string font)
  (let* ((data (font-data font))
	 (glyph-table (slot-value data '3b-bmfont-common::chars)))
    
    (loop for i from 0 below (length string)
	  with glyph = nil
	  with width
	  with height
	  with dx = 0.0f0
	  with x0
	  with y0
	  with x1
	  with y1
	  with min-x = 0.0f0
	  with max-x = 0.0f0
	  with min-y = 0.0f0
	  with max-y = 0.0f0
	  do (setq glyph (gethash (char string i) glyph-table))
	     (setq width (float (3b-bmfont:glyph-width glyph) 1.0f0))
	     (setq height (float (3b-bmfont:glyph-height glyph) 1.0f0))
	     (setq x0 (+ dx (3b-bmfont:glyph-xoffset glyph)))
	     (setq y0 (3b-bmfont:glyph-yoffset glyph))
	     (setq x1 (+ x0 width))
	     (setq y1 (+ y0 height))
	   
	     (setq min-x (min x0 min-x))
	     (setq max-x (max x1 max-x))
	     (setq min-y (min y0 min-y))
	     (setq max-y (max y1 max-y))

	     (incf dx (float (3b-bmfont:glyph-xadvance glyph) 1.0f0))
	  finally (return (values min-x min-y max-x max-y)))))





(defparameter *style-classic-plist*
  (list :text                     #(0.90f0 0.90f0 0.90f0 1.00f0)
	:text-disabled            #(0.60f0 0.60f0 0.60f0 1.00f0)
	:window-bg                #(0.00f0 0.00f0 0.00f0 0.70f0)
	:child-bg                 #(0.00f0 0.00f0 0.00f0 0.00f0)
	:popup-bg                 #(0.11f0 0.11f0 0.14f0 0.92f0)
	:border                   #(0.50f0 0.50f0 0.50f0 0.50f0)
	:border-shadow            #(0.00f0 0.00f0 0.00f0 0.00f0)
	:frame-bg                 #(0.43f0 0.43f0 0.43f0 0.39f0)
	:frame-bg-hovered         #(0.47f0 0.47f0 0.69f0 0.40f0)
	:frame-bg-active          #(0.42f0 0.41f0 0.64f0 0.69f0)
	:title-bg                 #(0.27f0 0.27f0 0.54f0 0.83f0)
	:title-bg-active          #(0.32f0 0.32f0 0.63f0 0.87f0)
	:title-bg-collapsed       #(0.40f0 0.40f0 0.80f0 0.20f0)
	:menu-bar-bg              #(0.40f0 0.40f0 0.55f0 0.80f0)
	:scrollbar-bg             #(0.20f0 0.25f0 0.30f0 0.60f0)
	:scrollbar-grab           #(0.40f0 0.40f0 0.80f0 0.30f0)
	:scrollbar-grab-hovered   #(0.40f0 0.40f0 0.80f0 0.40f0)
	:scrollbar-grab-active    #(0.41f0 0.39f0 0.80f0 0.60f0)
	:checkmark                #(0.90f0 0.90f0 0.90f0 0.50f0)
	:slider-grab              #(1.00f0 1.00f0 1.00f0 0.30f0)
	:slider-grab-active       #(0.41f0 0.39f0 0.80f0 0.60f0)
	:button                   #(0.35f0 0.40f0 0.61f0 0.62f0)
	:button-hovered           #(0.40f0 0.48f0 0.71f0 0.79f0)
	:button-active            #(0.46f0 0.54f0 0.80f0 1.00f0)
	:header                   #(0.40f0 0.40f0 0.90f0 0.45f0)
	:header-hovered           #(0.45f0 0.45f0 0.90f0 0.80f0)
	:header-active            #(0.53f0 0.53f0 0.87f0 0.80f0)
	:separator                #(0.50f0 0.50f0 0.50f0 0.60f0)
	:separator-hovered        #(0.60f0 0.60f0 0.70f0 1.00f0)
	:separator-active         #(0.70f0 0.70f0 0.90f0 1.00f0)
	:resize-grip              #(1.00f0 1.00f0 1.00f0 0.16f0)
	:resize-grip-hovered      #(0.78f0 0.82f0 1.00f0 0.60f0)
	:resize-grip-active       #(0.78f0 0.82f0 1.00f0 0.90f0)

	:docking-empty-bg         #(0.20f0 0.20f0 0.20f0 1.00f0)
	:plot-lines               #(1.00f0 1.00f0 1.00f0 1.00f0)
	:plot-lines-hovered       #(0.90f0 0.70f0 0.00f0 1.00f0)
	:plot-histogram           #(0.90f0 0.70f0 0.00f0 1.00f0)
	:plot-histogram-hovered   #(1.00f0 0.60f0 0.00f0 1.00f0)
	:text-selected-bg         #(0.00f0 0.00f0 1.00f0 0.35f0)
	:drag-drop-target         #(1.00f0 1.00f0 0.00f0 0.90f0)

	:nav-windowing-highlight  #(1.00f0 1.00f0 1.00f0 0.70f0)
	:nav-windowing-dim-bg     #(0.80f0 0.80f0 0.80f0 0.20f0)
	:modal-window-dim-bg      #(0.20f0 0.20f0 0.20f0 0.35f0)

	:frame-border-size 1
	:frame-padding-x 1
	:frame-padding-y 1
	:item-inner-spacing-x 4
	:window-title-align-x 1
	:window-menu-button-position :right
	:title-bar-height 20
	:default-window-border-size 1))

(defclass gui-style ()
  ((text :accessor text-color)
   (text-disabled :accessor text-disabled-color)
   (window-bg :accessor window-bg-color)
   (child-bg :accessor child-bg-color)
   (popup-bg :accessor popup-bg-color)
   (border :accessor border-color)
   (border-shadow :accessor border-shadow-color)
   (frame-bg :accessor frame-bg-color)
   (frame-bg-hovered :accessor frame-bg-hovered-color)
   (frame-bg-active :accessor frame-bg-active-color)
   (title-bg :accessor title-bg-color)
   (title-bg-active :accessor title-bg-active-color)
   (title-bg-collapsed :accessor title-bg-collapsed-color)
   (menu-bar-bg :accessor menu-bar-bg-color)
   (scrollbar-bg :accessor scrollbar-bg-color)
   (scrollbar-grab :accessor scrollbar-grab-color)
   (scrollbar-grab-hovered :accessor scrollbar-grab-hovered-color)
   (scrollbar-grab-active :accessor scrollbar-grab-active-color)
   (checkmark :accessor checkmark-color)
   (slider-grab :accessor slider-grab-color)
   (slider-grab-active :accessor slider-grab-active-color)
   (button :accessor button-color)
   (button-hovered :accessor button-hovered-color)
   (button-active :accessor button-active-color)
   (header :accessor header-color)
   (header-hovered :accessor header-hovered-color)
   (header-active :accessor header-active-color)
   (separator :accessor separator-color)
   (separator-hovered :accessor separator-hovered-color)
   (separator-active :accessor separator-active-color)
   (resize-grip :accessor resize-grip-color)
   (resize-grip-hovered :accessor resize-grip-hovered-color)
   (resize-grip-active :accessor resize-grip-active-color)
   (tab :accessor tab-color)
   (tab-hovered :accessor tab-hovered-color)
   (tab-active :accessor tab-active-color)
   (tab-unfocused :accessor tab-unfocused-color)
   (tab-unfocused-active :accessor tab-unfocused-active-color)
   (docking-preview :accessor docking-preview-color)
   (docking-empty-bg :accessor docking-empty-bg-color)
   (plot-lines :accessor plot-lines-color)
   (plot-lines-hoverered :accessor plot-lines-hovered-color)
   (plot-histogram :accessor plot-histogram-color)
   (plot-histogram-hovered :accessor plot-histogram-hovered-color)
   (text-selected-bg :accessor text-selected-bg-color)
   (drag-drop-target :accessor drag-drop-target-color)
   (nav-highlight :accessor nav-highlight-color)
   (nav-windowing-highlight :accessor nav-windowing-highlight-color)
   (nav-windowing-dim-bg :accessor nav-windowing-dim-bg-color)
   (modal-window-dim-bg :accessor modal-window-dim-bg-color)
   (frame-border-size :accessor frame-border-size)
   (frame-padding-x :accessor frame-padding-x)
   (frame-padding-y :accessor frame-padding-y)
   (item-inner-spacing-x :accessor item-inner-spacing-x)
   (window-title-align-x :accessor window-title-align-x)
   (window-menu-button-position :accessor window-menu-button-position)
   (title-bar-height :accessor title-bar-height)
   (default-window-border-size :accessor default-window-border-size)

   (current-font :initform nil :accessor current-font)))

(defun lerp (a b s)
  (map 'vector #'+ a (map 'vector #'(lambda (x) (* x s)) (map 'vector #'- a b))))

(defun saturate (float)
  (when (minusp float) (return-from saturate 0.0f0))
  (when (> float 1.0f0) (return-from saturate 1.0f0))
  float)

(defun make-gui-style (style-plist &optional (display (default-display)))
  (let ((style-object (make-instance 'gui-style)))
    (setf (text-color style-object) (getf style-plist :text)
	  (text-disabled-color style-object) (getf style-plist :text-disabled)
	  (window-bg-color style-object) (getf style-plist :window-bg)
	  (child-bg-color style-object) (getf style-plist :child-bg)
	  (popup-bg-color style-object) (getf style-plist :popup-bg)
	  (border-color style-object) (getf style-plist :border)
	  (border-shadow-color style-object) (getf style-plist :border-shadow)
	  (frame-bg-color style-object) (getf style-plist :frame-bg)
	  (frame-bg-hovered-color style-object) (getf style-plist :frame-bg-hovered)
	  (frame-bg-active-color style-object) (getf style-plist :frame-bg-active)
	  (title-bg-color style-object) (getf style-plist :title-bg)
	  (title-bg-active-color style-object) (getf style-plist :title-bg-active)
	  (title-bg-collapsed-color style-object) (getf style-plist :title-bg-collapsed)
	  (menu-bar-bg-color style-object) (getf style-plist :menu-bar-bg)
	  (scrollbar-bg-color style-object) (getf style-plist :scrollbar-bg)
	  (scrollbar-grab-color style-object) (getf style-plist :scrollbar-grab)
	  (scrollbar-grab-hovered-color style-object) (getf style-plist :scrollbar-grab-hovered)
	  (scrollbar-grab-active-color style-object) (getf style-plist :scrollbar-grab-active)
	  (checkmark-color style-object) (getf style-plist :checkmark)
	  (slider-grab-color style-object) (getf style-plist :slider-grab)
	  (slider-grab-active-color style-object) (getf style-plist :slider-grab)
	  (button-color style-object) (getf style-plist :button)
	  (button-hovered-color style-object) (getf style-plist :button-hovered)
	  (header-color style-object) (getf style-plist :header)
	  (header-hovered-color style-object) (getf style-plist :header-hovered)
	  (header-active-color style-object) (getf style-plist :header-active)
	  (separator-color style-object) (getf style-plist :separator)
	  (separator-hovered-color style-object) (getf style-plist :separator-hovered)
	  (resize-grip-color style-object) (getf style-plist :resize-grip)
	  (resize-grip-hovered-color style-object) (getf style-plist :resize-grip-hovered)
	  (resize-grip-active-color style-object) (getf style-plist :resize-grip-active)
	  (tab-color style-object) (lerp (getf style-plist :header) (getf style-plist :title-bg-active) 0.80)
	  (tab-hovered-color style-object) (getf style-plist :header-hovered)
	  (tab-active-color style-object) (lerp (getf style-plist :header-active) (getf style-plist :title-bg-active) 0.60)
	  (tab-unfocused-color style-object) (lerp (getf style-plist :tab) (getf style-plist :title-bg) 0.80)
	  (tab-unfocused-active-color style-object) (lerp (getf style-plist :tab-active) (getf style-plist :title-bg) 0.80)
	  (docking-preview-color style-object) (let ((c (copy-seq (getf style-plist :header))))
					   (setf (elt c 3) (* (elt c 3) 0.7))
					   c)
	  (docking-empty-bg-color style-object) (getf style-plist :docking-empty-bg)
	  (plot-lines-color style-object) (getf style-plist :plot-lines)
	  (plot-lines-hovered-color style-object) (getf style-plist :plot-lines-hovered)
	  (plot-histogram-color style-object) (getf style-plist :plot-histogram)
	  (plot-histogram-hovered-color style-object) (getf style-plist :plot-histogram-hovered)
	  (text-selected-bg-color style-object) (getf style-plist :text-selected-bg)
	  (drag-drop-target-color style-object) (getf style-plist :drag-drop-target)
	  (nav-highlight-color style-object) (getf style-plist :nav-highlight)
	  (nav-windowing-highlight-color style-object) (getf style-plist :nav-windowing-highlight)
	  (nav-windowing-dim-bg-color style-object) (getf style-plist :nav-windowing-dim-bg)
	  (modal-window-dim-bg-color style-object) (getf style-plist :modal-window-dim-bg)
	  
	  (frame-border-size style-object) (getf style-plist :frame-border-size)
	  (frame-padding-x style-object) (getf style-plist :frame-padding-x)
	  (frame-padding-y style-object) (getf style-plist :frame-padding-y)
	  (item-inner-spacing-x style-object) (getf style-plist :item-inner-spacing-x)
	  (window-title-align-x style-object) (getf style-plist :window-title-align-x)
	  (window-menu-button-position style-object) (getf style-plist :window-menu-button-position)
	  (title-bar-height style-object) (getf style-plist :title-bar-height)
	  (default-window-border-size style-object) (getf style-plist :default-window-border-size)

	  (current-font style-object) (default-system-font display))	  
    
    style-object))

(defun make-gui-style-classic (&optional (display (default-display)))
  (make-gui-style *style-classic-plist* display))

(defclass rect-mixin ()
  ((x :initarg :rect-x :initform 0 :accessor rect-x :accessor rect-x0)
   (y :initarg :rect-y :initform 0 :accessor rect-y :accessor rect-y0)
   (width :initarg :width :initform 10 :accessor rect-width)
   (height :initarg :height :initform 10 :accessor rect-height)))

(defmethod rect-x1 ((rect rect-mixin))
  (+ (rect-x0 rect) (rect-width rect)))

(defmethod (setf rect-x1) ((value real) (rect rect-mixin))
  (setf (rect-width rect) (- value (rect-x0 rect)))
  value)

(defmethod rect-y1 ((rect rect-mixin))
  (+ (rect-y0 rect) (rect-height rect)))

(defmethod (setf rect-y1) ((value real) (rect rect-mixin))
  (setf (rect-height rect) (- value (rect-y0 rect)))
  value)

(defclass client-rect (rect-mixin)
  ())

(defclass view-mixin (rect-mixin node-mixin)
  ((layer :initform 0 :initarg :layer :accessor view-layer)))

(defmethod view-p (object)
  (declare (ignore object))
  nil)

(defmethod view-p ((object view-mixin))
  t)

(defclass group-owner-view-mixin (group-owner-mixin view-mixin)
  ())

(defclass collapse-button (selectable-mixin primitive-owner-mixin group-mixin view-mixin)
  ())

(defclass close-button (selectable-mixin primitive-owner-mixin group-mixin view-mixin)
  ())

(defclass title-bar (group-mixin selectable-mixin view-mixin)
  ((collapse-button :initform nil :accessor collapse-button)
   (close-button :initform nil :accessor close-button)))

(defmethod rect-width ((title-bar title-bar))
  (rect-width (node-parent title-bar)))

(defmethod initialize-instance :after ((title-bar title-bar) &rest initargs
				       &key (collapse-button? t)
					 (close-button? t))
  (declare (ignorable initargs))

  (let* ((window (node-parent title-bar))
	 (style (window-style window)))
  
    (setf (rect-x title-bar) (rect-x window)
	  (rect-y title-bar) (rect-y window)
	  (rect-height title-bar) (title-bar-height style)
	  (object-group title-bar) (object-group window)
	  (node-scene title-bar) (node-scene window)
	  (view-layer title-bar) (1+ (view-layer window)))
    )
  
  (when collapse-button?
    (setf (collapse-button title-bar) (make-instance 'collapse-button
						     :parent title-bar
						     :scene (node-scene title-bar)
						     :group (object-group title-bar)
						     :layer (1+ (view-layer title-bar)))))
  (when close-button?
    (setf (close-button title-bar) (make-instance 'close-button
						  :parent title-bar
						  :scene (node-scene title-bar)
						  :group (object-group title-bar)
						  :layer (1+ (view-layer title-bar)))))
  (values))

(defmethod destroy-object ((object title-bar))
  
  (when (close-button object)
    (destroy-object (close-button object))
    (setf (close-button object) nil))
  
  (when (collapse-button object)
    (destroy-object (collapse-button object))
    (setf (collapse-button object) nil))
  
  (remhash (object-id object) *object-id->object-table*)
  
  (values))

(defun add-title-bar (title-bar name &key (style *style*)
				       (unsaved-document? nil)
				       (highlighted? nil))
  (let ((scene (node-scene title-bar))
	(window (node-parent title-bar))
	(pad-left (frame-padding-x style))
	(pad-right (frame-padding-x style))
	(button-size (font-size (current-font style)))
	(close-button-pos-x)
	(close-button-pos-y)
	(collapse-button-pos-x)
	(collapse-button-pos-y)
	(unsaved-document-marker "*")
	(marker-size-x 0.0f0)
	(text-size-x)
	;;(text-size-y)
	(centeredness)
	(pad-extend)
	(layout-r-x0)
	(layout-r-y0)
	;;(layout-r-x1)	
	;;(layout-r-y1)
	(font (current-font style))
	(group (object-group title-bar))
	)

    (if (window-collapsed? window)
	(let ((title-bar-color (if highlighted? (title-bg-active-color style) (title-bg-collapsed-color style))))
	  ;; draw just title bar
	  (scene-add-filled-2d-rectangle-list-primitive
	   scene group nil title-bar-color
	   (list (rect-x0 title-bar) (rect-y0 title-bar)
		 (rect-x1 title-bar) (rect-y1 title-bar))
	   (object-id title-bar)
	   (view-layer title-bar)))
    
	(let ((title-bar-color (if highlighted? (title-bg-active-color style) (title-bg-color style))))
	  (scene-add-filled-2d-rectangle-list-primitive
	   scene group nil title-bar-color
	   (list (rect-x0 title-bar) (rect-y0 title-bar)
		 (rect-x1 title-bar) (rect-y1 title-bar))
	   (object-id title-bar)
	   (view-layer title-bar))))

    (when (close-button title-bar)
      (incf pad-right button-size)
      (setq close-button-pos-x (- (rect-x1 title-bar) pad-right (frame-padding-x style)))
      (setq close-button-pos-y (rect-y0 title-bar)))

    (when (and (collapse-button title-bar) (eq (window-menu-button-position style) :right))
      (incf pad-right button-size)
      (setq collapse-button-pos-x (- (rect-x1 title-bar) (+ 4 pad-right) (frame-padding-x style)))
      (setq collapse-button-pos-y (rect-y0 title-bar)))

    (when (and (collapse-button title-bar) (eq (window-menu-button-position style) :left))
      (setq collapse-button-pos-x (- (+ (rect-x0 title-bar) pad-left) (frame-padding-x style)))
      (setq collapse-button-pos-y (rect-y0 title-bar))
      (incf pad-left button-size))

    (when (collapse-button title-bar)
      (add-collapse-button (collapse-button title-bar) collapse-button-pos-x collapse-button-pos-y
			   :style style :hovered? nil))

    (when (close-button title-bar)
      (add-close-button (close-button title-bar) close-button-pos-x close-button-pos-y
			:style style :hovered? nil))

    (when unsaved-document?
      (multiple-value-bind (min-x min-y max-x max-y) (calc-text-size unsaved-document-marker font)
	(declare (ignore min-y max-y min-x))
	(setq marker-size-x max-x)))

    (multiple-value-bind (min-x min-y max-x max-y) (calc-text-size name font)
      (declare (ignore min-x min-y max-y))
      (setq text-size-x (+ marker-size-x max-x)))

    (when (> pad-left (frame-padding-x style))
      (incf pad-left (item-inner-spacing-x style)))
    
    (when (> pad-right (frame-padding-x style))
      (incf pad-right (item-inner-spacing-x style)))
    
    (when (< 0.0f0 (window-title-align-x style) 1.0f0)
      (setq centeredness (saturate (- 1.0f0 (* 2.0f0 (abs (- (window-title-align-x style) 0.5f0))))))
      (setq pad-extend (min (max pad-left pad-right) (- (rect-width title-bar) pad-left pad-right text-size-x)))
      (setq pad-left (max pad-left (* pad-extend centeredness)))
      (setq pad-right (max pad-right (* pad-extend centeredness))))
    
    (setq layout-r-x0 (+ (rect-x0 title-bar) pad-left))
    (setq layout-r-y0 (rect-y0 title-bar))
    ;;(setq layout-r-x1 (- (rect-x1 title-bar) pad-right))    
    ;;(setq layout-r-y1 (rect-y1 title-bar))

    ;;(setq clip-r-x0 layout-r-x0)
    ;;(setq clip-r-y1 layout-r-y0)
    ;;(setq clip-r-x1 (+ layout-r-x1 (item-inner-spacing-x style)))
    ;;(setq clip-r-y1 layout-r-y1)

    (scene-add-text-primitive
     scene (object-group window) nil (current-font style) (text-color style) layout-r-x0 layout-r-y0
     (if unsaved-document? (concatenate 'string name unsaved-document-marker) name)
     (object-id title-bar)
     (1+ (view-layer title-bar)))))

(defmethod clui:handle-event :around ((window krma-window-mixin) (event clui::pointer-event-mixin))
  (let ((x (clui::pointer-event-x event))
	(y (clui::pointer-event-y event)))
    
    (let ((2d-object (most-specifically-hovered-2d-object
		      (krma-select-box-2d (clui::window-display window)) 0 0)))

      (if (view-p 2d-object)
	  (progn
	    (setf (clui::pointer-event-native-x event) x
		  (clui::pointer-event-native-y event) y)

	    (clui:handle-event 2d-object event))

	  (call-next-method)))))

(defmethod clui:handle-event :around ((window krma-window-mixin) (event clui::input-event-mixin))
  (let ((2d-object (most-specifically-hovered-2d-object
		    (krma-select-box-2d (clui::window-display window)) 0 0)))

    (if (view-p 2d-object)

	(clui:handle-event 2d-object event)
	
	(call-next-method))))

(defmethod clui:handle-event ((title-bar title-bar) (event clui::pointer-motion-event-mixin))
  (let ((last-event (clui::display-last-event
		     (clui::window-display (node-parent (node-parent title-bar))))))
    (when (or (typep last-event 'clui::pointer-button-press-event-mixin)
	      (typep last-event 'clui::pointer-button-hold-and-drag-event-mixin))

      (change-class event 'clui::pointer-button-hold-and-drag-event-mixin)

      (setf (clui::pointer-drag-delta-x event) (- (clui::pointer-event-x event)
						  (clui::pointer-event-x last-event)))

      (setf (clui::pointer-drag-delta-y event) (- (clui::pointer-event-y event)
						  (clui::pointer-event-y last-event)))
      
    (clui:handle-event title-bar event))))

(defmethod clui:handle-event ((title-bar title-bar) (event clui::pointer-button-hold-and-drag-event-mixin))
  (let* ((dx (clui::pointer-drag-delta-x event))
	 (dy (clui::pointer-drag-delta-y event)))
    (multiple-value-bind (x y) (window-position (node-parent title-bar))
      (set-window-position (node-parent title-bar)
			   (+ x dx) (+ y dy)))))

(defmethod window-cursor-position ((view view-mixin))
  (multiple-value-bind (x y) (window-cursor-position (node-parent view))
    (let ((coord (safe-euclid (m* (vec4 x y 0 1) (model-matrix view)))))
      (values (vx coord) (vy coord)))))
      
      

(defmethod clui:handle-event ((button close-button) (event clui::pointer-button-press-event-mixin))
  (destroy-object (node-parent (node-parent button)))
  #+ccl(ccl:gc)
  #+sbcl(gc t))

(defmethod clui:handle-event ((view title-bar) (event clui::pointer-button-press-event-mixin))
  (clui:focus-window (node-parent view))
  (call-next-method))

(defmethod clui:handle-event ((button close-button) (event clui::pointer-enter-event-mixin))
  (delete-object-from-scene button)
  (let ((close-button-pos-x)
	(close-button-pos-y)
	(button-size (font-size (current-font *style*)))
	(pad-right (frame-padding-x *style*)))
    (incf pad-right button-size)
    (setq close-button-pos-x (- (rect-x1 (node-parent button)) pad-right (frame-padding-x *style*)))
    (setq close-button-pos-y (rect-y0 (node-parent button)))
    
    (print close-button-pos-x)
    (print close-button-pos-y)
    (finish-output)
    
    (add-close-button button close-button-pos-x close-button-pos-y :style *style* :hovered? t))
  (call-next-method)
  (values))

(defmethod clui:handle-event ((button close-button) (event clui::pointer-exit-event-mixin))
  (delete-object-from-scene button)
  (let ((close-button-pos-x)
	(close-button-pos-y)
	(button-size (font-size (current-font *style*)))
	(pad-right (frame-padding-x *style*)))
    (incf pad-right button-size)
    (setq close-button-pos-x (- (rect-x1 (node-parent button)) pad-right (frame-padding-x *style*)))
    (setq close-button-pos-y (rect-y0 (node-parent button)))
    (add-close-button button close-button-pos-x close-button-pos-y :style *style* :hovered? nil))
  (call-next-method)
  (values))

(defmethod clui:handle-event ((button collapse-button) (event clui::pointer-enter-event-mixin))
  (delete-object-from-scene button)
  (let ((collapse-button-pos-x)
	(collapse-button-pos-y)
	(button-size (font-size (current-font *style*)))
	(pad-right (+ 4 (frame-padding-x *style*)))
	(pad-left (frame-padding-x *style*)))

    (incf pad-right button-size)
    
    (when (eq (window-menu-button-position *style*) :right)
      (incf pad-right button-size)
      (setq collapse-button-pos-x (- (rect-x1 (node-parent button)) pad-right (frame-padding-x *style*)))
      (setq collapse-button-pos-y (rect-y0 (node-parent button))))

    (when (eq (window-menu-button-position *style*) :left)
      (setq collapse-button-pos-x (- (+ (rect-x0 (node-parent button)) pad-left) (frame-padding-x *style*)))
      (setq collapse-button-pos-y (rect-y0 (node-parent button))))
    
    (add-collapse-button button collapse-button-pos-x collapse-button-pos-y :style *style* :hovered? t))
  (call-next-method)
  (values))

(defmethod clui:handle-event ((button collapse-button) (event clui::pointer-exit-event-mixin))
  (delete-object-from-scene button)
  (let ((collapse-button-pos-x)
	(collapse-button-pos-y)
	(button-size (font-size (current-font *style*)))
	(pad-right (+ 4 (frame-padding-x *style*)))
	(pad-left (frame-padding-x *style*)))

    (incf pad-right button-size)
    
    (when (eq (window-menu-button-position *style*) :right)
      (incf pad-right button-size)
      (setq collapse-button-pos-x (- (rect-x1 (node-parent button)) pad-right (frame-padding-x *style*)))
      (setq collapse-button-pos-y (rect-y0 (node-parent button))))

    (when (eq (window-menu-button-position *style*) :left)
      (setq collapse-button-pos-x (- (+ (rect-x0 (node-parent button)) pad-left) (frame-padding-x *style*)))
      (setq collapse-button-pos-y (rect-y0 (node-parent button))))
    
    (add-collapse-button button collapse-button-pos-x collapse-button-pos-y :style *style* :hovered? nil))
  (call-next-method)
  (values))

(defclass homemade-window-mixin (primitive-owner-mixin selectable-mixin group-owner-view-mixin)
  ((title :initarg :title :initform "krma" :accessor window-title)
   (style :initform nil :writer (setf %window-style) :reader window-style)
   (width :initform 300)
   (height :initform 200)
   (client-rect :initform nil :accessor window-client-rect)
   (iconified? :initform nil :accessor clui::window-iconified?)
   (title-bar :initform nil :accessor window-title-bar)
   (menu-bar :initform nil :accessor window-menu-bar)
   (scrollbar-x :initform nil :accessor scrollbar-x)
   (scrollbar-y :initform nil :accessor scrollbar-y)
   (resize-grips :initform nil :accessor resize-grips)
   (window-scrollbar-sizes :initform (make-array 2) :accessor window-scrollbar-sizes)
   (rounding? :initform nil :accessor window-rounding?)
   (border-size :initform nil :accessor window-border-size)
   (collapsed? :initform nil :accessor window-collapsed?)
   (alpha :initform 1.0f0 :accessor window-alpha)
   (dock-is-active? :initform nil :accessor dock-is-active?)
   (flags :initform 0 :accessor window-flags)))

(defmethod clui::window-p ((window homemade-window-mixin))
  t)

(defmethod z-index ((window homemade-window-mixin))
  (/ (view-layer window) +view-depth+))

(defmethod reindex-view ((window homemade-window-mixin))
  (let ((title-bar (window-title-bar window)))
    (when title-bar
      (reindex-view title-bar)))
  (values))

(defmethod reindex-view ((title-bar title-bar))
  (let ((window (node-parent title-bar)))
    (setf (view-layer title-bar) (1+ (view-layer window)))
    (let ((collapse-button (collapse-button title-bar)))
      (when collapse-button
	(reindex-view collapse-button)))
    (let ((close-button (close-button title-bar)))
      (when close-button
	(reindex-view close-button)))
    (values)))

(defmethod reindex-view ((button collapse-button))
  (let ((title-bar (node-parent button)))
    (setf (view-layer button) (1+ (view-layer title-bar)))))

(defmethod reindex-view ((button close-button))
  (let ((title-bar (node-parent button)))
    (setf (view-layer button) (1+ (view-layer title-bar)))))

(defmethod reindex-views ((scene krma-essential-scene-mixin) sorted-views)
  (do ((children sorted-views (cdr children))
       (new-index 0 (1+ new-index)))
      ((null children) (return-from reindex-views (length sorted-views)))
    (setf (view-layer (car children)) (* new-index +view-depth+))
    (reindex-view (car children))))

(defmethod clui:focus-window ((window homemade-window-mixin))
  (let ((scene (node-scene window)))
    (let ((siblings (remove window (node-children scene))))
      (setq siblings (sort siblings #'< :key #'z-index))
      (let ((new-index (reindex-views scene siblings)))
	(setf (view-layer window) (* new-index +view-depth+))
	(reindex-view window)
	(delete-groups scene (mapcar #'object-group (node-children scene)) nil)
	(loop for sibling in siblings
	      do (add-homemade-window sibling :style (window-style sibling))
	      finally (add-homemade-window window :style (window-style window)))
      (values)))))

(defmethod (setf z-index) (z-index (window homemade-window-mixin))
  (setq z-index (round z-index))
  (setf (view-layer window) (* z-index +view-depth+))
  (reindex-view window)
  (refresh-view window)
  z-index)

(defmethod clui:window-position ((view view-mixin))
  (values (rect-x view) (rect-y view) (view-layer view)))

(defmethod clui:window-position ((view group-owner-view-mixin))
  (let* ((vec4 (vec4 (rect-x view) (rect-y view) (view-layer view) 1))
	 (actual-pos (safe-euclid (m* vec4 (model-matrix view)))))
    (values (vx actual-pos) (vy actual-pos) (vz actual-pos))))

(defmethod translate-node ((node rect-mixin) (vec vec3))
  (setf (rect-x node) (vx vec)
	(rect-y node) (vy vec))
  (values))

(defmethod translate-node ((node view-mixin) (vec vec3))
  (call-next-method)
  (setf (view-layer node) (vz vec))
  (values))

(defmethod clui:set-window-position ((view view-mixin) x y)
  (translate-node view (vec3 x y (view-layer view)))
  (refresh-view (group-owner view))
  (values))

(defmethod clui:set-window-position ((view group-owner-view-mixin) x y)
  (multiple-value-bind (cur-x cur-y) (clui:window-position view)
    (let* ((dx (- x cur-x))
	   (dy (- y cur-y))
	   (vec3 (vec3 dx dy 0)))
      (translate-node view vec3))))
  
(defmethod clui:window-size ((window view-mixin))
  (values (rect-width window) (rect-height window)))

(defmethod clui:set-window-size ((window group-owner-view-mixin) (width real) (height real))
  (setf (rect-width window) width
	(rect-height window) height)
  (delete-object-from-scene window)
  (add-homemade-window window :style (window-style window))
  (values))

(defmethod transform-coordinate ((vec vec2) (view view-mixin))
  (transform-coordinate (vec3 (vx vec) (vy vec) (view-layer view)) view))

(defmethod initialize-instance :after ((window homemade-window-mixin) &rest initargs
				       &key (title-bar? t) (menu-bar? nil)
					 (scene (application-scene *app*))
					 (style *style*)
					 (x 0)
					 (y 0)
				       &allow-other-keys)
  (declare (ignorable initargs menu-bar?))

  (if (node-children scene)
      
      (let ((highest-z-index
	  (loop for child in (node-children scene)
		maximize (z-index child))))
	(setf (view-layer window) (* (1+ highest-z-index) +view-depth+)))
      
      (setf (view-layer window) 0))

  (setf (%window-style window) style)
  
  (setf (window-border-size window) (default-window-border-size style))

  (setf (object-group window) (new-group))
  
  (setf (node-scene window) scene)

  (when title-bar?
    (let ((title-bar (make-instance 'title-bar
				    :parent window
)))	    
      
      (setf (window-title-bar window) title-bar)))

  (nmtranslate (model-matrix window) (vec3 x y 0))
  
  (setf (node-children scene) (append (node-children scene) (list window)))

  (let ((tb-height (title-bar-height style)))
    (setf (window-client-rect window) (make-instance 'client-rect
						     :rect-x 0 :rect-y tb-height
						     :width (rect-width window)
						     :height (- (rect-height window) tb-height))))

  (refresh-view window)
  
  (values))

(defmethod refresh-view ((window homemade-window-mixin))
  (delete-object-from-scene window)
  (scene-ensure-group (node-scene window) (object-group window))
  (add-homemade-window window :style (window-style window))
  (group-set-model-matrix (node-scene window) (object-group window) (global-model-matrix window))
  (values))
  

(defmethod destroy-object ((window homemade-window-mixin))
  ;; remove the graphical representation from the draw-lists
  (delete-object-from-scene window)

  ;; remove the references to the object so it will no longer 
  ;; answer messages or be re-added, and can be garbage collected
  (setf (node-children (node-scene window)) (remove window (node-children (node-scene window))))

  (remhash (object-id window) *object-id->object-table*)
  
  (when (window-title-bar window)
    (destroy-object (window-title-bar window))
    (setf (window-title-bar window) nil))

  (when (window-menu-bar window)
    (destroy-object (window-menu-bar window))
    (setf (window-menu-bar window) nil)))

(defun add-homemade-window (window &key (style *style*) (no-resize? nil))
  
  (unless (clui::window-iconified? window)
    (add-window-background window :style style))
  
  (add-title-bar (window-title-bar window) (window-title window) :style style)
  ;;(add-menu-bar (window-menu-bar window) :style style)
  (let ((scrollbar-x (scrollbar-x window)))
    (when scrollbar-x
      (add-scrollbar scrollbar-x :horizontal)))

  (let ((scrollbar-y (scrollbar-y window)))
    (when scrollbar-y
      (add-scrollbar scrollbar-y :vertical)))

  (unless no-resize?
    (loop for grip in (resize-grips window)
	  do (let ((corner (lerp (vector (rect-x0 window) (rect-y0 window))
				 (vector (rect-x1 window) (rect-y1 window))
				 (corner-pos grip))))
	       (declare (ignorable corner))
	       ))))

(defun add-window-background (window &key (style *style*))
  (let ((scene (node-scene window))
	(group (object-group window)))
    ;; draw window background
    (scene-add-filled-2d-rectangle-list-primitive
     scene group nil (window-bg-color style)
     (list
      (rect-x window)
      (rect-y window)
      (+ (rect-x window) (rect-width window))
      (+ (rect-y window) (rect-height window)))
     (object-id window)
     (view-layer window))))

(defun get-window-scrollbar-rect (window &optional (axis :vertical))
  (let* ((rect-x0 (rect-x0 window))
	 (rect-y0 (rect-y0 window))
	 (rect-x1 (rect-x1 window))
	 (rect-y1 (rect-y1 window))
	 (client-rect (window-client-rect window))
	 (client-x0 (rect-x0 client-rect))
	 (client-y0 (rect-y0 client-rect))
	 (client-x1 (rect-x1 client-rect))
	 (client-y1 (rect-y1 client-rect))
	 (border-size (window-border-size window))
	 (scrollbar-size (if (eq axis :horizontal)
			     (elt (window-scrollbar-sizes window) 0)
			     (elt (window-scrollbar-sizes window) 1))))
    (case axis
      (:horizontal (values client-x0 (max rect-y0 (- rect-y1 border-size scrollbar-size))
			   client-x1 rect-y1))
      (otherwise
       (unless (eq axis :vertical)
	 (warn "`axis' ~S is not valid, wanted :horizontal or :vertical" axis))
       (values (max rect-x0 (- rect-x1 border-size scrollbar-size)) client-y0
	       rect-x1 client-y1)))))

(defun add-scrollbar (scrollbar axis &key (style *style*))
  (let* ((window (node-parent scrollbar))
	 (scene (node-scene scrollbar))
	 (group (object-group scrollbar))
	 (client-rect (window-client-rect window))
	 (size-avail (if (eq axis :horizontal)
			 (- (rect-x1 client-rect)
			    (rect-x0 client-rect))
			 (- (rect-y1 client-rect)
			    (rect-y0 client-rect))))
	 (size-contents (if (eq axis :horizontal)
			    (+ size-avail (* 2.0f0 (elt (window-padding window) 0)))
			    (+ size-avail (* 2.0f0 (elt (window-padding window) 1))))))
    (multiple-value-bind (bb-x0 bb-y0 bb-x1 bb-y1)
	(get-window-scrollbar-rect window axis)
      (scene-add-scrollbar scene group bb-x0 bb-y0 bb-x1 bb-y1
			   window
			   axis
			   (if (eq axis :horizontal)
			       (elt (scroll-amount window) 0)
			       (elt (scroll-amount window) 1))
			   size-avail size-contents
			   :style style))))

#+NOTYET
(defun scene-add-scrollbar (scene group bb-x0 bb-y0 bb-x1 bb-y1 window axis scroll-amount size-avail size-contents &key (style *style*))
  (let ((frame-width (- bb-x1 bb-x0))
	(frame-height (- bb-y1 bb-y0))
	(font-size (font-size (current-font style))))

    (unless (and (plusp frame-width) (plusp frame-height))
      (warn "invalid scrollbar size: ~S X ~S" frame-width frame-height)
      (return-from scene-add-scrollbar-1 (values)))

    (let ((alpha 1.0f0))
      (when (and (eq axis :vertical) (< frame-height (+ font-size (* 2.0f0 (frame-padding-y style)))))
	(setq alpha (saturate (/ (- frame-height font-size) (* 2.0f0 (frame-padding-y style)))))))
    (unless (plusp alpha)
      (return-from scene-add-scrollbar-1 (values)))

    (let ((allow-interaction? )))))
								     


			    

(defun add-close-button (close-button pos-x pos-y &key (style *style*)
						    (hovered? nil))
  (let* ((scene (node-scene close-button))
	 (font-size (font-size (current-font style)))
	 (bounding-box-x0 pos-x)
	 (bounding-box-y0 pos-y)
	 (bounding-box-x1 (+ pos-x font-size (* 2.0f0 (frame-padding-x style))))
	 (bounding-box-y1 (+ pos-y font-size (* 2.0f0 (frame-padding-y style))))
	 (center-x (/ (+ bounding-box-x0 bounding-box-x1) 2.0f0))
	 (center-y (/ (+ bounding-box-y0 bounding-box-y1) 2.0f0))
	 (cross-extent (1- (* font-size 0.5f0 0.7071f0)))
	 (cross-color (text-color style))
	 (object-id (object-id close-button))
	 (group (object-group close-button))
	 (layer (view-layer close-button)))

    (push (scene-add-filled-2d-circle-primitive scene group nil (if hovered? (button-hovered-color *style*) 0)
						  center-x center-y (max 2.0f0 (1+ (* 0.5f0 font-size)))
						  12
						  object-id
						  layer)
	    (object-primitive-handles close-button))

    (push (scene-add-2d-line-primitive scene group nil 2.0f0 cross-color
				       (+ center-x cross-extent)
				       (+ center-y cross-extent)
				       (- center-x cross-extent)
				       (- center-y cross-extent)
				       object-id
				       (1+ layer))
	  (object-primitive-handles close-button))
    
    (push (scene-add-2d-line-primitive scene group nil 2.0f0 cross-color
				       (+ center-x cross-extent)
				       (- center-y cross-extent)
				       (- center-x cross-extent)
				       (+ center-y cross-extent)
				       object-id
				       (1+ layer))
	  (object-primitive-handles close-button))

    (values)))

(defun scene-add-arrow (scene group pos-x pos-y color dir scale object-id layer &key (style *style*))
  (let* ((h (font-size (current-font style)))
	 (r (* h 0.40f0 scale))
	 (center-x (+ pos-x (* h 0.50f0)))
	 (center-y (+ pos-y (* h 0.50f0 scale)))
	 (a-x)
	 (a-y)
	 (b-x)
	 (b-y)
	 (c-x)
	 (c-y))

    (case dir
      ((:up :down)
       (when (eq dir :up) (setq r (- r)))
       (setq a-x 0.0f0
	     a-y (*  0.750 r)
	     b-x (* -0.866 r)
	     b-y (* -0.750 r)
	     c-x (*  0.866 r)
	     c-y (* -0.750 r)))
      ((:left :right)
       (when (eq dir :left) (setq r (- r)))
       (setq a-x (*  0.750 r)
	     a-y 0.0f0
	     b-x (* -0.750 r)
	     b-y (*  0.866 r)
	     c-x (* -0.750 r)
	     c-y (* -0.866 r)))
      (otherwise
       (warn "argument `dir' is not valid: ~S, needs to be :up, :down, :left, :right" dir)
       (setq a-x 0.0f0
	     a-y (*  0.750 r)
	     b-x (* -0.866 r)
	     b-y (* -0.750 r)
	     c-x (*  0.866 r)
	     c-y (* -0.750 r)))) ;; down

    (scene-add-filled-2d-triangle-list-primitive
     scene group nil color
     (list (+ center-x a-x) (+ center-y a-y)
	   (+ center-x b-x) (+ center-y b-y)
	   (+ center-x c-x) (+ center-y c-y))
     object-id
     layer)))

(defun scene-add-arrow-pointing-at (scene group pos-x pos-y half-size-x half-size-y color dir &optional (object-id 0) (layer 0))
  (let ((a-x)
	(a-y)
	(b-x)
	(b-y))
    
    (case dir
      (:left
       (setq a-x (+ pos-x half-size-x))
       (setq a-y (- pos-y half-size-y))
       (setq b-x (+ pos-x half-size-x))
       (setq b-y (+ pos-y half-size-y)))
      (:right
       (setq a-x (- pos-x half-size-x))
       (setq a-y (+ pos-y half-size-y))
       (setq b-x (- pos-x half-size-x))
       (setq b-y (- pos-y half-size-y)))
      (:up
       (setq a-x (+ pos-x half-size-x))
       (setq a-y (+ pos-y half-size-y))
       (setq b-x (- pos-x half-size-x))
       (setq b-y (+ pos-y half-size-y)))
      (otherwise
       (unless (eq dir :down)
	 (warn "argument `dir' is not valid: ~S, needs to be :up, :down, :left, :right" dir))
       (setq a-x (- pos-x half-size-x))
       (setq a-y (- pos-y half-size-y))
       (setq b-x (+ pos-x half-size-x))
       (setq b-y (- pos-y half-size-y))))

    (scene-add-filled-2d-triangle-list-primitive
     scene group nil color
     (list a-x a-y
	   b-x b-y
	   pos-x pos-y)
     object-id
     layer)))    

(defun scene-add-arrow-dock-menu (scene group p-min-x p-min-y size color object-id layer &key (style *style*))
  (declare (ignorable style))
  (scene-add-filled-2d-rectangle-list-primitive
   scene group nil color
   (list (+ p-min-x (* size 0.10f0))
	 (+ p-min-y (* size 0.15f0))
	 (+ p-min-x (* size 0.70f0))
	 (+ p-min-y (* size 0.30f0)))
   object-id
   layer)
  (scene-add-arrow-pointing-at scene group
			       (+ p-min-x (* size 0.40f0))
			       (+ p-min-y (* size 0.85f0))
			       (* size 0.30f0)
			       (* size 0.40f0)
			       color
			       :down
			       object-id
			       layer))  

(defun add-collapse-button (collapse-button pos-x pos-y &key (style *style*) (dock-node? nil)
							  (hovered? nil) (window-collapsed? nil))

  (let* ((scene (node-scene collapse-button))
	 (font-size (font-size (current-font style)))
	 ;;(off-x (if dock-node? (+ (floor (* 0.5f0 (- (item-inner-spacing-x style)))) 0.5f0) 0.0f0))
	 ;;(off-y 0.0f0)
	 ;;(bg-col (button-color style))
	 (text-col (text-color style))
	 (bounding-box-x0 pos-x)
	 (bounding-box-y0 pos-y)
	 (bounding-box-x1 (+ pos-x (+ (font-size (current-font style)) (* 2.0f0 (frame-padding-x style)))))
	 (bounding-box-y1 (+ pos-y (+ (font-size (current-font style)) (* 2.0f0 (frame-padding-y style)))))
	 (center-x (/ (+ bounding-box-x0 bounding-box-x1) 2.0f0))
	 (center-y (/ (+ bounding-box-y0 bounding-box-y1) 2.0f0))
	 (object-id (object-id collapse-button))
	 (layer (view-layer collapse-button)))

    (push (scene-add-filled-2d-circle-primitive scene (object-group collapse-button) nil (if hovered? (button-hovered-color *style*) 0)
						center-x center-y (max 2.0f0 (1+ (* 0.5f0 font-size)))
						12
						object-id
						layer)
	  (object-primitive-handles collapse-button))

    (if dock-node?
	(scene-add-arrow-dock-menu scene (object-group collapse-button) 
				   (+ bounding-box-x0 (frame-padding-x style))
				   (+ bounding-box-y0 (frame-padding-y style))
				   (font-size (current-font style))
				   text-col
				   object-id
				   (1+ layer)
				   :style style)
	(scene-add-arrow scene (object-group collapse-button)
			 (+ bounding-box-x0 (frame-padding-x style))
			 (+ bounding-box-y0 (frame-padding-y style))
			 text-col (if window-collapsed? :right :down)
			 1.0f0
			 object-id
			 (1+ layer)
			 :style style))
    (values)))

(defun add-menu-bar (menu-bar &key (style *style*))
  (let ((scene (node-scene menu-bar))
	(group (object-group menu-bar))
	(window (node-parent menu-bar)))
    (scene-add-filled-2d-rectangle-list-primitive
     scene group nil (menu-bar-bg-color style)
     (list (+ (rect-x0 menu-bar) (window-border-size window))
	   (rect-y0 menu-bar)
	   (- (rect-x1 menu-bar) (window-border-size window))
	   (rect-y1 menu-bar))
     (object-id menu-bar)
     (view-layer menu-bar))
    (when (and (plusp (frame-border-size style))
	       (> (rect-y1 menu-bar) (+ (rect-y window) (rect-height window))))
      (scene-add-2d-line-primitive scene group nil (frame-border-size style) (border-color style)
			 (rect-x0 menu-bar) (rect-y1 menu-bar)
			 (rect-x1 menu-bar) (rect-y1 menu-bar)
			 (object-id menu-bar)
			 (view-layer menu-bar)))))



(defmethod clui:handle-event ((window homemade-window-mixin) (event clui::pointer-button-press-event-mixin))
  (clui:focus-window window)
  (call-next-method))

(defmethod clui:handle-event ((window krma-window-mixin) (event clui::pointer-button-press-event-mixin))
  (call-next-method)
  (let* ((hovered (most-specifically-hovered-2d (krma-select-box-2d (clui::window-display window)) 0 0)))
    (when hovered
      (let ((object (object-from-id hovered)))
	(when object
	  (clui:handle-event object event))))
    (values)))

(defclass popup-window-mixin (homemade-window-mixin)
  ())

(defmethod get-window-bg-color-accessor ((window popup-window-mixin))
  #'popup-bg-color)		      
  
(defclass homemade-window (homemade-window-mixin)
  ())

(defun make-test-window (&optional (app *app*))
  
  (let ((w (make-instance 'homemade-window :x 25 :y 50
					   :width 300 :height 200
					   :parent (main-window *app*)
					   :scene (application-scene app))))
    w))

(defun test (n)
  (add-filled-2d-rectangle-list-primitive (list 175 175 275 275) :color #x00000077 :layer n))

(defun test2 (n)
  (add-filled-2d-rectangle-list-primitive (list 225 225 325 325) :color #x00000077 :layer n))

(defun test3 ()
  (loop with prim-hndls = ()
	for i from 1 below 20
	when (evenp i)
	do (push (test i) prim-hndls)
	   (push (test2 (1+ i)) prim-hndls)
	when (oddp i)
	  do (push (test (1+ i)) prim-hndls)
	     (push (test2 i) prim-hndls)
	     
	do
	   (sleep 1)
	   (mapcar #'(lambda (id) (delete-primitive (application-scene *app*) id)) prim-hndls)
	   (setq prim-hndls ())
	   (sleep 0.1)
	))

(defun test5 (n)
  (print n)
  (add-filled-2d-rectangle-list (list 175 175 275 275) :group :g1 :color #x00000077 :layer n))

(defun test6 (n)
  (print n)
  (add-filled-2d-rectangle-list (list 225 225 325 325) :group :g2 :color #x00000077 :layer n))

(defun test4 ()
  (loop for i from 1 below 21
	with j
	with k
	with tmp
	do (setq j i)
	   (setq k (1+ i))
	   (if (evenp (mod i 2))
	       (setq tmp j
		     j k
		     k tmp))
	   (print "----")
	   (test5 j)
	   (test6 k)
	   (finish-output)
	   (sleep 1)
	   (delete-groups (application-scene *app*) (list :g1 :g2))
	   (sleep 0.1)
	   ))
