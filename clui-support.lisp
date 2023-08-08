(in-package :clui)

#+win32
(defclass win32:desktop-with-krma-mixin (krma::krma-enabled-display-mixin win32:desktop-mixin)
  ())

#+win32
(defclass win32:desktop-with-krma (win32:desktop-with-krma-mixin)
  ())

#+win32
(defclass win32:krma-enabled-window-mixin (krma::krma-window win32:window-mixin)
  ())

#+win32
(defclass win32:krma-enabled-window (win32:krma-enabled-window-mixin)
  ())

#+cocoa
(defclass cocoa:desktop-with-krma-mixin (krma::krma-enabled-display-mixin cocoa:desktop-mixin)
  ())

#+cocoa
(defmethod clui::application-did-finish-launching ((dpy cocoa:desktop-with-krma-mixin) notification)
  (declare (ignorable notification))
  ;;(abstract-os::post-empty-event application)
  ;;(ns::|stop:| application nil)
  (call-next-method)
  (krma::start-compactor-thread dpy)
  (values))

#+cocoa
(defclass cocoa:desktop-with-krma (cocoa:desktop-with-krma-mixin)
  ())

#+cocoa
(defclass cocoa:krma-enabled-window-mixin (krma::krma-window cocoa:window-mixin)
  ((layer :accessor window-layer)))

#+cocoa
(defmethod cocoa-window-wants-update-layer ((window cocoa::krma-enabled-window-mixin))
  t)

#+cocoa
(defclass cocoa:krma-enabled-window (cocoa:krma-enabled-window-mixin)
  ())

#+cocoa
(defclass cocoa::vulkan-helper-window (vk::vulkan-helper-window cocoa::helper-window)
  ((vk::handle :accessor objc-object-id)
   (layer :accessor window-layer)))




#+x11
(defclass x11:local-server-with-krma-mixin (krma::krma-enabled-display-mixin x11:local-server-mixin)
  ())

#+x11
(defclass x11:local-server-with-krma (x11:local-server-with-krma-mixin)
  ())

#+x11
(defclass x11:krma-enabled-window-mixin (krma::krma-window x11:window-mixin)
  ())

#+x11
(defclass x11:krma-enabled-window (x11:krma-enabled-window-mixin)
  ())

#+wayland
(defclass wayland:desktop-with-krma-mixin (krma::krma-enabled-display-mixin wayland:desktop-mixin)
  ())

#+wayland
(defclass wayland:desktop-with-krma (wayland:desktop-with-krma-mixin)
  ())

#+wayland
(defclass wayland:krma-enabled-window-mixin (krma::krma-window wayland:window-mixin)
  ())

#+wayland
(defclass wayland:krma-enabled-window (wayland:krma-enabled-window-mixin)
  ())

#+win32
(defmethod vk::get-required-instance-extensions ((display win32:desktop-with-krma-mixin))
  (vk::get-win32-required-instance-extensions))

#+cocoa
(defmethod vk::get-required-instance-extensions ((display cocoa:desktop-with-krma-mixin))
  (vk::get-cocoa-required-instance-extensions))

#+x11
(defmethod vk::get-required-instance-extensions ((display x11:local-server-with-krma-mixin))
  (vk::get-x11-required-instance-extensions))

#+wayland
(defmethod vk::get-required-instance-extensions ((display wayland:desktop-with-krma-mixin))
  (get-wayland-required-instance-extensions))

#+win32
(defmethod compute-make-display-instance-arguments ((protocol clui:display)
						    (cocoa null)
						    (metal null)
						    (opengl null)
						    (vulkan t)
						    (wayland null)
						    (win32 t)
						    (x11 null)
						    &rest initargs
						    &key &allow-other-keys)
  (list* (find-class 'win32:desktop-with-krma) initargs))

#+cocoa
(defmethod compute-make-display-instance-arguments (protocol
						    (cocoa t)
						    (metal null)
						    (opengl null)
						    (vulkan t)
						    (wayland null)
						    (win32 null)
						    (x11 null)
						    &rest initargs)
  (list* (find-class 'cocoa:desktop-with-krma) initargs))

#+cocoa
(defmethod compute-make-display-instance-arguments (protocol
						    (cocoa t)
						    (metal null)
						    (opengl null)
						    (vulkan t)
						    (wayland null)
						    (win32 null)
						    (x11 t)
						    &rest initargs)
  (declare (ignore protocol))
  (list* (find-class 'cocoa:desktop-with-krma) initargs))

#+x11
(defmethod compute-make-display-instance-arguments (protocol
						    (cocoa null)
						    (metal null)
						    (opengl null)
						    (vulkan t)
						    (wayland null)
						    (win32 null)
						    (x11 t)
						    &rest initargs)
  (list* (find-class 'x11:local-server-with-krma) initargs))


#+wayland
(defmethod compute-make-display-instance-arguments (protocol
						    (cocoa null)
						    (metal null)
						    (opengl null)
						    (vulkan t)
						    (wayland t)
						    (win32 null)
						    (x11 null)
						    &rest initargs)
  (list* (find-class 'wayland:desktop-with-krma) initargs))

#+win32
(defmethod get-a-win32-window-class ((display win32:desktop-with-krma-mixin) errorp &rest initargs
				     &key &allow-other-keys)
  (declare (ignore initargs))
  (find-class 'win32:krma-enabled-window errorp))

;; :animable? used to be a keyword here, but animable? affects whether the
;; run loop polls for messages or waits for messages, and vulkan can also
;; be used in a demand-refresh wait-for-messages manner


#+x11
(defmethod get-an-x11-window-class ((display x11:local-server-with-krma-mixin) errorp &rest initargs
				     &key &allow-other-keys)
  (declare (ignore initargs))
  (find-class 'x11:krma-enabled-window errorp))

#+win32
(defmethod create-native-window-surface ((display win32:desktop-mixin)
					 instance window
					 &optional (allocator vk::+null-allocator+))
  (vk::create-win32-window-surface instance window allocator))

#+cocoa
(defmethod create-native-window-surface ((display cocoa:desktop-with-krma-mixin)
					 instance window
					 &optional (allocator vk::+null-allocator+))
  (vk::create-cocoa-window-surface window allocator))

#+x11
(defmethod create-native-window-surface ((display x11:local-server-with-krma-mixin)
					 instance window
					 &optional (allocator vk::+null-allocator+))
  (vk::create-x11-window-surface display instance window allocator))


#+wayland
(defmethod create-native-window-surface ((display wayland:desktop-mixin)
					 instance (window wayland:window-mixin)
					 &optional (allocator vk::+null-allocator+))
  (vk::create-wayland-window-surface instance window allocator))

#+cocoa
(defmethod initialize-helper-window ((display cocoa:desktop-with-krma-mixin) helper-window)
  (setf (vk::render-surface helper-window)
	(create-native-window-surface display
				      (vk::get-vulkan-instance display)
				      helper-window))
  (setf (vk::window (vk::render-surface helper-window)) helper-window)
  helper-window)

#+win32
(defmethod initialize-helper-window ((display win32:desktop-with-krma-mixin) helper-window)
  (setf (vk::render-surface helper-window)
	(create-native-window-surface display
				      (vk::get-vulkan-instance display)
				      helper-window))
  (setf (vk::window (vk::render-surface helper-window)) helper-window)
  helper-window)

#+x11
(defmethod initialize-helper-window ((display x11:local-server-with-krma-mixin) helper-window)
  (setf (vk::render-surface helper-window)
	(create-native-window-surface display
				      (vk::get-vulkan-instance display)
				      helper-window))
  (setf (vk::window (vk::render-surface helper-window)) helper-window)
  helper-window)

#+win32
(defmethod helper-window-class ((display win32:desktop-with-krma-mixin))
  'vk::vulkan-helper-window)

#+cocoa
(defmethod helper-window-class ((display cocoa:desktop-with-krma-mixin))
  'cocoa::vulkan-helper-window)

#+x11
(defmethod helper-window-class ((display x11:local-server-with-krma-mixin))
  'vk::vulkan-helper-window)

(defmethod helper-window-class ((display clui:display-mixin))
  'vk::vulkan-helper-window)
