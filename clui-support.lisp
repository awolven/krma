(in-package :clui)

#+win32
(defclass win32::display-with-krma-mixin (krma::krma-enabled-display-mixin win32:display-mixin)
  ())

#+win32
(defclass win32::display-with-krma (win32::display-with-krma-mixin)
  ())

#+win32
(defclass win32::krma-enabled-window-mixin (krma::krma-window win32:window-mixin)
  ())

#+win32
(defclass win32::krma-enabled-window (win32::krma-enabled-window-mixin)
  ())

#+win32
(export '(win32::display-with-krma-mixin
	  win32::display-with-krma
	  win32::krma-enabled-window-mixin
	  win32::krma-enabled-window)
	:win32)


#+cocoa
(defclass cocoa::display-with-krma-mixin (krma::krma-enabled-display-mixin cocoa:display-mixin)
  ())

#+cocoa
(defmethod cocoa:application-did-finish-launching ((dpy cocoa::display-with-krma-mixin) notification)
  (declare (ignorable notification))
  (call-next-method)
  (krma::start-compactor-thread dpy)
  (values))

#+cocoa
(defclass cocoa::display-with-krma (cocoa::display-with-krma-mixin)
  ())

#+cocoa
(defclass cocoa::krma-enabled-window-mixin (krma::krma-window cocoa:window-mixin)
  ((layer :accessor window-layer)))

#+cocoa
(defmethod cocoa-window-wants-update-layer ((window cocoa::krma-enabled-window-mixin))
  t)

#+cocoa
(defclass cocoa::krma-enabled-window (cocoa::krma-enabled-window-mixin)
  ())

#+cocoa
(defclass cocoa::vulkan-helper-window (vk::vulkan-helper-window cocoa::helper-window)
  ((vk::handle :accessor objc-object-id)
   (layer :accessor window-layer)))

#+cocoa
(export '(cocoa::display-with-krma-mixin
	  cocoa::display-with-krma
	  cocoa::krma-enabled-window-mixin
	  cocoa::krma-enabled-window
	  cocoa::vulkan-helper-window)
	:cocoa)

#+x11
(defclass x11::local-server-with-krma-mixin (krma::krma-enabled-display-mixin x11:local-server-mixin)
  ())

#+x11
(defclass x11::local-server-with-krma (x11::local-server-with-krma-mixin)
  ())

#+x11
(defclass x11::krma-enabled-window-mixin (krma::krma-window x11:window-mixin)
  ())

#+x11
(defclass x11::krma-enabled-window (x11::krma-enabled-window-mixin)
  ())

#+x11
(export '(x11::local-server-with-krma-mixin
	  x11::local-server-with-krma
	  x11::krma-enabled-window-mixin
	  x11::krma-enabled-window)
	:x11)

#+wayland
(export '(wayland::display-with-krma-mixin
	  wayland::display-with-krma
	  wayland::krma-enabled-window-mixin
	  wayland::krma-enabled-window)
	:wayland)

#+wayland
(defclass wayland::display-with-krma-mixin (krma::krma-enabled-display-mixin wayland:display-mixin)
  ())

#+wayland
(defclass wayland::display-with-krma (wayland::display-with-krma-mixin)
  ())

#+wayland
(defclass wayland::krma-enabled-window-mixin (krma::krma-window wayland:window-mixin)
  ())

#+wayland
(defclass wayland::krma-enabled-window (wayland::krma-enabled-window-mixin)
  ())

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
  (list* (find-class 'win32::display-with-krma) initargs))

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
  (list* (find-class 'cocoa::display-with-krma) initargs))

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
  (list* (find-class 'cocoa::display-with-krma) initargs))

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
  (list* (find-class 'x11::local-server-with-krma) initargs))


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
  (list* (find-class 'wayland::display-with-krma) initargs))

#+win32
(defmethod get-a-win32-window-class ((display win32::display-with-krma-mixin) errorp &rest initargs
				     &key &allow-other-keys)
  (declare (ignore initargs))
  (find-class 'win32::krma-enabled-window errorp))

#+x11
(defmethod get-an-x11-window-class ((display x11::local-server-with-krma-mixin) errorp &rest initargs
				     &key &allow-other-keys)
  (declare (ignore initargs))
  (find-class 'x11::krma-enabled-window errorp))

#+cocoa
(defmethod get-a-cocoa-window-class (display errorp &rest initargs &key &allow-other-keys)
  (declare (ignore display initargs))
  (find-class 'cocoa::krma-enabled-window errorp))

#+win32
(defmethod create-native-window-surface ((display win32::display-with-krma-mixin)
					 instance window
					 &optional (allocator vk::+null-allocator+))
  (vk::create-win32-window-surface instance window allocator))

#+cocoa
(defmethod create-native-window-surface ((display cocoa::display-with-krma-mixin)
					 instance window
					 &optional (allocator vk::+null-allocator+))
  (vk::create-cocoa-window-surface window allocator))

#+x11
(defmethod create-native-window-surface ((display x11::local-server-with-krma-mixin)
					 instance window
					 &optional (allocator vk::+null-allocator+))
  (vk::create-x11-window-surface display instance window allocator))

#+x11
(defmethod compute-make-display-instance-arguments (protocol
						    (cocoa null)
						    (metal null)
						    (opengl null)
						    (vulkan t)
						    (wayland null)
						    (win32 null)
						    (x11 t)
						    &rest initargs
						    &key &allow-other-keys)
  (declare (ignorable protocol))
  (list* (find-class 'x11::local-server-with-krma) initargs))


#+wayland
(defmethod create-native-window-surface ((display wayland:display-mixin)
					 instance (window wayland:window-mixin)
					 &optional (allocator vk::+null-allocator+))
  (vk::create-wayland-window-surface instance window allocator))



#+win32
(defmethod helper-window-class ((display win32:display-with-krma-mixin))
  'vk::vulkan-helper-window)

#+cocoa
(defmethod helper-window-class ((display cocoa::display-with-krma-mixin))
  'cocoa::vulkan-helper-window)

#+x11
(defmethod helper-window-class ((display x11::local-server-with-krma-mixin))
  'vk::vulkan-helper-window)


