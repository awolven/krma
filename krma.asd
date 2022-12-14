(defsystem krma
  :description "A platform independent graphics primitive rendering library."
  :depends-on (:cffi
	       :float-features
	       :3d-vectors :3d-matrices
	       :cl-vulkan ;;#+darwin :cl-metal
	       :sdf/bmfont :3b-bmfont/json
	       :trivial-main-thread)
  :author "Andrew K Wolven <awolven@gmail.com>"
  :license "MIT"
  :serial t
  :components
  ((:file "package")
   (:file "setup")
   (:file "utilities")
   (:file "macros")
   (:file "lights")
   (:file "materials")
   (:file "groups")
   (:file "draw-list-classes")
   (:file "foreign-arrays")
   (:file "cmds")
   (:file "draw-list")
   (:file "compact-draw-list")
   (:file "draw-data")
   (:file "scene-mixin")
   (:file "application-mixin")
   (:file "vulkan-pipelines")
   (:file "pipeline-combinations")
   (:file "text")
   (:file "main")))

(pushnew :krma *features*)
(pushnew (asdf/system:system-relative-pathname :krma "submodules/sdf/") asdf:*central-registry* :test #'equalp)
(pushnew (asdf/system:system-relative-pathname :krma "submodules/binpack/") asdf:*central-registry* :test #'equalp)
(pushnew (asdf/system:system-relative-pathname :krma "submodules/3b-bmfont/") asdf:*central-registry* :test #'equalp)
(pushnew (asdf/system:system-relative-pathname :krma "submodules/zpb-ttf/") asdf:*central-registry* :test #'equalp)
(pushnew (asdf/system:system-relative-pathname :krma "submodules/cl-vulkan/") asdf:*central-registry* :test #'equalp)
