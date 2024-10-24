(cl:pushnew :vulkan cl:*features*)
#+darwin(cl:pushnew :metal cl:*features*)

(defsystem krma
  :description "A cross platform graphics abstraction layer."
  :depends-on (:cffi
	       :float-features
	       :3d-math
	       :cl-vulkan ;;#+darwin :cl-metal
	       :sdf/bmfont :3b-bmfont/json
	       :trivial-main-thread
	       :lparallel)
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
   (:file "instance-array")
   (:file "cmds")
   (:file "draw-list")
   (:file "compact-draw-list")
   (:file "draw-data")
   (:file "scene-mixin")
   (:file "application-mixin")
   (:file "clui-support")
   (:file "vulkan-pipelines")
   (:file "pipeline-combinations")
   (:file "text")
   (:file "main")))

(setf (logical-pathname-translations "SUBMODULES")
      (list (list "**;*"
		  (merge-pathnames (make-pathname
				    :directory '(:relative :wild-inferiors)
				    :name :wild
				    :type :wild
				    :version :wild)
				   (asdf/system:system-relative-pathname :krma "submodules/")))))

(pushnew :krma *features*)

(let ((directories (list #p"submodules:sdf;"
			 #p"submodules:binpack;"
			 #p"submodules:3b-bmfont;"
			 #p"submodules:zpb-ttf;"
			 #p"submodules:clim-protocol;"
			 #p"submodules:cl-vulkan;"
			 #p"submodules:clui;"
			 #p"submodules:3d-math;")))
  (loop for directory in directories
	do (pushnew (translate-logical-pathname directory) asdf:*central-registry*)))


