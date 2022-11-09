(defsystem krma
  :description "A platform independent graphics primitive rendering library."
  :depends-on (:cffi :3d-vectors :3d-matrices #-darwin :cl-vulkan #+darwin :cl-metal
                                              :sdf/bmfont :3b-bmfont/json)
  :author "Andrew K Wolven <awolven@gmail.com>"
  :serial t
  :components
  ((:file "package")
   (:file "setup")
   (:file "utilities")
   (:file "macros")
   (:file "foreign-arrays")
   (:file "cmds")
   (:file "draw-list-classes")
   (:file "draw-list")
   (:file "compact-draw-list")
   (:file "draw-data")
   (:file "scene-mixin")
   (:file "application-mixin")
   (:file "vulkan-pipelines")
   (:file "pipeline-combinations")
   (:file "text")))

(pushnew :krma *features*)
(pushnew (asdf/system:system-relative-pathname :krma "submodules/sdf/") asdf:*central-registry* :test #'equalp)
(pushnew (asdf/system:system-relative-pathname :krma "submodules/binpack/") asdf:*central-registry* :test #'equalp)
(pushnew (asdf/system:system-relative-pathname :krma "submodules/3b-bmfont/") asdf:*central-registry* :test #'equalp)
#+NOTYET
(pushnew (asdf/system:system-relative-pathname :krma "submodules/zpb-ttf/") asdf:*central-registry* :test #'equalp)
