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
   (:file "draw-lists")
   (:file "draw-data")
   (:file "scene-mixin")
   (:file "application-mixin")
   (:file "vulkan-pipelines")
   (:file "text")))
