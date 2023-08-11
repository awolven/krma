# krma

KRMA is a graphics abstraction layer for rendering 2d and 3d graphics.  It currently uses Vulkan as a backend.

KRMA supports immediate mode and retained mode methods of rendering.

KRMA works on SBCL with partial support for CCL and can use MacOS (Cocoa), MS Windows (Win32) or Linux (X11).

To install krma, clone the repository:

git clone git@github.com:awolven/krma.git

then from a shell:

cd krma

git submodule update --init --recursive

On Linux you must install the following packages before building:

libXinerama-devel
libXrandr-devel
libXcursor-devel

On MacOS you must install MoltenVK.  I have a patch somewhere for MoltenVK.  Contact <awolven@gmail.com>.

then from lisp:  (assumes SBCL or CCL with quicklisp)

(push "~/krma/" asdf:*central-registry*)

(ql:quickload :krma)

(krma:add-2d-line 100 100 200 200 :color #xffff00ff)
(krma:add-text "Sample Text" 250 200)

There is slightly out of date documentation in krma/documentation/.

Good or bad, it's krma.
