# krma

KRMA is a graphics abstraction layer for rendering 2d and 3d graphics.  It currently uses Vulkan as a backend.

KRMA supports immediate mode and retained mode methods of rendering.

KRMA works on SBCL with partial support for CCL and can use MacOS (Cocoa), MS Windows (Win32) or Linux (X11).

To install KRMA, clone the repository:

git clone https://github.com/awolven/krma

then from a shell:

cd krma

git submodule update --init --recursive

On Linux you must install the following packages before building:

libXinerama-devel
libXrandr-devel
libXcursor-devel
libX11-xcb-devel

then from lisp:  (assumes SBCL or CCL with quicklisp)

(push "~/krma/" asdf:*central-registry*)

(ql:quickload :krma)

(trivial-main-thread:call-in-main-thread #'(lambda () (krma:main (make-instance 'krma::krma-test-application))))

(krma:add-2d-line 100 100 200 200 :color #xffff00ff)
(krma:add-text "Sample Text" 250 200)

There is slightly out of date documentation in krma/documentation/.

KRMA currently doesn't work on macOS Ventura, check back for updates.

Good or bad, it's krma.
