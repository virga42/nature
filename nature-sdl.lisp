(ql:quickload :lispbuilder-sdl)

(defpackage :nature
  (:use :cl :lispbuilder-sdl :ltk))

(in-package :nature)

(load "/home/tcallahan/bottega/nature/nature-ltk.lisp")

(defun setup ()
  (sdl:with-init ()
    (sdl:window 800 600 :title-caption "fun coding nature")
    (setf (sdl:frame-rate) 30)
    (draw)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
		       (if (sdl:key= key :sdl-key-escape) (sdl:push-quit-event)))
      (:idle ()
	     (update-display)
	     (process-events)))))

(defun draw ()
  (let ((a-circle (draw-circle-* 100 100 25)))
    (update-display)))
