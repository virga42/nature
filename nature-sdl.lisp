(ql:quickload :ltk)
(ql:quickload :lispbuilder-sdl)

(defpackage :nature
  (:use :cl :lispbuilder-sdl :ltk))

(in-package :nature)

;(load "/home/tcallahan/bottega/nature/nature-ltk.lisp")

(defun update-label (label text)
  (setf (text label) text))

(defun main ()
  (with-ltk ()
    (let* ((frame (make-instance 'frame :width 80 :height 140))
	   (button (make-instance 'button :master frame :text "Start"))
	   (label (make-instance 'label :master frame))
	   (label-fn (lambda (evt) (update-label label evt))))
      (setf (command button) (lambda () (setup label-fn)))
      (pack frame)
      (pack label)
      (pack button)
      (process-events))))

(defun setup (fn)
  (sdl:with-init ()
    (sdl:window 800 600 :title-caption "fun coding nature")
    (setf (sdl:frame-rate) 30)
    (draw)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
		       (if (sdl:key= key :sdl-key-escape) (sdl:push-quit-event)))
      (:idle ()
	     (when (sdl:mouse-left-p)
	       (funcall fn (sdl:mouse-x)))
	     (update-display)
	     (process-events)))))

(defun draw ()
  (let ((a-circle (draw-circle-* 100 100 25)))
    (update-display)))
