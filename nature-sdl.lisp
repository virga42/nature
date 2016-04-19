(ql:quickload :ltk)
(ql:quickload :lispbuilder-sdl)

(defpackage :nature
  (:use :cl :lispbuilder-sdl :ltk))

(in-package :nature)

;(load "/home/tcallahan/bottega/nature/nature-ltk.lisp")

(defparameter *window-width* 800)
(defparameter *window-height* 600)
(defparameter *fps* 30)
(defparameter *mouse-x* 0)
(defparameter *mouse-y* 0)

(defun setup ())
(defun draw ())

(defun main ()
  (with-ltk ()
    (let* ((frame (make-instance 'frame :width 80 :height 140))
	   (button (make-instance 'button :master frame :text "Start"))
	   (label (make-instance 'label :master frame))
	   (label-fn (lambda (evt) (update-label label evt))))
      (setf (command button) (lambda () (main-sdl label-fn)))
      (pack frame)
      (pack label)
      (pack button)
      (process-events))))

(defun update-label (label text)
  (setf (text label) text))


(defun main-sdl (fn)
  (sdl:with-init ()
    (sdl:window *window-width* *window-height* :title-caption "fun coding nature")
    (setf (sdl:frame-rate) *fps*)
    (setup)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
		       (if (sdl:key= key :sdl-key-escape) (sdl:push-quit-event)))
      (:mouse-button-down-event (:x x :y y)
				(progn
				  (on-mouse-down fn x y)
				  (setf *mouse-x* x)
				  (setf *mouse-y* y)))
      (:mouse-button-up-event (:x x :y y)
			      (update-walk-probability (mouse-direction (list (- x *mouse-x*) (- y *mouse-y*))))) 
      (:idle ()
	     (draw)
	     (update-display)
	     (process-events)))))

(defun on-mouse-down (fn x y)
  (funcall fn (format nil "X: ~a Y: ~a Rel: ~a" x y (sdl:mouse-relative-position))))  

