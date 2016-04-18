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

(defparameter walker-x 400)
(defparameter walker-y 300)

(defun setup (fn)
  (sdl:with-init ()
    (sdl:window 800 600 :title-caption "fun coding nature")
    (setf (sdl:frame-rate) 30)
    ;; (random-walk)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
		       (if (sdl:key= key :sdl-key-escape) (sdl:push-quit-event)))
      (:mouse-button-down-event (:x x :y y)
				(on-mouse-down fn x y))
      (:mouse-motion-event (:x mouse-x :y mouse-y)
			   (progn
			     (update-walk-probability (mouse-direction (sdl:mouse-relative-position)))
			     (random-walk)))
      (:idle ()

	     (update-display)
	     (process-events)))))

(defun on-mouse-down (fn x y)
  (funcall fn (format nil "X: ~a Y: ~a Rel: ~a" x y (sdl:mouse-relative-position))))  

(defun random-walk ()
      (setf walker-x (+ walker-x (random-elt horiz-probability-set)))
      (setf walker-y (+ walker-y (random-elt vert-probability-set)))
      (draw-pixel-* walker-x walker-y))

(defparameter horiz-probability-set (list -1 0 1))
(defparameter vert-probability-set (list -1 0 1))

(defun update-walk-probability (direction)
  (reset-probability-sets)
  (cond
    ((eq direction 'left) (setf horiz-probability-set (list -1 -1 0 1)))
    ((eq direction 'right) (setf horiz-probability-set (list -1 0 1 1)))
    ((eq direction 'up) (setf vert-probability-set (list -1 -1 0 1)))
    ((eq direction 'down) (setf vert-probability-set (list -1 0 1 1)))))

(defun reset-probability-sets ()
  (setf horiz-probability-set (list -1 0 1))
  (setf vert-probability-set (list -1 0 1)))

(defun random-elt (choices)
  (elt choices (random (length choices))))

(defun mouse-direction (mouse-movement-delta)
  (let ((x-delta (elt mouse-movement-delta 0))
	(y-delta (elt mouse-movement-delta 1)))
    (cond ((and (> (abs x-delta) (abs y-delta)) (> x-delta 0)) 'right)
	  ((and (> (abs x-delta) (abs y-delta)) (< x-delta 0)) 'left)
	  ((and (< (abs x-delta) (abs y-delta)) (> y-delta 0)) 'down)
	  (t 'up))))
