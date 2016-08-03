(ql:quickload :ltk)

(defpackage :study
  (:use :cl :ltk))

(in-package :study)


(defparameter *window-width* 800)
(defparameter *window-height* 600)
(defparameter *canvas-width* 650)
(defparameter *canvas-height* 600)

(defun setup ())
(defun draw ())

(defun main ()
  (with-ltk ()
    (let* ((panel-frame (make-instance 'frame :width 150 :height 600))
	   (canvas-frame (make-instance 'frame :width 650 :height 600))
	   (start-button (make-instance 'button :master panel-frame :text "Start"))
	   (reset-button (make-instance 'button :master panel-frame :text "Reset"))
	   (label (make-instance 'label :master panel-frame))
	   (item-count-entry (make-instance 'entry :master panel-frame :text "3"))
	   (canvas (make-instance 'canvas
				  :master canvas-frame
				  :width *canvas-width*
				  :height *canvas-height*))
	   (label-fn (lambda (evt) (update-label label evt))))
      (setf (command start-button) (lambda () (draw label item-count-entry canvas)))
      (setf (command reset-button) (lambda () (setup canvas)))
      (pack panel-frame :side :left)
      (pack canvas-frame :side :right)
      (pack item-count-entry)
      (pack label)
      (pack start-button)
      (pack reset-button)
      (pack canvas :side :right)
      (process-events))))

(defun draw (label entry canv)
  (let ((bird-count (parse-integer (text entry)))
	(birds '())) 
    (setf (text label) (text entry))
    (setf (text entry) "")
    (dotimes (n bird-count)
      (push (bird (random 650) (random 600) (random 360) 25) birds))
    (dotimes (n 100)
      (setf birds 
	    (loop for bird in birds
	       do
		 (draw-bird bird canv)
	       collect
		 (update-position bird (fly bird))))
      (sleep .1)
      (setup canv))))

(defun bird (x y angle speed)
  (list x y angle speed))

(defun bird-* (&key (x 0) (y 0) (angle 0) (speed 0))
  (bird x y angle speed))

(defun draw-bird (bird canvas)
  (draw-arrow (x-coord (bird-coord bird))
	      (y-coord (bird-coord bird))
	      (bird-speed bird)
	      (bird-heading bird)
	      canvas))	       

(defun clone-bird (bird &key (x nil) (y nil) (angle nil) (speed nil))
  (let ((x (?? x (x-coord (bird-coord bird))))
	(y (?? y (y-coord (bird-coord bird))))
	(angle (?? angle (bird-heading bird)))
	(speed (?? speed (bird-speed bird))))))

(defmacro ?? (exp1 exp2)
  (let ((exp1-value exp1))
    `(if ,exp1-value ,exp1-value ,exp2)))
				  
(defun bird-coord (bird)
  (list (first bird) (second bird)))

(defun bird-heading (bird)
  (third bird))

(defun bird-speed (bird)
  (fourth bird))

(defun fly (bird)
  (reconcile (vector-addition (bird-coord bird) (bird-speed bird) (bird-heading bird))))

(defun update-position (bird coord)
  (bird (x-coord coord) (y-coord coord) (bird-heading bird) (bird-speed bird)))

(defun turn-bird (bird angle)
  (clone-bird bird :angle angle))

(defun draw-arrow (x y magnitude angle canv)
  (labels ((draw-line (x y magnitude angle)
	     (let ((x0 x)
		   (y0 y)
		   (x1 (x-coord (vector-addition (list x y) magnitude angle)))
		   (y1 (y-coord (vector-addition (list x y) magnitude angle))))
	       (create-line canv (list x0 y0 x1 y1))
	       (list x1 y1))))
    (let ((point (draw-line x y magnitude angle)))
      (draw-line (x-coord point) (y-coord point) 10 (+ angle 150))
      (draw-line (x-coord point) (y-coord point) 10 (- angle 150)))))

(defun rad (degrees)
  (* pi (/ degrees 180.0)))

(defun x-coord (point)
  (first point))

(defun y-coord (point)
  (second point))

(defun vector->cartesian (magnitude angle)
  (list (* (cos (rad angle)) magnitude)
	(* (sin (rad angle)) magnitude)))

(defun vector-addition (coord magnitude angle)
  (list (+ (x-coord coord) (x-coord (vector->cartesian magnitude angle)))
	(+ (y-coord coord) (y-coord (vector->cartesian magnitude angle)))))

(defun in-between (min max n)
  (cond ((< n min) (- max n))
	((> n max) (- n max))
	(t n)))

(defun reconcile (coord)
  (list (in-between 0 *canvas-width* (x-coord coord))
	(in-between 0 *canvas-height* (y-coord coord))))

(defun setup (canvas)
  (clear canvas))
