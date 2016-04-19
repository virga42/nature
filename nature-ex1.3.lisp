(load "~/Documents/lisp/nature/nature/nature-sdl.lisp")

(in-package :nature)

(defparameter *walkers* '())

(defun setup ()
  (setf *walkers* '())
  (dotimes (i 10)
    (setf *walkers* (cons (walker) *walkers*))))

(defun draw ()
  (loop for walker in *walkers* do
       (dotimes (i 5)
	 (funcall walker 'update)
	 (funcall walker 'draw))))

(defparameter walker-x (/ *window-width* 2))
(defparameter walker-y (/ *window-height* 2))

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

(defun walker ()
  (let ((color (sdl:color :r (random 255) :g (random 255) :b (random 255)))
	(x (/ *window-width* 2))
	(y (/ *window-height* 2)))
    (lambda (message)
      (cond
	((eq message 'update)
	 (progn
	   (setf x (+ x (random-elt horiz-probability-set)))
	   (setf y (+ y (random-elt vert-probability-set)))))
	((eq message 'draw)
	 (setf (alpha color
	 (draw-pixel-* x y :color color :a (alpha-helper (distance-from-origin x y))))))))

(defun distance-from-origin (x y)
  (let ((ox (/ *window-width* 2))
	(oy (/ *window-height* 2)))
    (sqrt (+ (* (- y oy) (- y oy)) (* (- x ox) (- x ox))))))

(defun alpha-helper (distance)
  (cond
    ((< distance 100) 60)
    ((< distance 200) 120)
    ((< distance 300) 180)
    (t 255)))
  
