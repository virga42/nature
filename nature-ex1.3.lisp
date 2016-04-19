(load "~/bottega/nature/nature-sdl.lisp")

(in-package :nature)

(defparameter *walkers* '())

(defun setup ()
  (setf *walkers* '())
  (dotimes (i 5)
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
  (let ((hue (random 360))
	(x (/ *window-width* 2))
	(y (/ *window-height* 2)))
    (lambda (message)
      (cond
	((eq message 'update)
	 (progn
	   (setf x (+ x (random-elt horiz-probability-set)))
	   (setf y (+ y (random-elt vert-probability-set)))))
	((eq message 'draw)
	 (let* ((rgb (hsl->rgb hue (saturation-selector (distance-from-origin x y)) 1))
	       (r (first rgb))
	       (g (second rgb))
	       (b (third rgb)))
	 (draw-pixel-* x y :color (sdl:color :r r :g g :b b))))))))

(defun distance-from-origin (x y)
  (let ((ox (/ *window-width* 2))
	(oy (/ *window-height* 2)))
    (sqrt (+ (* (- y oy) (- y oy)) (* (- x ox) (- x ox))))))

(defun saturation-selector (distance)
  (let ((limit (/ *window-height* 2.0)))
    (if (>= distance limit)
	1
	(/ distance limit))))
  
(defun hsl->rgb (h s v)
  (if (null h)
      '(0 0 0)
      (let* ((c (* v s))
	     (h-prime (/ h 60))
	     (x (* c (- 1 (abs (- 1 (mod h-prime 2))))))
	     (rgb-prime	
	      (cond
		((< h-prime 1) (list c x 0))
		((< h-prime 2) (list x c 0))
		((< h-prime 3) (list 0 c x))
		((< h-prime 4) (list 0 x c))
		((< h-prime 5) (list x 0 c))
		((< h-prime 6) (list c 0 x))))
	     (m (- v c)))
	(mapcar (lambda(e)(floor (* 255 (+ e m)))) rgb-prime)))) 
