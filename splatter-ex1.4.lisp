(load "~/bottega/nature/nature-sdl.lisp")

;; Exercise I.4
;; Consider a simulation of paint splatter drawn as a collection of colored dots. Most of the paint clusters around a central location, but some dots do splatter out towards the edges. Can you use a normal distribution of random numbers to generate the locations of the dots? Can you also use a normal distribution of random numbers to generate a color palette?
;;

(in-package :nature)

(defparameter *counter* 0)

(defun setup ()
  (setf *counter* 0))  

(defun draw (&optional (n 5))
  (when (< *counter* 200)
    (dotimes (i n)
      (plot-splatter))
    (incf *counter*)))

(defun plot-splatter ()
  (let* ((xmean (/ *window-width* 2))
	 (ymean (/ *window-height* 2))
	 (rgbmean (/ 255 2))
	 (sdx 100)
	 (sdy 100)
	 (sdr 75)
	 (sdg 75)
	 (sdb 75)
	 (xloc (floor (+ (* (random-gauss) sdx) xmean)))
	 (yloc (floor (+ (* (random-gauss) sdy) ymean)))
	 (r (+ (* (random-gauss) sdr) rgbmean))
	 (g (+ (* (random-gauss) sdg) rgbmean))
	 (b (+ (* (random-gauss) sdb) rgbmean)))
    (draw-filled-circle-* xloc yloc 2
			  :color (sdl:color :r r :g g :b b)
			  :alpha 100)))



