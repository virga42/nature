(load "~/bottega/nature/nature-sdl.lisp")

;; Demonstrate random distribution
;; Uniform and Gaussian (Normal)

(in-package :nature)

(defparameter *size* 10)
(defparameter *random-set* (make-array *size* :initial-element 0))
(defparameter *counter* 0)

(defun setup ()
  (setf *random-set* (fill *random-set* 0))
  (setf *counter* 0))  

(defun draw (&optional (n 5))
  (when (< *counter* 200)
    (dotimes (i n)
      (incf (elt *random-set* (floor (random-number-generator))))
      (clear-display *black*)
      (render-bars *random-set*))
    (incf *counter*)))

;; (defun random-number-generator ()
;;   (random 10))

(defun random-number-generator ()
  (+ 5 (* 5 (random-gauss))))

(defun render-bars (set)
  (let* ((width (floor (/ *window-width* *size*)))
	 (random-set-list (coerce *random-set* 'list)))
  (loop for i from 0 for j in random-set-list do
       (draw-rectangle-* (* i width) (- *window-height* j) width j :color *blue*))))

