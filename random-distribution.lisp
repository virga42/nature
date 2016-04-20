(load "~/bottega/nature/nature-sdl.lisp")

(in-package :nature)

(defparameter *size* 10)
(defparameter *random-set* (make-array *size* :initial-element 0))
(defparameter *counter* 0)

(defun setup ()
  (setf *random-set* (fill *random-set* 0))
  (setf *counter* 0))  

(defun draw (&optional (n 5))
  (when (< *counter* 1000)
    (dotimes (i n)
      (incf (elt *random-set* (random-number-generator))))
      ;; (print *random-set*)
      (clear-display *black*)
      (render-bars *random-set*))
    (setf *counter* (1+ *counter*)))

(defun random-number-generator ()
  (random *size*))

;;; update-nth loop-style
;; (defun update-nth (n list)
;;   (loop for i from 0 for j in list collect (if (= i n) (1+ j) j)))

;;; update-nth recursive-style
;; (defun update-nth (n list)
;;   (if (> n 0)
;;       (cons (car list) (update-nth (1- n) (cdr list)))
;;       (cons (1+ (car list)) (cdr list))))

(defun render-bars (set)
  (let* ((width (/ *window-width* *size*))
	 (random-set-list (coerce *random-set* 'list)))
  (loop for i from 0 for j in random-set-list do
       (draw-rectangle-* (* i width) (- *window-height* j) width j :color *blue*))))

(defun standard-deviation (list)
  (let* ((n (length list))
	 mean)
    (cond
      ((= n 0) nil)
      ((= n 1) 0)
      (t (progn
	   (setf mean (/ (reduce #'+ list) n))
	   (sqrt (/ (reduce #'+
			    (mapcar #'(lambda (x) (expt (- x mean) 2))
				    list))
		    n)))))))

;; Generate 12 numbers from 0->1
;; Sum them
;; Subtract 6

(defun random-gauss ()
  )
