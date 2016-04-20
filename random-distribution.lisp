(load "~/bottega/nature/nature-sdl.lisp")

(in-package :nature)

(defparameter *size* 10)
(defparameter *random-set* (make-list *size* :initial-element 0))
(defparameter *counter* 0)

(defun setup ()
  (setf *random-set* (make-list (length *random-set*) :initial-element 0))
  (setf *counter* 0))  

(defun draw (&optional (n 5))
  (when (< *counter* 100)
    (dotimes (i n)
      (setf *random-set* (update-nth (random-number-generator) *random-set*))
      ;; (print *random-set*)
      (clear-display *black*)
      (render-bars *random-set*))
    (setf *counter* (1+ *counter*))))

(defun random-number-generator ()
  (random *size*))

;;; update-nth loop-style
;; (defun update-nth (n list)
;;   (loop for i from 0 for j in list collect (if (= i n) (1+ j) j)))

;;; update-nth recursive-style
(defun update-nth (n list)
  (if (> n 0)
      (cons (car list) (update-nth (1- n) (cdr list)))
      (cons (1+ (car list)) (cdr list))))

(defun render-bars (set)
  (loop for i from 0 for j in set do
       (draw-rectangle-* (* i 10) 0 10 j :color *blue*)))

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
