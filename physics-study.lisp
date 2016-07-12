(ql:quickload :ltk)

(defpackage :study
  (:use :cl :ltk))

(in-package :study)


(defparameter *window-width* 800)
(defparameter *window-height* 600)

(defun setup ())
(defun draw ())

(defun main ()
  (with-ltk ()
    (let* ((panel-frame (make-instance 'frame :width 150 :height 600))
	   (canvas-frame (make-instance 'frame :width 650 :height 600))
	   (start-button (make-instance 'button :master panel-frame :text "Start"))
	   (reset-button (make-instance 'button :master panel-frame :text "Reset"))
	   (label (make-instance 'label :master panel-frame))
	   (item-count-entry (make-instance 'entry :master panel-frame))
	   (canvas (make-instance 'canvas :master canvas-frame :width 650 :height 600))
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

(defun draw (label text canv)
  (setf (text label) (text text))
  (setf (text text) "")
  (draw-arrow 100 100 canv))

(defun draw-arrow (x y canv)
  (create-line canv (list 100 100 120 120))
  (create-line canv (list 120 110 120 120))
  (create-line canv (list 110 120 120 120)))

(defun setup (canvas)
  (clear canvas))
