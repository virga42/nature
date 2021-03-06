(ql:quickload :ltk)

(in-package :ltk)

(defun main ()
  (with-ltk ()
    (let* ((frame (make-instance 'frame :width 80 :height 140))
	   (button (make-instance 'button :master frame :text "Start")))
      (setf (command button) (lambda () (setup)))
      (pack frame)
      (pack button))))
