(cl:in-package :post-man)


(defparameter *grid-size* 32)

(defclass gameplay-state (input-handling-state) ())


(defmethod gamekit:draw ((this gameplay-state))
  (with-slots () this
    (bodge-canvas:clear-buffers *background*)
    (bodge-canvas:translate-canvas 400 400)
    (gamekit:with-pushed-canvas ()
      (let* ((factor (* 0.5 (bodge-util:real-time-seconds)))
             (clamped (abs (- (mod factor 2) 1))))
        (bodge-canvas:rotate-canvas factor)
        (bodge-canvas:translate-canvas -400 -400)
        (bodge-canvas:skew-canvas 0.5 0.5)
        (bodge-canvas:scale-canvas clamped clamped))
      (loop for x from 0 below 32
            do (loop for y from 0 below 32
                     do (gamekit:draw-rect (gamekit:vec2 (* x *grid-size*)
                                                         (* y *grid-size*))
                                           *grid-size* *grid-size*
                                           :stroke-paint *foreground*))))))


(defun pause-game ()
  (gamekit.fistmachine:transition-to 'main-menu-state))


(defmethod gamekit.input-handler:button-pressed ((this gameplay-state)
                                                 (button (eql :gamepad-start)))
  (pause-game))


(defmethod gamekit.input-handler:button-pressed ((this gameplay-state)
                                                 (button (eql :escape)))
  (pause-game))
