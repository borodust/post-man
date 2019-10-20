(cl:in-package :post-man)


(defclass endgame-state (input-handling-state)
  ((boxes-collected :initform (error ":boxes-collected missing")
                    :initarg :boxes-collected)
   (seconds-spent :initform (error ":seconds-spent missing")
                  :initarg :seconds-spent)
   (level :initform (error ":level missing")
          :initarg :level)
   (stat-font :initform (gamekit:make-font :retro 36))
   (continue-font :initform (gamekit:make-font :retro 42))))


(defmethod gamekit:post-initialize ((this endgame-state)))


(defmethod gamekit:draw ((this endgame-state))
  (with-slots (boxes-collected seconds-spent level
               stat-font continue-font)
      this
    (bodge-canvas:clear-buffers *background*)
    (draw-splash)
    (gamekit:with-pushed-canvas ()
      (gamekit:translate-canvas (- (/ (gamekit:viewport-width) 2) 190) 180)
      (gamekit:draw-text (format nil "Level:~27T~D" level)
                         *origin*
                         :font stat-font
                         :fill-color *foreground*)
      (gamekit:translate-canvas 0 40)
      (gamekit:draw-text (format nil "Time spent:~23T~D:~2,'0d"
                                 (truncate (/ seconds-spent 60))
                                 (mod (truncate seconds-spent) 60))
                         *origin*
                         :font stat-font
                         :fill-color *foreground*)
      (gamekit:translate-canvas 0 40)
      (gamekit:draw-text (format nil "Boxes placed:~20T~D" boxes-collected) *origin*
                         :font stat-font
                         :fill-color *foreground*))
    (gamekit:with-pushed-canvas ()
      (gamekit:translate-canvas (- (/ (gamekit:viewport-width) 2) 230)
                                80)
      (gamekit:draw-text "PRESS ENTER OR START" *origin*
                         :font continue-font
                         :fill-color *foreground*))))


(defun go-to-menu ()
  (gamekit.fistmachine:transition-to 'main-menu-state))


(defmethod gamekit.input-handler:button-pressed ((this endgame-state)
                                                 (button (eql :enter)))
  (go-to-menu))


(defmethod gamekit.input-handler:button-pressed ((this endgame-state)
                                                 (button (eql :gamepad-start)))
  (go-to-menu))
