(cl:in-package :post-man)





(defclass gameplay-state (input-handling-state)
  ((level :initform (make-instance 'level))
   (bogdans :initform (list (make-instance 'bogdan)))
   (rob-o-man :initform (make-instance 'rob-o-man))))


(defmethod gamekit:draw ((this gameplay-state))
  (with-slots (level bogdans rob-o-man) this
    (bodge-canvas:clear-buffers *background*)
    (render level)
    (loop for bogdan in bogdans
          do (render bogdan))
    (gamekit:with-pushed-canvas ()
      (gamekit:translate-canvas 200 0)
      (render rob-o-man))))


(defun pause-game ()
  (gamekit.fistmachine:transition-to 'main-menu-state))


(defmethod gamekit.input-handler:button-pressed ((this gameplay-state)
                                                 (button (eql :gamepad-start)))
  (pause-game))


(defmethod gamekit.input-handler:button-pressed ((this gameplay-state)
                                                 (button (eql :escape)))
  (pause-game))
