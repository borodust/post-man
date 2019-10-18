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
      (render rob-o-man))))


(defun calc-movement-vector (button-bag)
  (let ((movement-vector (gamekit:vec2 0 0)))
    (flet ((%update-if (vector &rest buttons)
             (when (loop for button in buttons
                           thereis (member button button-bag))
               (setf movement-vector (gamekit:add movement-vector vector)))))
      (%update-if (gamekit:vec2 0 1) :w :up :gamepad-up)
      (%update-if (gamekit:vec2 -1 0) :a :left :gamepad-left)
      (%update-if (gamekit:vec2 0 -1) :s :down :gamepad-down)
      (%update-if (gamekit:vec2 1 0) :d :right :gamepad-right))
    (bodge-math:normalize movement-vector)))


(defmethod gamekit:act ((this gameplay-state))
  (with-slots (rob-o-man level) this
    (move-object rob-o-man (calc-movement-vector
                            (gamekit.input-handler:pressed-buttons this)))
    (let ((position (next-position rob-o-man)))
      (unless (level-collide level position (bound-of rob-o-man))
        (update-position rob-o-man position)))
    (update rob-o-man)))


(defun pause-game ()
  (gamekit.fistmachine:transition-to 'main-menu-state))


(defmethod gamekit.input-handler:button-pressed ((this gameplay-state)
                                                 (button (eql :gamepad-start)))
  (pause-game))


(defmethod gamekit.input-handler:button-pressed ((this gameplay-state)
                                                 (button (eql :escape)))
  (pause-game))
