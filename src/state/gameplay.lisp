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


(defun select-direction (button-bag)
  (flet ((%select (direction &rest buttons)
           (when (loop for button in buttons
                         thereis (member button button-bag))
             direction)))
    (or (%select :forward :w :up :gamepad-up)
        (%select :left :a :left :gamepad-left)
        (%select :backward :s :down :gamepad-down)
        (%select :right :d :right :gamepad-right))))


(defmethod gamekit:act ((this gameplay-state))
  (with-slots (rob-o-man level) this
    (update rob-o-man)
    (let ((next-direction (select-direction
                           (gamekit.input-handler:pressed-buttons this))))
      (if (or (not next-direction)
              (level-obstacle-exists level
                                     (gamekit:add (next-position-of rob-o-man)
                                                  (direction->vector next-direction))))
          (move-rob-o-man rob-o-man nil)
          (move-rob-o-man rob-o-man next-direction)))))


(defun pause-game ()
  (gamekit.fistmachine:transition-to 'main-menu-state))


(defmethod gamekit.input-handler:button-pressed ((this gameplay-state)
                                                 (button (eql :gamepad-start)))
  (pause-game))


(defmethod gamekit.input-handler:button-pressed ((this gameplay-state)
                                                 (button (eql :escape)))
  (pause-game))
