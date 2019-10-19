(cl:in-package :post-man)


(defclass rob-o-man (being) ()
  (:default-initargs :speed 2 :color (gamekit:vec4 0.1 0.4 0.1 1)))


(defun select-direction (button-bag)
  (flet ((%select (direction &rest buttons)
           (when (loop for button in buttons
                         thereis (member button button-bag))
             direction)))
    (or (%select :up :w :up :gamepad-up)
        (%select :left :a :left :gamepad-left)
        (%select :down :s :down :gamepad-down)
        (%select :right :d :right :gamepad-right))))


(defmethod update ((this rob-o-man))
  (call-next-method)
  (let ((next-direction (select-direction
                         (gamekit.input-handler:pressed-buttons *gameplay*))))
    (if (or (not next-direction)
            (level-obstacle-exists *level*
                                   (gamekit:add (next-position-of this)
                                                (direction->vector next-direction))))
        (move-being this nil)
        (move-being this next-direction))))
