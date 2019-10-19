(cl:in-package :post-man)


(defclass rob-o-man (being)
  ((inventory :initform nil))
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
  (with-slots (inventory) this
    (let ((next-direction (select-direction
                           (gamekit.input-handler:pressed-buttons *gameplay*))))
      (if (or (not next-direction)
              (level-obstacle-exists *level*
                                     (gamekit:add (next-position-of this)
                                                  (direction->vector next-direction))))
          (move-being this nil)
          (move-being this next-direction)))
    (when inventory
      (update-position inventory (position-of this)))))


(defmethod render ((this rob-o-man))
  (call-next-method)
  (with-slots (inventory) this
    (when inventory
      (gamekit:translate-canvas 0 -10)
      (when-let ((direction (direction-of this)))
        (case direction
          (:up (gamekit:translate-canvas 0 10))
          (:down (gamekit:translate-canvas 0 -10))
          (:left (gamekit:translate-canvas -10 0))
          (:right (gamekit:translate-canvas 10 0))))
      (render inventory))))


(defmethod interact ((this rob-o-man) (object box))
  (with-slots (inventory) this
    (setf inventory (remove-object *level* object))
    (pick-objective *level*)))


(defmethod interact ((this rob-o-man) (object rack))
  (with-slots (inventory) this
    (when (activatedp object)
      (setf inventory nil)
      (deactivate object)
      (spawn-box *level*))))
