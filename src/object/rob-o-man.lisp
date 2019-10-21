(cl:in-package :post-man)

(defparameter *rob-o-man-free-speed* 3)
(defparameter *rob-o-man-carry-speed* 2)


(defclass rob-o-man (being)
  ((inventory :initform nil))
  (:default-initargs :speed *rob-o-man-free-speed*
                     :color (gamekit:vec4 0.1 0.4 0.1 1)))


(defmethod update ((this rob-o-man))
  (call-next-method)
  (with-slots (inventory) this
    (let ((next-direction (select-direction *gameplay*)))
      (if (or (not next-direction)
              (level-obstacle-exists *level*
                                     (gamekit:add (next-position-of this)
                                                  (direction->vector next-direction))))
          (move-being this nil)
          (move-being this next-direction)))
    (when inventory
      (update-position inventory (position-of this)))))


(defmethod render ((this rob-o-man))
  (with-slots (inventory) this
    (gamekit:with-pushed-canvas ()
      (when inventory
        (when-let ((direction (direction-of this)))
          (case direction
            (:up (gamekit:translate-canvas 0 10) (render inventory))))))
    (gamekit:with-pushed-canvas ()
      (translate-position (position-of this))
      (gamekit:translate-canvas -9 0)
      (gamekit:scale-canvas 0.5 0.5)
      (if-let ((direction (direction-of this)))
        (case direction
          (:up (gamekit:draw-image *origin* :rob-o-man-back))
          (:down (gamekit:draw-image *origin* :rob-o-man-front))
          (:left (gamekit:draw-image *origin* :rob-o-man-left))
          (:right (gamekit:draw-image *origin* :rob-o-man-right)))
        (gamekit:draw-image *origin* :rob-o-man-front)))
    (when inventory
      (if-let ((direction (direction-of this)))
        (case direction
          (:down (gamekit:translate-canvas 0 -2) (render inventory))
          (:left (gamekit:translate-canvas -4 0) (render inventory))
          (:right (gamekit:translate-canvas 4 0) (render inventory)))
        (render inventory)))))


(defmethod interact ((this rob-o-man) (object box))
  (with-slots (inventory) this
    (play-box-pick-up-sound)
    (setf inventory (remove-object *level* object))
    (update-speed this *rob-o-man-carry-speed*)
    (remove-renderable *gameplay* inventory)
    (pick-objective *level*)))


(defmethod interact ((this rob-o-man) (object rack))
  (with-slots (inventory) this
    (when (activatedp object)
      (play-box-drop-sound)
      (destroy inventory)
      (setf inventory nil)
      (update-speed this *rob-o-man-free-speed*)
      (deactivate object))))
