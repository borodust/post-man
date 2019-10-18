(cl:in-package :post-man)


(defparameter *rob-o-man-speed* 75)


(defclass rob-o-man (movable updatable bounded) ()
  (:default-initargs :speed *rob-o-man-speed*
                     :bound (gamekit:vec2 (- *grid-cell-width* 4)
                                          (- *grid-cell-width* 4))))


(defmethod render ((this rob-o-man))
  (gamekit:draw-rect (position-of this) *grid-cell-width* (* *grid-cell-width* 2)
                     :fill-paint (gamekit:vec4 0.1 0.4 0.1 1)))
