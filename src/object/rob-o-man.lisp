(cl:in-package :post-man)


(defparameter *rob-o-man-speed* 100)


(defclass rob-o-man (movable updatable) ()
  (:default-initargs :speed *rob-o-man-speed*))


(defmethod render ((this rob-o-man))
  (gamekit:draw-rect (position-of this) 50 100 :fill-paint (gamekit:vec4 0.1 0.4 0.1 1)))
