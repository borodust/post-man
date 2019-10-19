(cl:in-package :post-man)


(defclass box (positionable) ())


(defmethod render ((this box))
  (gamekit:with-pushed-canvas ()
    (gamekit:translate-canvas (* (gamekit:x (position-of this)) *grid-cell-width*)
                              (* (gamekit:y (position-of this)) *grid-cell-width*))
    (gamekit:draw-rect (gamekit:vec2 8 8) 16 16
                       :fill-paint (gamekit:vec4 0.8 0.8 0.1 1))))
