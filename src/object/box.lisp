(cl:in-package :post-man)


(defclass box (positionable renderable) ())


(defmethod render ((this box))
  (gamekit:with-pushed-canvas ()
    (translate-position (position-of this))
    (gamekit:scale-canvas 0.5 0.5)
    (gamekit:draw-image *origin* :box)))
