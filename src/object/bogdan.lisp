(cl:in-package :post-man)


(defclass bogdan () ())


(defmethod render ((this bogdan))
  (gamekit:draw-rect *origin* *grid-cell-width* *grid-cell-width*
                     :fill-paint (gamekit:vec4 0.4 0.1 0.1 1)))
