(cl:in-package :post-man)


(defclass rob-o-man () ())


(defmethod render ((this rob-o-man))
  (gamekit:draw-rect *origin* 50 100 :fill-paint (gamekit:vec4 0.1 0.4 0.1 1)))
