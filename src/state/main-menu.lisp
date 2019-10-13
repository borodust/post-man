(cl:in-package :post-statics)


(defclass main-menu-state () ())


(defmethod gamekit:draw ((this main-menu-state))
  (ge.vg:antialias-shapes nil)
  (gamekit:with-pushed-canvas ()
    (gamekit:scale-canvas 2 2)
    (gamekit:translate-canvas 32 90)
    (gamekit:draw-text "Post Statics" (gamekit:vec2 0 0)))
  (gamekit:with-pushed-canvas ()
    (gamekit:translate-canvas 100 70)
    (gamekit:draw-text "> Start" (gamekit:vec2 0 30))
    (gamekit:draw-text "  Settings" (gamekit:vec2 0 15))
    (gamekit:draw-text "  Exit" (gamekit:vec2 0 0))))
