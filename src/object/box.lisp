(cl:in-package :post-man)


(defclass box (positionable renderable) ())


(defmethod render ((this box))
  (gamekit:with-pushed-canvas ()
    (translate-position (position-of this))
    (gamekit:scale-canvas 0.5 0.5)
    (gamekit:draw-image *origin* :box)))


(defun play-box-pick-up-sound ()
  (gamekit:play-sound :box-pick-up))


(defun play-box-drop-sound ()
  (gamekit:play-sound :box-drop))
