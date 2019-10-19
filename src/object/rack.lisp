(cl:in-package :post-man)


(defclass vertical-rack (positionable) ())


(defmethod obstacle-of ((this vertical-rack))
  '((0 . 0) (0 . 1)))


(defmethod render ((this vertical-rack))
  (gamekit:with-pushed-canvas ()
    (gamekit:translate-canvas (* (gamekit:x (position-of this)) *grid-cell-width*)
                              (* (gamekit:y (position-of this)) *grid-cell-width*))
    (gamekit:draw-rect *origin* *grid-cell-width* *grid-cell-width*
                       :fill-paint *foreground*)
    (gamekit:translate-canvas 0 *grid-cell-width*)
    (gamekit:draw-rect *origin* *grid-cell-width* *grid-cell-width*
                       :fill-paint *foreground*)))


(defclass horizontal-rack (positionable) ())


(defmethod obstacle-of ((this horizontal-rack))
  '((0 . 0) (1 . 0)))


(defmethod render ((this horizontal-rack))
  (gamekit:with-pushed-canvas ()
    (gamekit:translate-canvas (* (gamekit:x (position-of this)) *grid-cell-width*)
                              (* (gamekit:y (position-of this)) *grid-cell-width*))
    (gamekit:draw-rect *origin* *grid-cell-width* *grid-cell-width*
                       :fill-paint *foreground*)
    (gamekit:translate-canvas *grid-cell-width* 0)
    (gamekit:draw-rect *origin* *grid-cell-width* *grid-cell-width*
                       :fill-paint *foreground*)))
