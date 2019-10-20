(cl:in-package :post-man)


(defclass rack (positionable objective renderable)
  ((activated :initform nil :reader activatedp)))


(defmethod activate ((this rack))
  (with-slots (activated) this
    (setf activated t)))


(defmethod deactivate ((this rack))
  (with-slots (activated) this
    (setf activated nil)))


(defgeneric %render-rack (rack))


(defgeneric %render-active-rack (rack))


(defmethod render ((this rack))
  (translate-position (position-of this))
  (gamekit:scale-canvas 0.5 0.5)
  (if (activatedp this)
      (%render-active-rack this)
      (%render-rack this)))


(defclass vertical-rack (rack) ())


(defmethod obstacle-of ((this vertical-rack))
  '((0 . 0) (0 . 1)))


(defmethod %render-active-rack ((this vertical-rack))
  (gamekit:draw-image *origin* :vertical-rack-active))


(defmethod %render-rack ((this vertical-rack))
  (gamekit:draw-image *origin* :vertical-rack))



(defclass horizontal-rack (rack) ())


(defmethod obstacle-of ((this horizontal-rack))
  '((0 . 0) (1 . 0)))


(defmethod %render-active-rack ((this horizontal-rack))
  (gamekit:draw-image *origin* :horizontal-rack-active))


(defmethod %render-rack ((this horizontal-rack))
  (gamekit:draw-image *origin* :horizontal-rack))
