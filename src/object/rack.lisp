(cl:in-package :post-man)


(defclass rack (positionable objective)
  ((activated :initform nil :reader activatedp)))


(defmethod activate ((this rack))
  (with-slots (activated) this
    (setf activated t)))


(defmethod deactivate ((this rack))
  (with-slots (activated) this
    (setf activated nil)))

(defgeneric %rack-block-translate (rack))

(defmethod render ((this rack))
  (gamekit:with-pushed-canvas ()
    (let ((color (if (activatedp this)
                     (gamekit:vec4 0.4 0.4 0.9 1)
                     *foreground*)))
      (flet ((%render-block ()
               (gamekit:draw-rect *origin* *grid-cell-width* *grid-cell-width*
                                  :fill-paint color)))
        (gamekit:translate-canvas (* (gamekit:x (position-of this)) *grid-cell-width*)
                                  (* (gamekit:y (position-of this)) *grid-cell-width*))
        (%render-block)
        (%rack-block-translate this)
        (%render-block)))))

(defclass vertical-rack (rack) ())


(defmethod obstacle-of ((this vertical-rack))
  '((0 . 0) (0 . 1)))

(defmethod %rack-block-translate ((this vertical-rack))
  (gamekit:translate-canvas 0 *grid-cell-width*))



(defclass horizontal-rack (rack) ())


(defmethod obstacle-of ((this horizontal-rack))
  '((0 . 0) (1 . 0)))

(defmethod %rack-block-translate ((this horizontal-rack))
  (gamekit:translate-canvas *grid-cell-width* 0))
