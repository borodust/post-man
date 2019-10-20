(cl:in-package :post-man)


(defvar *directions* '((:up . :down)
                       (:down . :up)
                       (:left . :right)
                       (:right . :left)))


(defclass being (updatable positionable renderable)
  ((direction :initform nil :reader direction-of)
   (next-position :reader next-position-of)
   (color :initform (gamekit:vec4 0.5 0.5 0.5 1) :initarg :color)
   (speed :initform 1 :initarg :speed)))


(defmethod initialize-instance :after ((this being) &key)
  (with-slots (next-position) this
    (setf next-position (position-of this))))

(defun direction->vector (direction)
  (ecase direction
    (:up (gamekit:vec2 0 1))
    (:down (gamekit:vec2 0 -1))
    (:left (gamekit:vec2 -1 0))
    (:right (gamekit:vec2 1 0))))


(defun move-being (being next-direction)
  (with-slots (direction next-position) being
    (when (and next-direction
               (or (eq direction (alexandria:assoc-value *directions* next-direction))
                   (not direction)))
      (setf direction next-direction
            next-position (gamekit:add next-position
                                       (direction->vector next-direction))))))


(defmethod render ((this being))
  (with-slots (direction color) this
    (gamekit:with-pushed-canvas ()
      (translate-position (position-of this))
      (gamekit:draw-rect *origin*
                         *grid-cell-width* *grid-cell-width*
                         :fill-paint color)
      (case direction
        (:up (gamekit:translate-canvas 0 10))
        (:down (gamekit:translate-canvas 0 -10))
        (:left (gamekit:translate-canvas -10 0))
        (:right (gamekit:translate-canvas 10 0)))
      (gamekit:draw-circle (gamekit:vec2 12 22) 3
                           :fill-paint *foreground*)
      (gamekit:draw-circle (gamekit:vec2 20 22) 3
                           :fill-paint *foreground*)
      (gamekit:draw-rect (gamekit:vec2 7 6) 18 8
                         :fill-paint *foreground*))))


(defmethod update ((this being))
  (with-slots (direction next-position speed) this
    (call-next-method)
    (when direction
      (let ((vector (gamekit:subt next-position (position-of this))))
        (if (>= 0 (bodge-math:dot (direction->vector direction) vector))
            (progn
              (setf direction nil)
              (update-position this next-position))
            (let ((path-delta (ge.ng:mult (ge.ng:normalize vector)
                                          (* *update-delta-time* speed))))
              (update-position this (gamekit:add (position-of this) path-delta))))))))
