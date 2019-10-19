(cl:in-package :post-man)


(defparameter *rob-o-man-speed* 75)

(defvar *directions* '((:forward . :backward)
                       (:backward . :forward)
                       (:left . :right)
                       (:right . :left)))


(defclass rob-o-man (updatable positionable)
  ((direction :initform nil)
   (next-position :initform (gamekit:vec2 0 0) :reader next-position-of)))


(defun vec= (this that)
  (and (< (abs (- (gamekit:x this) (gamekit:x that))) single-float-epsilon)
       (< (abs (- (gamekit:y this) (gamekit:y that))) single-float-epsilon)))


(defun direction->vector (direction)
  (ecase direction
    (:forward (gamekit:vec2 0 1))
    (:backward (gamekit:vec2 0 -1))
    (:left (gamekit:vec2 -1 0))
    (:right (gamekit:vec2 1 0))))


(defun move-rob-o-man (rob-o-man next-direction)
  (with-slots (direction next-position) rob-o-man
    (when (and next-direction
               (or (eq direction (alexandria:assoc-value *directions* next-direction))
                   (not direction)))
      (setf direction next-direction
            next-position (gamekit:add next-position
                                       (direction->vector next-direction))))))


(defmethod render ((this rob-o-man))
  (with-slots (direction) this
    (gamekit:with-pushed-canvas ()
      (gamekit:translate-canvas (* (gamekit:x (position-of this)) *grid-cell-width*)
                                (* (gamekit:y (position-of this)) *grid-cell-width*))
      (gamekit:draw-rect *origin*
                         *grid-cell-width* *grid-cell-width*
                         :fill-paint (gamekit:vec4 0.1 0.4 0.1 1))
      (case direction
        (:forward (gamekit:translate-canvas 0 10))
        (:backward (gamekit:translate-canvas 0 -10))
        (:left (gamekit:translate-canvas -10 0))
        (:right (gamekit:translate-canvas 10 0)))
      (gamekit:draw-circle (gamekit:vec2 12 22) 3
                           :fill-paint *foreground*)
      (gamekit:draw-circle (gamekit:vec2 20 22) 3
                           :fill-paint *foreground*)
      (gamekit:draw-rect (gamekit:vec2 7 6) 18 8
                         :fill-paint *foreground*))))


(defmethod update ((this rob-o-man))
  (with-slots (direction next-position) this
    (when direction
      (let ((vector (gamekit:subt next-position (position-of this))))
        (if (>= 0 (bodge-math:dot (direction->vector direction) vector))
            (progn
              (setf direction nil)
              (update-position this next-position))
            (let ((path-delta (ge.ng:mult (ge.ng:normalize vector)
                                          (* *update-delta-time* 2))))
              (update-position this (gamekit:add (position-of this) path-delta))))))))
