(cl:in-package :post-man)

(defparameter *grid-cell-width* 32)
(defparameter *grid-size* 20)


(defun fill-obstacle-map (map)
  (loop repeat *grid-size*
        do (setf (gethash (cons (random *grid-size*) (random *grid-size*)) map) t))
  map)


(defclass level ()
  ((obstacle-map :initform (fill-obstacle-map (make-hash-table :test 'equal)))))


(defmethod render ((this level))
  (with-slots (obstacle-map) this
    (loop for x from 0 below *grid-size*
          do (loop for y from 0 below *grid-size*
                   if (gethash (cons x y) obstacle-map)
                     do (gamekit:draw-rect (gamekit:vec2 (* x *grid-cell-width*)
                                                         (* y *grid-cell-width*))
                                           *grid-cell-width* *grid-cell-width*
                                           :fill-paint *foreground*
                                           :stroke-paint *foreground*)
                   else
                     do (gamekit:draw-rect (gamekit:vec2 (* x *grid-cell-width*)
                                                         (* y *grid-cell-width*))
                                           *grid-cell-width* *grid-cell-width*
                                           :stroke-paint *foreground*)))))
