(cl:in-package :post-man)

(defun fill-obstacle-map (map)
  (loop repeat *grid-size*
        do (let* ((grid-x (random *grid-size*))
                  (grid-y (random *grid-size*))
                  (obstacle (make-instance 'obstacle
                                           :position (gamekit:vec2 (* grid-x
                                                                      *grid-cell-width*)
                                                                   (* grid-y
                                                                      *grid-cell-width*))
                                           :bound (gamekit:vec2 *grid-cell-width*
                                                                *grid-cell-width*))))
             (setf (gethash (cons grid-x grid-y) map) obstacle)))
  map)


(defclass level ()
  ((obstacle-map :initform (fill-obstacle-map (make-hash-table :test 'equal)))))


(defun find-adjascent-cells (level position)
  (with-slots (obstacle-map) level
    (let* ((grid-x (truncate (/ (gamekit:x position) *grid-cell-width*)))
           (grid-y (truncate (/ (gamekit:y position) *grid-cell-width*))))
      (flet ((%get (x y)
               (gethash (cons x y) obstacle-map)))
        (remove-if #'null (list (%get grid-x grid-y)
                                (%get (1+ grid-x) grid-y)
                                (%get (1+ grid-x) (1+ grid-y))
                                (%get grid-x (1+ grid-y))
                                (%get (1- grid-x) (1+ grid-y))
                                (%get (1- grid-x) grid-y)
                                (%get (1- grid-x) (1- grid-y))
                                (%get grid-x (1- grid-y))
                                (%get (1+ grid-x) (1- grid-y))))))))

(defun level-obstacle-exists (level position)
  (with-slots (obstacle-map) level
    (let ((x (truncate (gamekit:x position)))
          (y (truncate (gamekit:y position))))
      (gethash (cons x y) obstacle-map))))


(defun level-collide (level position bound)
  (with-slots (obstacle-map) level
    (let ((obstacle (make-instance 'obstacle :position position :bound bound))
          (level-obstacles (find-adjascent-cells level position)))
      (loop for level-obstacle in level-obstacles
              thereis (collidingp obstacle level-obstacle)))))


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
