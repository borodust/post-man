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


(defun %find-adjacent-cells (level node)
  (with-slots (obstacle-map) level
    (destructuring-bind (grid-x . grid-y) node
      (flet ((%get (x y)
               (when (and (<= 0 x (1- *grid-size*))
                          (<= 0 y (1- *grid-size*))
                          (not (gethash (cons x y) obstacle-map)))
                 (cons x y))))
        (remove-if #'null (list (%get (1+ grid-x) grid-y)
                                (%get grid-x (1+ grid-y))
                                (%get (1- grid-x) grid-y)
                                (%get grid-x (1- grid-y))))))))


(defun level-obstacle-exists (level position)
  (with-slots (obstacle-map) level
    (let ((x (truncate (gamekit:x position)))
          (y (truncate (gamekit:y position))))
      (gethash (cons x y) obstacle-map))))


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


(defun find-level-path (level start goal)
  (let ((start-x (truncate (gamekit:x start)))
        (start-y (truncate (gamekit:y start)))
        (goal-x (truncate (gamekit:x goal)))
        (goal-y (truncate (gamekit:y goal))))
    (flet ((%path-cost (node goal)
             (bodge-math:vector-length (bodge-math:subt
                                        (bodge-math:vec2 (car node) (cdr node))
                                        (bodge-math:vec2 (car goal) (cdr goal)))))
           (%node-children (node)
             (%find-adjacent-cells level node))
           (%to-vec2 (cell)
             (gamekit:vec2 (car cell) (cdr cell))))
      (mapcar #'%to-vec2
              (find-node-path (cons start-x start-y) (cons goal-x goal-y)
                              :heuristic-cost #'%path-cost
                              :path-cost #'%path-cost
                              :node-children #'%node-children
                              :node-equal #'equal)))))


(defun find-level-random-position (level)
  (with-slots (obstacle-map) level
    (flet ((%gen ()
             (cons (random *grid-size*) (random *grid-size*))))
      (loop for cell = (%gen)
            for obstacle = (gethash cell obstacle-map)
            while obstacle
            finally (return (gamekit:vec2 (car cell) (cdr cell)))))))


(defun find-level-random-path (level position)
  (find-level-path level position (find-level-random-position level)))
