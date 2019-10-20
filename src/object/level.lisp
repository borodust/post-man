(cl:in-package :post-man)


(defparameter *floor-color* (gamekit:vec4 0.3 0.3 0.3 1))
(defparameter *obstacle-count* (truncate (* *grid-size* 1.5)))


(defclass level (positionable renderable)
  ((obstacle-map :initform (make-hash-table :test 'equal))
   (object-map :initform (make-hash-table))
   (objectives :initform (make-array 0 :adjustable t :fill-pointer t)))
  (:default-initargs :position (gamekit:vec2 (1+ *grid-size*) (1+ *grid-size*))))


(defun cell->position (cell)
  (gamekit:vec2 (car cell) (cdr cell)))


(defun position->cell (position)
  (cons (truncate (gamekit:x position))
        (truncate (gamekit:y position))))


(defun %level-obstacle-exists (level cell)
  (with-slots (obstacle-map) level
    (let ((x (car cell))
          (y (cdr cell)))
      (or (< x 0) (>= x *grid-size*)
          (< y 0) (>= y *grid-size*)
          (gethash cell obstacle-map)))))


(defun %find-adjacent-cells (level node)
  (with-slots (obstacle-map) level
    (destructuring-bind (grid-x . grid-y) node
      (flet ((%get (x y)
               (let ((cell (cons x y)))
                 (unless (%level-obstacle-exists level cell)
                   cell))))
        (remove-if #'null (list (%get (1+ grid-x) grid-y)
                                (%get grid-x (1+ grid-y))
                                (%get (1- grid-x) grid-y)
                                (%get grid-x (1- grid-y))))))))


(defun %find-adjacent-obstacles (level node)
  (with-slots (obstacle-map) level
    (destructuring-bind (grid-x . grid-y) node
      (flet ((%get (x y)
               (%level-obstacle-exists level (cons x y))))
        (remove-duplicates (list (%get (1+ grid-x) grid-y)
                                 (%get grid-x (1+ grid-y))
                                 (%get (1- grid-x) grid-y)
                                 (%get grid-x (1- grid-y))))))))


(defun find-adjacent-obstacles (level position)
  (with-slots (obstacle-map) level
    (let* ((x (truncate (gamekit:x position)))
           (y (truncate (gamekit:y position)))
           (cell (cons x y)))
      (%find-adjacent-obstacles level cell))))


(defun level-obstacle-exists (level position)
  (with-slots (obstacle-map) level
    (let* ((x (truncate (gamekit:x position)))
           (y (truncate (gamekit:y position)))
           (cell (cons x y)))
      (%level-obstacle-exists level cell))))


(defun level-renderable-objects (level)
  (with-slots (object-map) level
    (loop for object being the hash-key of object-map
          collect object)))


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


(defun %obstacle-fits-p (level cell &optional (configuration '((0 . 0))))
  (not (loop with (cell-x . cell-y) = cell
             for (relative-x . relative-y) in configuration
             for x = (+ relative-x cell-x)
             for y = (+ relative-y cell-y)
               thereis (%level-obstacle-exists level (cons x y)))))


(defun %find-level-random-cell (level &optional (configuration '((0 . 0))))
  (with-slots (obstacle-map) level
    (flet ((%gen ()
             (cons (random-integer *grid-size*) (random-integer *grid-size*)))
           (%collides (cell)
             (not (%obstacle-fits-p level cell configuration))))
      (loop for cell = (%gen)
            while (%collides cell)
            finally (return cell)))))


(defun find-level-random-position (level &optional (configuration '((0 . 0))))
  (let ((cell (%find-level-random-cell level configuration)))
    (gamekit:vec2 (car cell) (cdr cell))))


(defun find-level-random-path (level position)
  (find-level-path level position (find-level-random-position level)))


(defun %add-object (level object cell)
  (with-slots (obstacle-map object-map objectives) level
    (let ((configuration (obstacle-of object)))
      (when (%obstacle-fits-p level cell configuration)
        (loop with (cell-x . cell-y) = cell
              for (x-offset . y-offset) in configuration
              for obstacle-cell = (cons (+ cell-x x-offset) (+ cell-y y-offset))
              do (setf (gethash obstacle-cell obstacle-map) object))
        (setf (gethash object object-map) configuration)
        (when (typep object 'objective)
          (vector-push-extend object objectives))
        (update-position object (cell->position cell))
        object))))


(defun add-object (level object position)
  (%add-object level object (position->cell position)))


(defun spawn-object (level object)
  (let ((cell (%find-level-random-cell level (obstacle-of object))))
    (%add-object level object cell)))


(defun remove-object (level object)
  (with-slots (object-map obstacle-map objectives) level
    (when-let ((configuration (gethash object object-map)))
      (remhash object object-map)
      (loop with object-x = (truncate (gamekit:x (position-of object)))
            and object-y = (truncate (gamekit:y (position-of object)))
            for (offset-x . offset-y) in configuration
            for cell = (cons (+ object-x offset-x) (+ object-y offset-y))
            do (remhash cell obstacle-map))
      (when (typep object 'objective)
        (deletef objectives object))))
  object)


(defun spawn-vertical-rack (level)
  (spawn-object level (make-instance 'vertical-rack)))


(defun spawn-horizontal-rack (level)
  (spawn-object level (make-instance 'horizontal-rack)))


(defun fill-level (level)
  (loop repeat *obstacle-count*
        if (oddp (random-integer 2))
          do (spawn-vertical-rack level)
        else
          do (spawn-horizontal-rack level)))


(defmethod initialize-instance :after ((this level) &key)
  (fill-level this))


(defun pick-objective (level)
  (with-slots (objectives) level
    (let ((objective (aref objectives (random-integer (length objectives)))))
      (activate objective))))


(defmethod render ((this level))
  (let ((side (* *grid-size* *grid-cell-width*)))
    (gamekit:draw-rect *origin* side side :fill-paint *floor-color*)))
