(cl:in-package :post-man)


(declaim (special *level*
                  *player*
                  *gameplay*))


(defvar *origin* (gamekit:vec2 0 0))


(defvar *up* (gamekit:vec2 0 1))
(defvar *down* (gamekit:vec2 0 -1))
(defvar *left* (gamekit:vec2 1 0))
(defvar *right* (gamekit:vec2 -1 0))

(defvar *background* (gamekit:vec4 0.1 0.1 0.1 1))
(defvar *foreground* (gamekit:vec4 0.9 0.9 0.9 1))

(defparameter *grid-cell-width* 32)
(defparameter *grid-size* 20)


(defun vec= (this that)
  (and (< (abs (- (gamekit:x this) (gamekit:x that))) single-float-epsilon)
       (< (abs (- (gamekit:y this) (gamekit:y that))) single-float-epsilon)))


;; FIXME: detect unreachable paths
(defun find-node-path (start-node goal-node
                       &key heuristic-cost path-cost node-children node-equal)
  (labels ((%heuristic-cost (current-node)
             (funcall heuristic-cost current-node goal-node))
           (%path-cost (current-node another-node)
             (funcall path-cost current-node another-node))
           (%node-children (node)
             (funcall node-children node))
           (%node-equal (this that)
             (funcall node-equal this that))
           (%key (path-head)
             (destructuring-bind (path-cost . heuristic-cost)
                 (first path-head)
               (+ heuristic-cost path-cost))))
    (let ((paths (bodge-heap:make-binary-heap :key #'%key))
          (processed (make-hash-table :test 'equal)))
      (bodge-heap:binary-heap-push paths (list* (cons 0 (%heuristic-cost start-node))
                                                (list start-node)))
      (setf (gethash start-node processed) t)
      (loop for ((parent-path-cost . nil) . path) = (bodge-heap:binary-heap-pop paths)
            for parent = (first path)
            always parent
            until (%node-equal parent goal-node)
            do (loop for child in (%node-children parent)
                     for heuristic-cost = (%heuristic-cost child)
                     for path-cost = (+ parent-path-cost (%path-cost parent child))
                     for head = (cons path-cost heuristic-cost)
                     unless (gethash child processed)
                       do (setf (gethash child processed) t)
                          (bodge-heap:binary-heap-push paths
                                                       (list* head (list* child path))))
            finally (return (nreverse path))))))


(defun to-isometric (position &key (width-factor 0.5) (height-factor 0.5)
                                tile-width tile-height)
  (let* ((x-iso (* (- (gamekit:x position) (gamekit:y position))
                   (if tile-width
                       (/ tile-width *grid-cell-width* 2)
                       width-factor)))
         (y-iso (* (+ (gamekit:x position) (gamekit:y position))
                   (if tile-height
                       (/ tile-height *grid-cell-width* 2)
                       height-factor))))
    (gamekit:vec2 x-iso y-iso)))


(defun translate-isometric (position &key (width-factor 0.5) (height-factor 0.5)
                                       tile-width tile-height)
  (let ((position (to-isometric position :width-factor width-factor
                                         :height-factor height-factor
                                         :tile-width tile-width
                                         :tile-height tile-height)))
    (gamekit:translate-canvas (* (gamekit:x position) *grid-cell-width*)
                              (* (gamekit:y position) *grid-cell-width*))))


(defun translate-position (position)
  (gamekit:translate-canvas (* (gamekit:x position) *grid-cell-width*)
                            (* (gamekit:y position) *grid-cell-width*)))
