(cl:in-package :post-man)


(defclass bogdan (being)
  ((path :initform nil))
  (:default-initargs :speed 1 :color (gamekit:vec4 0.4 0.1 0.1 1)))


(defmethod update ((this bogdan))
  (call-next-method)
  (with-slots (path) this
    (if path
        (unless (direction-of this)
          (let* ((next-cell (pop path))
                 (vector (gamekit:subt next-cell (position-of this))))
            (move-being this (cond
                               ((> (gamekit:x vector) 0.5) :right)
                               ((< (gamekit:x vector) -0.5) :left)
                               ((> (gamekit:y vector) 0.5) :up)
                               ((< (gamekit:y vector) -0.5) :down)))))
        (setf path (find-level-random-path *level* (position-of this))))))
