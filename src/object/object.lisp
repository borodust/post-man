(cl:in-package :post-man)


(declaim (special *update-delta-time*))


(defclass updatable ()
  ((last-update-time :initform nil)))


(defgeneric update (updatable)
  (:method (updatable)))


(defmethod update :around ((this updatable))
  (with-slots (last-update-time) this
    (let* ((now (bodge-util:real-time-seconds))
           (*update-delta-time* (if last-update-time
                                    (- now last-update-time)
                                    now)))
      (unwind-protect
           (call-next-method)
        (setf last-update-time now)))))


(defclass movable (updatable)
  ((speed :initform 0 :initarg :speed)
   (direction :initform (gamekit:vec2 0 0))
   (position :initform (gamekit:vec2 0 0) :reader position-of)))


(defun move-object (object direction)
  (with-slots ((this-direction direction)) object
    (setf (gamekit:x this-direction) (gamekit:x direction)
          (gamekit:y this-direction) (gamekit:y direction))))


(defmethod update ((this movable))
  (with-slots (speed direction position) this
    (call-next-method)
    (setf position (gamekit:add position (gamekit:mult direction
                                                       speed
                                                       *update-delta-time*)))))
