(cl:in-package :post-man)


(declaim (special *update-delta-time*))


(defgeneric destroy (object)
  (:method (object) (declare (ignore object))))

;;;
;;; RENDERABLE
;;;
(defgeneric render (object))

(defgeneric register-renderable (state renderable))
(defgeneric remove-renderable (state renderable))

(defclass renderable () ())

(defmethod initialize-instance :after ((this renderable) &key)
  (register-renderable *gameplay* this))


(defmethod destroy :before ((this renderable))
  (remove-renderable *gameplay* this))


(defmethod render :around (object)
  (declare (ignore object))
  (gamekit:with-pushed-canvas ()
    (call-next-method)))

;;;
;;; UPDATABLE
;;;
(defclass updatable ()
  ((last-update-time :initform nil)))


(defgeneric update (updatable)
  (:method (updatable)))


(defun delta-time (updatable)
  (with-slots (last-update-time) updatable
    (if last-update-time
        (- (bodge-util:real-time-seconds) last-update-time)
        0)))


(defmethod update :around ((this updatable))
  (with-slots (last-update-time) this
    (let* ((now (bodge-util:real-time-seconds))
           (*update-delta-time* (if last-update-time
                                    (- now last-update-time)
                                    0)))
      (unwind-protect
           (call-next-method)
        (setf last-update-time now)))))


;;;
;;; POSITIONABLE
;;;
(defclass positionable ()
  ((position :initform (gamekit:vec2 0 0) :initarg :position :reader position-of)))


(defun update-position (positionable position)
  (with-slots ((this-position position)) positionable
    (setf (gamekit:x this-position) (gamekit:x position)
          (gamekit:y this-position) (gamekit:y position))))

;;;
;;; MOVABLE
;;;
(defclass movable (updatable positionable)
  ((speed :initform 0 :initarg :speed)
   (direction :initform nil)))


(defun move-object (object direction)
  (with-slots ((this-direction direction)) object
    (setf (gamekit:x this-direction) (gamekit:x direction)
          (gamekit:y this-direction) (gamekit:y direction))))


(defun next-position (movable)
  (with-slots (speed direction position) movable
    (gamekit:add position (gamekit:mult (bodge-math:normalize direction)
                                        speed
                                        (delta-time movable)))))

;;;
;;; BOUNDED
;;;
(defclass bounded ()
  ((bound :initarg :bound :initform (error ":bound missing") :reader bound-of)))


(defun collidingp (this that)
  (let ((this-position (position-of this))
        (that-position (position-of that))
        (this-bound (bound-of this))
        (that-bound (bound-of that)))
    (and (< (gamekit:x this-position)
            (+ (gamekit:x that-position)
               (gamekit:x that-bound)))
         (> (+ (gamekit:x this-position)
               (gamekit:x this-bound))
            (gamekit:x that-position))
         (< (gamekit:y this-position)
            (+ (gamekit:y that-position)
               (gamekit:y that-bound)))
         (> (+ (gamekit:y this-position)
               (gamekit:y this-bound))
            (gamekit:y that-position)))))


;;;
;;; INTERACTABLE
;;;
(defgeneric interact (this that)
  (:method (this that) (declare (ignore this that))))


;;;
;;; OBSTACLE
;;;
(defgeneric obstacle-of (object)
  (:method (object)
    (declare (ignore object))
    '((0 . 0))))


;;;
;;; OBJECTIVE
;;;
(defclass objective ()
  ((activated :initform nil :reader activatedp)))


(defgeneric activatedp (objective)
  (:method ((this objective))
    (slot-value this 'activated)))


(defgeneric activate (objective)
  (:method ((this objective))
    (with-slots (activated) this
      (setf activated t))))


(defgeneric deactivate (objective)
  (:method ((this objective))
    (with-slots (activated) this
      (setf activated nil))))


(defgeneric objective-reached (state))
(defgeneric player-captured (state))


(defmethod deactivate :around (object)
  (let ((was-active (activatedp object)))
    (prog1 (call-next-method)
      (when (and was-active (not (activatedp object)))
        (objective-reached *gameplay*)))))


(defun play-user-action-sound ()
  (gamekit:play-sound :user-action))
