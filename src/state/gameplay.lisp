(cl:in-package :post-man)


(defclass gameplay-state (input-handling-state)
  ((level :initform (make-instance 'level))
   (bogdans :initform (list))
   (rob-o-man :initform (make-instance 'rob-o-man))))


(defmacro with-bound-objects ((state) &body body)
  (once-only (state)
    `(let ((*level* (slot-value ,state 'level))
           (*player* (slot-value ,state 'rob-o-man))
           (*gameplay* ,state))
       ,@body)))


(defmethod gamekit:post-initialize ((this gameplay-state))
  (with-slots (bogdans level) this
    (loop repeat 10
          do (push (make-instance 'bogdan
                                  :speed (+ (random 0.5) 1)
                                  :position (find-level-random-position level))
                   bogdans))))


(defmethod gamekit:draw ((this gameplay-state))
  (with-slots (level bogdans rob-o-man) this
    (bodge-canvas:clear-buffers *background*)
    (bodge-canvas:antialias-shapes nil)
    (render level)
    (loop for bogdan in bogdans
          do (render bogdan))
    (gamekit:with-pushed-canvas ()
      (render rob-o-man))))


(defmethod gamekit:act ((this gameplay-state))
  (with-slots (rob-o-man bogdans level) this
    (with-bound-objects (this)
      (update level)
      (update rob-o-man)
      (loop for bogdan in bogdans
            do (update bogdan)))))


(defun pause-game ()
  (gamekit.fistmachine:transition-to 'main-menu-state))


(defun interact-with-obstacles (this)
  (with-slots (level rob-o-man) this
    (with-bound-objects (this)
      (if-let ((objects (find-adjacent-obstacles level (position-of rob-o-man))))
        (loop for object in objects
              do (interact rob-o-man object))
        (interact rob-o-man nil)))))


(defmethod gamekit.input-handler:button-pressed ((this gameplay-state)
                                                 (button (eql :gamepad-start)))
  (pause-game))


(defmethod gamekit.input-handler:button-pressed ((this gameplay-state)
                                                 (button (eql :escape)))
  (pause-game))


(defmethod gamekit.input-handler:button-pressed ((this gameplay-state)
                                                 (button (eql :enter)))
  (interact-with-obstacles this))


(defmethod gamekit.input-handler:button-pressed ((this gameplay-state)
                                                 (button (eql :gamepad-x)))
  (interact-with-obstacles this))
