(cl:in-package :post-man)


(defclass gameplay-state (input-handling-state)
  ((level :initform (make-instance 'level))
   (bogdans :initform (list))
   (rob-o-man :initform (make-instance 'rob-o-man))))


(defmethod gamekit:post-initialize ((this gameplay-state))
  (with-slots (bogdans level) this
    (loop repeat 10
          do (push (make-instance 'bogdan
                                  :speed (+ (random 0.5) 1)
                                  :position (find-level-random-position level))
                   bogdans))
    (spawn-box level)))


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
    (update level)
    (let ((*level* level)
          (*player* rob-o-man)
          (*gameplay* this))
      (update rob-o-man)
      (loop for bogdan in bogdans
            do (update bogdan)))))


(defun pause-game ()
  (gamekit.fistmachine:transition-to 'main-menu-state))


(defmethod gamekit.input-handler:button-pressed ((this gameplay-state)
                                                 (button (eql :gamepad-start)))
  (pause-game))


(defmethod gamekit.input-handler:button-pressed ((this gameplay-state)
                                                 (button (eql :escape)))
  (pause-game))
