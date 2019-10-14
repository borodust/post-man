(cl:in-package :post-man)


(defclass main-menu-state (input-handling-state)
  ((options :initform (make-array 2 :initial-contents '(:exit :start)))
   (selected-option-idx :initform 1)))


(defmethod gamekit:draw ((this main-menu-state))
  (with-slots (options selected-option-idx) this
    (ge.vg:antialias-shapes nil)
    (gamekit:with-pushed-canvas ()
      (gamekit:scale-canvas 2 2)
      (gamekit:translate-canvas 32 90)
      (gamekit:draw-text "Post Statics" (gamekit:vec2 0 0)))
    (gamekit:with-pushed-canvas ()
      (gamekit:translate-canvas 120 80)
      (loop for i from 0
            for option across options
            for option-string = (format nil "~A ~A" (if (= i selected-option-idx)
                                                        ">"
                                                        " ")
                                        (string option))
            do (gamekit:draw-text option-string (gamekit:vec2 0 (* i 15)))))))


(defun select-next-menu-option (state)
  (with-slots (options selected-option-idx) state
    (setf selected-option-idx (mod (1+ selected-option-idx) (length options)))))


(defun select-prev-menu-option (state)
  (with-slots (options selected-option-idx) state
    (setf selected-option-idx (mod (1- selected-option-idx) (length options)))))


(defmethod gamekit.input-handler:button-pressed ((this main-menu-state)
                                                 (button (eql :down)))
  (select-next-menu-option this))


(defmethod gamekit.input-handler:button-pressed ((this main-menu-state)
                                                 (button (eql :s)))
  (select-next-menu-option this))


(defmethod gamekit.input-handler:dpad-changed ((this main-menu-state)
                                               (button (eql :down)))
  (select-next-menu-option this))


(defmethod gamekit.input-handler:button-pressed ((this main-menu-state)
                                                 (button (eql :up)))
  (select-prev-menu-option this))


(defmethod gamekit.input-handler:button-pressed ((this main-menu-state)
                                                 (button (eql :w)))
  (select-prev-menu-option this))


(defmethod gamekit.input-handler:dpad-changed ((this main-menu-state)
                                               (button (eql :up)))
  (select-prev-menu-option this))
