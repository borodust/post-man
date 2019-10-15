(cl:in-package :post-man)


(defclass main-menu-state (input-handling-state)
  ((options :initform (make-array 2 :initial-contents '(:exit :start)))
   (selected-option-idx :initform 1)))


(defmethod gamekit:draw ((this main-menu-state))
  (with-slots (options selected-option-idx) this
    (bodge-canvas:clear-buffers (gamekit:vec4 0.1 0.1 0.1 1))
    (gamekit:with-pushed-canvas ()
      (gamekit:translate-canvas 230 290)
      (gamekit:scale-canvas 0.5 0.5)
      (gamekit:draw-image (gamekit:vec2 0 0) :splash))
    (gamekit:with-pushed-canvas ()
      (gamekit:translate-canvas 300 120)
      (gamekit:scale-canvas 2 2)
      (loop for i from 0
            for option across options
            when (= i selected-option-idx)
              do (gamekit:draw-text ">" (gamekit:vec2 0 (* i 15))
                                    :font (gamekit:make-font :retro 20)
                                    :fill-color (gamekit:vec4 0.9 0.9 0.9 1))
            do (gamekit:draw-text (string option) (gamekit:vec2 20 (* i 15))
                                  :font (gamekit:make-font :retro 20)
                                  :fill-color (gamekit:vec4 0.9 0.9 0.9 1))))))


(defun select-next-menu-option (state)
  (with-slots (options selected-option-idx) state
    (setf selected-option-idx (mod (1+ selected-option-idx) (length options)))))


(defun select-prev-menu-option (state)
  (with-slots (options selected-option-idx) state
    (setf selected-option-idx (mod (1- selected-option-idx) (length options)))))


(defun invoke-action (this)
  (with-slots (options selected-option-idx) this
    (case (aref options selected-option-idx)
      (:start)
      (:exit (gamekit:stop)))))


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


(defmethod gamekit.input-handler:button-pressed ((this main-menu-state)
                                                 (button (eql :enter)))
  (invoke-action this))


(defmethod gamekit.input-handler:button-pressed ((this main-menu-state)
                                                 (button (eql :enter)))
  (invoke-action this))


(defmethod gamekit.input-handler:button-pressed ((this main-menu-state)
                                                 (button (eql :gamepad-x)))
  (invoke-action this))
