(cl:in-package :post-man)


(defclass init-state () ())


(defmethod gamekit:post-initialize ((this init-state))
  (gamekit:prepare-resources :retro :splash
                             :rob-o-man-front :rob-o-man-back
                             :rob-o-man-right :rob-o-man-left
                             :bogdan-front :bogdan-back
                             :bogdan-right :bogdan-left
                             :box :floor
                             :horizontal-rack :horizontal-rack-active
                             :vertical-rack :vertical-rack-active
                             :menu-tune :gameplay-tune
                             :capture-tune :user-action :box-pick-up :box-drop))


(defmethod gamekit:draw ((this init-state))
  (with-slots (ui-font) this
    (bodge-canvas:clear-buffers *background*)
    (gamekit:translate-canvas (- (/ (gamekit:viewport-width) 2) 120)
                              120)
    (gamekit:scale-canvas 3 3)
    (let ((alpha (+ 0.2 (abs (* 0.8 (cos (bodge-util:real-time-seconds)))))))
      (bodge-canvas:with-alpha (alpha)
        (gamekit:draw-text "LOADING..." *origin*
                           :fill-color *foreground*)))))


(defmethod gamekit:notice-resources ((this init-state) &rest resources)
  (declare (ignore this resources))
  (gamekit.fistmachine:transition-to 'main-menu-state))
