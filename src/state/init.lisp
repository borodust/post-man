(cl:in-package :post-statics)


(defclass init-state () ())


(defmethod gamekit:post-initialize ((this init-state))
  (gamekit.fistmachine:transition-to 'main-menu-state))
