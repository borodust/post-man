(cl:in-package :post-man)

(gamekit:defgame post-man (gamekit.fistmachine:fistmachine) ()
  (:viewport-width (* *grid-size* *grid-cell-width*))
  (:viewport-height (* *grid-size* *grid-cell-width*))
  (:viewport-title "POST-MAN")
  (:prepare-resources nil)
  (:default-initargs :initial-state 'init-state))


(defmethod gamekit:notice-resources (any &rest resources)
  (declare (ignore any resources)))


(defun run ()
  (gamekit:start 'post-man))
