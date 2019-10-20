(cl:in-package :post-man)

(gamekit:defgame post-man (gamekit.postproc:postproc gamekit.fistmachine:fistmachine)
  ((global-random-generator :initform (random-state:make-generator :mersenne-twister-64
                                                                   (string-hash *seed*))))
  (:viewport-width (* *grid-size* *grid-cell-width*))
  (:viewport-height (* *grid-size* *grid-cell-width*))
  (:viewport-title "POST-MAN")
  (:default-initargs :initial-state 'init-state))


(defmethod gamekit:notice-resources (any &rest resources)
  (declare (ignore any resources)))


(defmethod generate-seed ((this post-man))
  (with-slots (global-random-generator) this
    (random-state:random-int global-random-generator 0 #xFFFFFFFFFFFFFFFF)))


(defun run ()
  (gamekit:start 'post-man))
