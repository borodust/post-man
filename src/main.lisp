(cl:in-package :post-man)

(gamekit:defgame post-man (gamekit.postproc:postproc gamekit.fistmachine:fistmachine)
  ()
  (:viewport-width 800)
  (:viewport-height 600)
  (:viewport-title "Post Statics")
  (:default-initargs :initial-state 'init-state))

(defun run ()
  (gamekit:start 'post-man))
