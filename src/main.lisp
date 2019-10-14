(cl:in-package :post-man)

(gamekit:defgame post-man (gamekit.postproc:postproc gamekit.fistmachine:fistmachine)
  ()
  (:viewport-width 800)
  (:viewport-height 600)
  (:viewport-title "Post Statics")
  (:canvas-width 320)
  (:canvas-height 240)
  (:default-initargs :postproc-indirect-width 320
                     :postproc-indirect-height 240
                     :initial-state 'init-state))

(defun run ()
  (gamekit:start 'post-man))
