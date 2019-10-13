(cl:in-package :post-statics)

(gamekit:defgame post-statics (gamekit.postproc:postproc)
  ()
  (:viewport-width 960)
  (:viewport-height 540)
  (:viewport-title "Post Statics")
  (:canvas-width 270)
  (:canvas-height 270)
  (:default-initargs :postproc-indirect-width 480
                     :postproc-indirect-height 270))

(defun run ()
  (gamekit:start 'post-statics))
