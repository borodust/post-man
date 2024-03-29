(cl:in-package :post-man)


(defclass input-handling-state (gamekit.input-handler:input-handler) ())


(defmethod gamekit:post-initialize :around ((this input-handling-state))
  (gamekit.input-handler:activate-input-handler this)
  (call-next-method))


(defmethod gamekit:pre-destroy :around ((this input-handling-state))
  (unwind-protect
       (call-next-method)
    (gamekit.input-handler:deactivate-input-handler this)))
