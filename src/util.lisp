(cl:in-package :post-man)


(defvar *origin* (gamekit:vec2 0 0))

(defvar *background* (gamekit:vec4 0.1 0.1 0.1 1))
(defvar *foreground* (gamekit:vec4 0.9 0.9 0.9 1))


(defgeneric render (object))


(defgeneric update (object))
