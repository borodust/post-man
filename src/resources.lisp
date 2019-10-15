(cl:in-package :post-man)

(gamekit:register-resource-package
 :keyword (asdf:system-relative-pathname :post-man "assets/"))

(gamekit:define-font :retro "fonts/retro-gaming/Retro Gaming.ttf")

(gamekit:define-image :splash "images/Post_Logo.png"
  :use-nearest-interpolation t)
