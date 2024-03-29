(cl:in-package :post-man)

(gamekit:register-resource-package
 :keyword (asdf:system-relative-pathname :post-man "assets/"))

(gamekit:define-font :retro "fonts/retro-gaming/Retro Gaming.ttf")

(gamekit:define-image :splash "images/menu/Post_Logo.png"
  :use-nearest-interpolation t)

;;;
;;; ROB-O-MAN
;;;
(gamekit:define-image :rob-o-man-front "images/rob-o-man/robo_idle.png"
  :use-nearest-interpolation t)

(gamekit:define-image :rob-o-man-back "images/rob-o-man/robo_back.png"
  :use-nearest-interpolation t)

(gamekit:define-image :rob-o-man-right "images/rob-o-man/robo_side.png"
  :use-nearest-interpolation t)

(gamekit:define-image :rob-o-man-left "images/rob-o-man/robo_side_opposite.png"
  :use-nearest-interpolation t)

;;;
;;; BOGDAN
;;;
(gamekit:define-image :bogdan-front "images/bogdan/bogdan_idle.png"
  :use-nearest-interpolation t)

(gamekit:define-image :bogdan-back "images/bogdan/bogdan_back.png"
  :use-nearest-interpolation t)

(gamekit:define-image :bogdan-right "images/bogdan/bogdan_walk.png"
  :use-nearest-interpolation t)

(gamekit:define-image :bogdan-left "images/bogdan/bogdan_walk_opposite.png"
  :use-nearest-interpolation t)


;;;
;;; BOX
;;;
(gamekit:define-image :box "images/level/box.png"
  :use-nearest-interpolation t)


(gamekit:define-image :horizontal-rack "images/level/horizontal_boxes.png"
  :use-nearest-interpolation t)


(gamekit:define-image :horizontal-rack-active
  "images/level/horizontal_boxes_active.png"
  :use-nearest-interpolation t)


(gamekit:define-image :vertical-rack "images/level/vertical_boxes.png"
  :use-nearest-interpolation t)


(gamekit:define-image :vertical-rack-active
  "images/level/vertical_boxes_active.png"
  :use-nearest-interpolation t)

;;;
;;; LEVEL
;;;
(gamekit:define-image :floor
  "images/level/floor.png"
  :use-nearest-interpolation t)

;;;
;;; MUSIC
;;;
(gamekit:define-sound :menu-tune "music/Alexander Nakarada - Be Jammin.ogg")

(gamekit:define-sound :gameplay-tune "music/Kevin MacLeod - Funky Energy Loop.ogg")

(gamekit:define-sound :capture-tune "music/Rafael Krux - Horror Suspense.ogg")


;;;
;;; SFX
;;;
(gamekit:define-sound :user-action "sounds/404049__deathscyp__breaker-1.wav")

(gamekit:define-sound :box-pick-up
  "sounds/422651__trullilulli__sfx-player-action-phone-pick-up.wav")

(gamekit:define-sound :box-drop "sounds/272065__bexhillcollege__thrown-object.wav")
