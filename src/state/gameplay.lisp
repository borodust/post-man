(cl:in-package :post-man)

(defparameter *max-bogdan-count* 30)
(defparameter *max-box-count* 20)

(defclass gameplay-state (input-handling-state)
  ((level :initform nil)
   (bogdans :initform (list))
   (rob-o-man :initform nil)
   (renderables :initform (make-array 0 :adjustable t :fill-pointer t))
   (random-generator :initform nil)
   (level-number :initform nil)
   (box-count :initform nil)
   (bogdan-count :initform nil)
   (hud-font :initform (gamekit:make-font :retro 50))))


(defmethod initialize-instance :after ((this gameplay-state) &key level)
  (with-slots (random-generator level-number box-count bogdan-count) this
    (setf random-generator (random-state:make-generator :mersenne-twister-64
                                                        (derive-seed))
          level-number level
          box-count (min *max-box-count* (* 5 (1+ (truncate (/ level 5)))))
          bogdan-count (min *max-bogdan-count* (+ 10 (* 2 (truncate (/ level 5))))))))


(defmacro with-bound-objects ((state) &body body)
  (once-only (state)
    `(let ((*level* (slot-value ,state 'level))
           (*player* (slot-value ,state 'rob-o-man))
           (*gameplay* ,state))
       ,@body)))


(defun spawn-box ()
  (with-slots (level bogdans) *gameplay*
    (add-object level (make-instance 'box)
                (position-of (first bogdans)))))


(defmethod gamekit:post-initialize ((this gameplay-state))
  (with-slots (bogdans level rob-o-man bogdan-count) this
    (let ((*gameplay* this))
      (setf level (make-instance 'level)
            rob-o-man (make-instance 'rob-o-man))
      (loop repeat bogdan-count
            do (push (make-instance 'bogdan
                                    :speed (+ (random-float 0.5) 1)
                                    :position (find-level-random-position level))
                     bogdans))
      (spawn-box))))


(defun calc-text-width (text)
  (multiple-value-bind (origin width height advance)
      (gamekit:calc-text-bounds text (gamekit:make-font :retro 34))
    (declare (ignore origin width height))
    advance))


(defmethod objective-reached ((this gameplay-state))
  (with-slots (box-count level-number) this
    (decf box-count)
    (if (> box-count 0)
        (spawn-box)
        (gamekit.fistmachine:transition-to 'gameplay-state :level (1+ level-number)))))


(defmethod gamekit:draw ((this gameplay-state))
  (with-slots (renderables level-number box-count) this
    (bodge-canvas:clear-buffers *background*)
    (bodge-canvas:antialias-shapes nil)
    (flet ((%y-coord (renderable)
             (gamekit:y (position-of renderable))))
      (stable-sort renderables #'> :key #'%y-coord)
      (loop for renderable across renderables
            do (render renderable)))
    (gamekit:draw-text (format nil "Level ~A" level-number)
                       (gamekit:vec2 6 (- (gamekit:viewport-height) 25))
                       :font (gamekit:make-font :retro 34)
                       :fill-color *foreground*)
    (let ((text (format nil "Left: ~A" box-count)))
      (gamekit:draw-text text
                         (gamekit:vec2 (- (gamekit:viewport-width)
                                          (calc-text-width text)
                                          6)
                                       (- (gamekit:viewport-height) 25))
                         :font (gamekit:make-font :retro 34)
                         :fill-color *foreground*))))


(defmethod gamekit:act ((this gameplay-state))
  (with-slots (rob-o-man bogdans level) this
    (with-bound-objects (this)
      (update level)
      (update rob-o-man)
      (loop for bogdan in bogdans
            do (update bogdan)))))


(defmethod register-renderable ((this gameplay-state) renderable)
  (with-slots (renderables) this
    (vector-push-extend renderable renderables)))


(defmethod remove-renderable ((this gameplay-state) renderable)
  (with-slots (renderables) this
    (deletef renderables renderable)))


(defun pause-game ()
  (gamekit.fistmachine:transition-to 'main-menu-state))


(defun interact-with-obstacles (this)
  (with-slots (level rob-o-man) this
    (with-bound-objects (this)
      (if-let ((objects (find-adjacent-obstacles level (position-of rob-o-man))))
        (loop for object in objects
              do (interact rob-o-man object))
        (interact rob-o-man nil)))))


(defmethod gamekit.input-handler:button-pressed ((this gameplay-state)
                                                 (button (eql :gamepad-start)))
  (pause-game))


(defmethod gamekit.input-handler:button-pressed ((this gameplay-state)
                                                 (button (eql :escape)))
  (pause-game))


(defmethod gamekit.input-handler:button-pressed ((this gameplay-state)
                                                 (button (eql :enter)))
  (interact-with-obstacles this))


(defmethod gamekit.input-handler:button-pressed ((this gameplay-state)
                                                 (button (eql :gamepad-x)))
  (interact-with-obstacles this))


(defmethod generate-random-integer ((this gameplay-state) bound)
  (with-slots (random-generator) this
    (random-state:random-int random-generator 0 (1- bound))))


(defmethod generate-random-float ((this gameplay-state) bound)
  (with-slots (random-generator) this
    (random-state:random-float random-generator 0 (- bound single-float-epsilon))))
