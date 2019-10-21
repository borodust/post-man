(cl:in-package :post-man)

(defparameter *max-bogdan-count* 30)
(defparameter *max-box-count* 20)

(defparameter *capture-fade-out-time* 5)
(defparameter *capture-fade-out-scale* 5)

(defparameter *level-fade-out-time* 1)

(defclass gameplay-state (input-handling-state)
  ((level :initform nil)
   (bogdans :initform (list))
   (rob-o-man :initform nil)
   (renderables :initform (make-array 0 :adjustable t :fill-pointer t))
   (random-generator :initform nil)
   (level-number :initform nil)
   (box-count :initform nil)
   (bogdan-count :initform nil)
   (hud-font :initform (gamekit:make-font :retro 34))
   (seed :initform (error ":seed missing") :initarg :seed)
   (next-seed :initform nil)
   (total-boxes-placed :initform 0 :initarg :total-boxes-placed)
   (total-time-spent :initform 0 :initarg :total-time-spent)
   (dpad-state :initform nil)
   (started-at :initform (bodge-util:real-time-seconds))
   (captured-at :initform nil)
   (finished-at :initform nil)))


(defmethod initialize-instance :after ((this gameplay-state) &key level)
  (with-slots (random-generator level-number box-count bogdan-count seed next-seed) this
    (let ((*gameplay* this))
      (setf random-generator (random-state:make-generator :mersenne-twister-64 seed)
            level-number level
            box-count (min *max-box-count* (* 1 (1+ (truncate (/ level 2)))))
            bogdan-count (min *max-bogdan-count* (+ 10 (* 2 (truncate (/ level 3)))))
            next-seed (random-integer #xFFFFFFFFFFFFFFFF)))))


(defun calc-level-stats (this)
  (with-slots (total-boxes-placed total-time-spent started-at captured-at level-number)
      this
    (let ((time-spent-current-level (- (or captured-at (bodge-util:real-time-seconds))
                                       started-at)))
      (values total-boxes-placed
              (+ total-time-spent time-spent-current-level)
              level-number))))


(defun transition-to-endgame (this)
  (multiple-value-bind (boxes-placed time-spent level-reached)
      (calc-level-stats this)
    (gamekit.fistmachine:transition-to 'endgame-state
                                       :boxes-collected boxes-placed
                                       :seconds-spent time-spent
                                       :level level-reached)))


(defun transition-to-next-level (this)
  (with-slots (next-seed) this
    (multiple-value-bind (boxes-placed time-spent level-reached)
        (calc-level-stats this)
      (gamekit.fistmachine:transition-to 'gameplay-state
                                         :level (1+ level-reached)
                                         :seed next-seed
                                         :total-boxes-placed boxes-placed
                                         :total-time-spent time-spent))))


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


(defun play-endgame-sequence (this)
  (with-slots (captured-at) this
    (setf captured-at (bodge-util:real-time-seconds))))


(defun play-next-level-sequence (this)
  (with-slots (finished-at) this
    (setf finished-at (bodge-util:real-time-seconds))))


(defun calc-text-width (text font)
  (multiple-value-bind (origin width height advance)
      (gamekit:calc-text-bounds text font)
    (declare (ignore origin width height))
    advance))


(defmethod objective-reached ((this gameplay-state))
  (with-slots (box-count total-boxes-placed)
      this
    (decf box-count)
    (incf total-boxes-placed)
    (if (> box-count 0)
        (spawn-box)
        (play-next-level-sequence this))))


(defmethod player-captured ((this gameplay-state))
  (gamekit:stop-sound :gameplay-tune)
  (gamekit:play-sound :capture-tune :looped-p t)
  (play-endgame-sequence this))


(defun render-gameplay (this)
  (with-slots (renderables level-number box-count hud-font started-at) this
    (flet ((%y-coord (renderable)
             (gamekit:y (position-of renderable))))
      (stable-sort renderables #'> :key #'%y-coord)
      (loop for renderable across renderables
            do (render renderable)))
    (gamekit:draw-text (format nil "Level ~2,'0d" level-number)
                       (gamekit:vec2 6 (- (gamekit:viewport-height) 25))
                       :font hud-font
                       :fill-color *foreground*)
    (let ((text (format nil "Left: ~2,'0d" box-count)))
      (gamekit:draw-text text
                         (gamekit:vec2 (- (gamekit:viewport-width)
                                          (calc-text-width text hud-font)
                                          6)
                                       (- (gamekit:viewport-height) 25))
                         :font hud-font
                         :fill-color *foreground*))
    (when (< (- (bodge-util:real-time-seconds) started-at) *level-fade-out-time*)
      (let ((scale (/ (- (bodge-util:real-time-seconds) started-at)
                      *level-fade-out-time*)))
        (gamekit:draw-rect *origin*
                           (gamekit:viewport-width)
                           (gamekit:viewport-height)
                           :fill-paint (gamekit:vec4 0.1 0.1 0.1 (- 1 scale)))))))


(defun render-endgame (this)
  (with-slots (rob-o-man captured-at) this
    (let* ((transition
             (min 1.0 (/ (- (bodge-util:real-time-seconds) captured-at) *capture-fade-out-time*)))
           (scale (1+ (* (- *capture-fade-out-scale* 1) transition)))
           (scaled-x (* (+ (gamekit:x (position-of rob-o-man)) 1)
                        (* *grid-cell-width* scale)))
           (scaled-y (* (+ (gamekit:y (position-of rob-o-man)) 1)
                        (* *grid-cell-width* scale))))
      (gamekit:translate-canvas (* (+ (- scaled-x) (/ (gamekit:viewport-width) 2))
                                   transition)
                                (* (+ (- scaled-y) (/ (gamekit:viewport-height) 2))
                                   transition))
      (gamekit:scale-canvas scale scale)
      (gamekit:with-pushed-canvas ()
        (render-gameplay this))
      (gamekit:draw-rect *origin*
                         (gamekit:viewport-width)
                         (gamekit:viewport-height)
                         :fill-paint (gamekit:vec4 0.1 0.1 0.1
                                                   (/ scale
                                                      *capture-fade-out-scale*))))))


(defun render-next-level (this)
  (with-slots (finished-at) this
    (render-gameplay this)
    (let ((transition (min 1.0 (/ (- (bodge-util:real-time-seconds) finished-at)
                                  *level-fade-out-time*))))
      (gamekit:draw-rect *origin*
                         (gamekit:viewport-width)
                         (gamekit:viewport-height)
                         :fill-paint (gamekit:vec4 0.1 0.1 0.1 transition)))))


(defmethod gamekit:draw ((this gameplay-state))
  (with-slots (captured-at finished-at) this
    (bodge-canvas:clear-buffers *background*)
    (bodge-canvas:antialias-shapes nil)
    (cond
      (captured-at (render-endgame this))
      (finished-at (render-next-level this))
      (t (render-gameplay this)))))


(defmethod gamekit:act ((this gameplay-state))
  (with-slots (rob-o-man bogdans level captured-at finished-at) this
    (cond
      (captured-at (when (> (- (bodge-util:real-time-seconds) captured-at)
                            *capture-fade-out-time*)
                     (transition-to-endgame this)))
      (finished-at (when (> (- (bodge-util:real-time-seconds) finished-at)
                            *level-fade-out-time*)
                     (transition-to-next-level this)))
      (t (with-bound-objects (this)
           (update level)
           (update rob-o-man)
           (loop for bogdan in bogdans
                 do (update bogdan)))))))


(defmethod register-renderable ((this gameplay-state) renderable)
  (with-slots (renderables) this
    (vector-push-extend renderable renderables)))


(defmethod remove-renderable ((this gameplay-state) renderable)
  (with-slots (renderables) this
    (deletef renderables renderable)))


(defun pause-game (this)
  (transition-to-endgame this))


(defun interact-with-obstacles (this)
  (with-slots (level rob-o-man) this
    (with-bound-objects (this)
      (if-let ((objects (find-adjacent-obstacles level (position-of rob-o-man))))
        (loop for object in objects
              do (interact rob-o-man object))
        (interact rob-o-man nil)))))


(defmethod gamekit.input-handler:button-pressed ((this gameplay-state)
                                                 (button (eql :gamepad-start)))
  (pause-game this))


(defmethod gamekit.input-handler:button-pressed ((this gameplay-state)
                                                 (button (eql :escape)))
  (pause-game this))


(defmethod gamekit.input-handler:button-pressed ((this gameplay-state)
                                                 (button (eql :enter)))
  (interact-with-obstacles this))


(defmethod gamekit.input-handler:button-pressed ((this gameplay-state)
                                                 (button (eql :gamepad-a)))
  (interact-with-obstacles this))


(defmethod gamekit.input-handler:dpad-changed ((this gameplay-state) state)
  (with-slots (dpad-state) this
    (setf dpad-state state)))


(defmethod generate-random-integer ((this gameplay-state) bound)
  (with-slots (random-generator) this
    (random-state:random-int random-generator 0 (1- bound))))


(defmethod generate-random-float ((this gameplay-state) bound)
  (with-slots (random-generator) this
    (random-state:random-float random-generator 0 (- bound single-float-epsilon))))


(defmethod select-direction ((this gameplay-state))
  (with-slots (dpad-state) this
    (let ((button-bag (append (when dpad-state
                                (list dpad-state))
                              (gamekit.input-handler:pressed-buttons *gameplay*))))
      (flet ((%select (direction &rest buttons)
               (when (loop for button in buttons
                             thereis (member button button-bag))
                 direction)))
        (or (%select :up :w :up)
            (%select :left :a :left)
            (%select :down :s :down )
            (%select :right :d :right))))))
