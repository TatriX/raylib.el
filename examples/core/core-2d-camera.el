;;; Based on https://github.com/raysan5/raylib/blob/5.0/examples/core/core_2d_camera.c

(require 'raylib)
(require 'cl-lib)

(defvar screen-width 800)
(defvar screen-height 450)

(rl-init-window screen-width screen-height "raylib [core] example - 2d camera")

(defvar player [400 280 40 40])
(defvar buildings
  (cl-loop repeat 150
           for spacing = 0 then (+ spacing width)
           for width = (rl-get-random-value 50 200)
           for height = (rl-get-random-value 100 800)
           for x = (- spacing 6000)
           for y = (- screen-height 130 height)
           for color = (rl-color (rl-get-random-value 200 240)
                                 (rl-get-random-value 200 240)
                                 (rl-get-random-value 200 250)
                                 255)
           collect (cons (rl-rectangle x y width height) color)))

(defvar camera (make-instance 'rl-camera-2d
                              :target (rl-vector2 (+ (rl-x player) 20)
                                                  (+ (rl-y player) 20))
                              :offset (rl-vector2 (/ screen-width 2)
                                                  (/ screen-height 2))
                              :zoom 1))

(defun my-mainloop (dt)
  ;; Player movement
  (cond
   ((rl-is-key-down rl-key-right) (cl-incf (rl-x player) 2))
   ((rl-is-key-down rl-key-left) (cl-decf (rl-x player) 2)))

  ;; Camera target follows player
  (setf (slot-value camera 'target) (rl-vector2 (+ (rl-x player) 20)
                                                (+ (rl-y player) 20)))

  ;; Camera rotation controls
  (cond
   ((rl-is-key-down rl-key-a) (cl-decf (slot-value camera 'rotation)))
   ((rl-is-key-down rl-key-s) (cl-incf (slot-value camera 'rotation))))

  ;; Limit camera rotation to 80 degrees (-40 to 40)
  (with-slots (rotation) camera
    (when (> rotation 40) (setf rotation 40))
    (when (< rotation -40) (setf rotation -40)))

  ;; Camera zoom controls
  (with-slots (zoom) camera
    (cl-incf zoom (* (rl-get-mouse-wheel-move) 0.05))
    (when (> zoom 3) (setf zoom 3))
    (when (< zoom 0.1) (setf zoom 0.1)))

  (when (rl-is-key-down rl-key-r)
    (with-slots (rotation zoom) camera
      (setf zoom 1)
      (setf rotation 0)))


  (rl-begin-drawing)
  (rl-clear-background rl-white)

  ;; You can also use (rl-begin-mode-2d) and (rl-end-mode-2d) directly.
  (rl-with-camera-2d camera
    (rl-draw-rectangle -6000 320 13000 8000 rl-darkgray)
    (mapc (pcase-lambda (`(,rec . ,color)) (rl-draw-rectangle-rec rec color)) buildings)

    (rl-draw-rectangle-rec player rl-red)

    (with-slots (target) camera
      (rl-draw-line (rl-x target) (* screen-height -10) (rl-x target) (* screen-height 10) rl-green)
      (rl-draw-line (* screen-width -10) (rl-y target) (* screen-width 10) (rl-y target) rl-green)))

  (rl-draw-text "SCREEN AREA" 640 10 20 rl-red)

  (rl-draw-rectangle 0 0 screen-width 5 rl-red)
  (rl-draw-rectangle 0 5 5 (- screen-height 10) rl-red)
  (rl-draw-rectangle (- screen-width 5) 5 5 (- screen-height 10) rl-red)
  (rl-draw-rectangle 0 (- screen-height 5) screen-width 5 rl-red)

  (rl-draw-rectangle 10 10 250 113 (rl-fade rl-skyblue 0.5))
  (rl-draw-rectangle-lines 10 10 250 113 rl-blue)

  (rl-draw-text "Free 2d camera controls:" 20 20 10 rl-black)
  (rl-draw-text "- Right/Left to move Offset" 40 40 10 rl-darkgray)
  (rl-draw-text "- Mouse Wheel to Zoom in-out" 40 60 10 rl-darkgray)
  (rl-draw-text "- A / S to Rotate" 40 80 10 rl-darkgray)
  (rl-draw-text "- R to reset Zoom and Rotation" 40 100 10 rl-darkgray)

  (rl-end-drawing))

(rl-run-mainloop #'my-mainloop)
