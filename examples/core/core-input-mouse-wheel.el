;;; Based on https://github.com/raysan5/raylib/blob/5.0/examples/core/core_input_mouse_wheel.c

(require 'raylib)
(require 'cl-lib)

(defvar screen-width 800)
(defvar screen-height 450)

(rl-init-window screen-width screen-height "raylib [core] example - mouse wheel")

(defvar box-position-y (- (/ screen-height 2) 40))
(defvar scroll-speed 4) ;; Scrolling speed in pixels

(defun my-mainloop (dt)
  (cl-decf box-position-y (* (rl-get-mouse-wheel-move) scroll-speed))

  (rl-begin-drawing)
  (rl-clear-background rl-white)

  (rl-draw-rectangle (- (/ screen-width 2) 40) box-position-y 80 80 rl-maroon)

  (rl-draw-text "Use mouse wheel to move the cube up and down!"  10 10 20 rl-gray);
  (rl-draw-text (format "Box position Y: %03d" box-position-y) 10 40 20 rl-lightgray);

  (rl-end-drawing))

(rl-run-mainloop #'my-mainloop)
