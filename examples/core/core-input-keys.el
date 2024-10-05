;;; Based on https://github.com/raysan5/raylib/blob/5.0/examples/core/core_input_keys.c
(require 'raylib)
(require 'cl-lib)

(defvar screen-width 800)
(defvar screen-height 450)

(rl-init-window screen-width screen-height "raylib [core] example - keyboard input")

(defvar ball-position (rl-v2 (/ screen-width 2.0)
                             (/ screen-height 2.0)))

(defun my-mainloop ()
  (cond
   ((rl-is-key-down rl-key-right) (cl-incf (rl-x ball-position) 2))
   ((rl-is-key-down rl-key-left) (cl-decf (rl-x ball-position) 2))
   ((rl-is-key-down rl-key-down) (cl-incf (rl-y ball-position) 2))
   ((rl-is-key-down rl-key-up) (cl-decf (rl-y ball-position) 2)))

  (rl-begin-drawing)
  (rl-clear-background rl-darkgray)
  (rl-draw-text "move the ball with arrow keys" 10 10 20 rl-lightgray)
  (rl-draw-circle-v ball-position 50.0 rl-maroon)
  (rl-end-drawing))

(rl-run-mainloop #'my-mainloop)
