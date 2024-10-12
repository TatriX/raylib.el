;;; Based on https://github.com/raysan5/raylib/blob/5.0/examples/core/core_basic_window.c
(require 'raylib)

(defvar screen-width 800)
(defvar screen-height 450)

(rl-init-window screen-width screen-height "raylib [core] example - basic window")

(defun my-mainloop (dt)
  (rl-begin-drawing)
  (rl-clear-background rl-darkgray)
  (rl-draw-text "Congrats! You created your first window!" 190 200 20 rl-lightgray)
  (rl-end-drawing))

(rl-run-mainloop #'my-mainloop)
