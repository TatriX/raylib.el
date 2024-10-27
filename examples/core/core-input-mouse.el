;;; Based on https://github.com/raysan5/raylib/blob/5.0/examples/core/core_input_mouse.c
(require 'raylib)
(require 'cl-lib)

(defvar screen-width 800)
(defvar screen-height 450)

(rl-init-window screen-width screen-height "raylib [core] example - mouse input")

(defvar ball-position (rl-v2 -100 -100))
(defvar ball-color rl-darkblue)

(defun my-mainloop (dt)
  (setq ball-position (rl-get-mouse-position))
  (cond
   ((rl-is-mouse-button-pressed rl-mouse-button-left) (setq ball-color rl-maroon))
   ((rl-is-mouse-button-pressed rl-mouse-button-middle) (setq ball-color rl-lime))
   ((rl-is-mouse-button-pressed rl-mouse-button-right) (setq ball-color rl-darkblue))
   ((rl-is-mouse-button-pressed rl-mouse-button-side) (setq ball-color rl-purple))
   ((rl-is-mouse-button-pressed rl-mouse-button-forward) (setq ball-color rl-orange))
   ((rl-is-mouse-button-pressed rl-mouse-button-back) (setq ball-color rl-beige)))

  (rl-begin-drawing)
  (rl-clear-background rl-white)
  (rl-draw-circle-v ball-position 40 ball-color)
  (rl-draw-text "move ball with mouse and click mouse button to change color" 10 10 20 rl-darkgray)

  (rl-end-drawing))

(rl-run-mainloop #'my-mainloop)
