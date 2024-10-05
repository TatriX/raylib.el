(require 'raylib)

(defvar screen-width 1920)
(defvar screen-height 1080)

(rl-init-window screen-width screen-height "Emacs ❤ Raylib")
; (rl-set-target-fps 60)

(defvar dt (/ 1.0 60))

(defun my-mainloop ()
  (rl-begin-drawing)
  (rl-clear-background rl-darkgray)

  (let ((x (+ (/ screen-width 2) (* 1000 (cos (* 3 (rl-get-time))) dt)))
        (y (+ (/ screen-height 2) (* 1000 (sin (* 3 (rl-get-time))) dt))))
    (rl-draw-circle (float x) (float y) (float 100) rl-yellow))

  (rl-end-drawing))

(setq my-mainloop-timer (run-at-time t dt #'my-mainloop))

; (cancel-timer my-mainloop-timer)

; (rl-close-window)
