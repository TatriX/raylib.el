(require 'raylib)

(defvar screen-width (/ (x-display-pixel-width) 2))
(defvar screen-height (/ (x-display-pixel-height) 2))

(rl-init-window screen-width screen-height "Emacs ❤ Raylib")

(defun my-mainloop (dt)
  (rl-begin-drawing)
  (rl-clear-background rl-darkgray)

  (let ((x (+ (/ screen-width 2) (* 1000 (cos (* 3 (rl-get-time))) dt)))
        (y (+ (/ screen-height 2) (* 1000 (sin (* 3 (rl-get-time))) dt))))
    (rl-draw-circle x y 100 rl-yellow))
  ;; (user-error "oops")

  (rl-draw-fps 10 10)
  (rl-end-drawing))

(rl-run-mainloop #'my-mainloop)

(when nil
 (rl-close-window))
