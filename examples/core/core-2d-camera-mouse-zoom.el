;;; Based on https://github.com/raysan5/raylib/blob/5.0/examples/core/core_2d_camera_mouse_zoom.c

(require 'raylib)
(require 'raymath)
(require 'cl-lib)

(defvar screen-width 800)
(defvar screen-height 450)

(rl-init-window screen-width screen-height "raylib [core] example - 2d camera mouse zoom")

(defvar camera (make-instance 'rl-camera-2d :zoom 1))

(defun my-mainloop (dt)
  (when (rl-is-mouse-button-down rl-mouse-button-right)
    (with-slots (zoom target) camera
      (let ((delta (rl-get-mouse-delta)))
        (setf delta (rl-vector2-scale delta (/ -1 zoom)))
        (setf target (rl-vector2-add target delta)))))

  (when-let ((wheel (rl-get-mouse-wheel-move)))
    (let ((mouse-world-pos (rl-get-screen-to-world-2d (rl-get-mouse-position) camera))
          (zoom-incremenet 0.125))
      (with-slots (offset target zoom) camera
        (setf offset (rl-get-mouse-position))
        (setf target mouse-world-pos)
        (cl-incf zoom (* wheel zoom-incremenet))
        (when (< zoom zoom-incremenet)
          (setf zoom zoom-incremenet)))))

  ;; TODO wheel move

  (rl-begin-drawing)
  (rl-clear-background rl-black)

  (rl-with-camera-2d camera
    (rl-push-matrix)
    (rl-translatef 0 (* 25 50) 0)
    (rl-rotatef 90 1 0 0)
    (rl-draw-grid 100 50)
    (rl-pop-matrix)

    (rl-draw-circle 100 100 50 rl-yellow))

  (rl-draw-text "Mouse right button drag to move, mouse wheel to zoom" 10 10 20 rl-white)

  (rl-end-drawing))

(rl-run-mainloop #'my-mainloop)

;; (rl-stop-mainloop)
;; (my-mainloop rl-dt)
