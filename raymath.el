(require 'raylib)

(defun rl-vector2-scale (v scale)
  (rl-vector2 (* (rl-x v) scale)
              (* (rl-y v) scale)))

(defun rl-vector2-add (v add)
  (rl-vector2 (+ (rl-x v) (rl-x add))
              (+ (rl-y v) (rl-y add))))

(provide 'raymath)
