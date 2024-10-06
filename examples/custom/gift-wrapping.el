;; See https://en.wikipedia.org/wiki/Gift_wrapping_algorithm

(require 'raylib)
(require 'cl-lib)

(defvar screen-width 1920)
(defvar screen-height 1080)

(rl-init-window screen-width screen-height "Gift Wrapping")

(defun my-mainloop ()
  (rl-begin-drawing)
  (rl-clear-background rl-darkgray)

  (random "8") ;; set random seed
  (let (points hull (n 12) (radius 12) (leftmost 0))
    (dotimes (i n)
      (push (rl-v2 (random (- screen-width (* 10 radius)))
                   (random (- screen-height (* 10 radius))))
            points))

    (dolist (point points)
      (rl-draw-circle-v point radius rl-white))

    (seq-do-indexed (lambda (point i)
                      (when (< (rl-x point) (rl-x (seq-elt points leftmost)))
                        (setq leftmost i)))
                    (seq-rest points))

    (cl-flet ((counterclockwise (p q r)
                (setq p (seq-elt points p))
                (setq q (seq-elt points q))
                (setq r (seq-elt points r))
                (let ((cross (- (* (- (rl-x r) (rl-x q))
                                   (- (rl-y q) (rl-y p)))
                                (* (- (rl-x q) (rl-x p))
                                   (- (rl-y r) (rl-y q))))))
                  (< cross 0))))
      (cl-loop with p = leftmost
               with q = 0
               do
               (push (seq-elt points p) hull)
               (setq q (% (1+ p) n))
               (dotimes (i n)
                 (when (counterclockwise p i q)
                   (setq q i)))
               (setq p q)
               until (= p leftmost))

      (dolist (point hull)
        (rl-draw-circle-v point (/ radius 2) rl-blue))

      (setq hull (nreverse hull))

      (seq-do-indexed (lambda (point i)
                        (rl-draw-line-v point (seq-elt hull (% (1+ i) (length hull))) rl-blue))
                      hull)))

  (random t) ;; pick a new random seed
  (rl-end-drawing))

(rl-run-mainloop #'my-mainloop)
