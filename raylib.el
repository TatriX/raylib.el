;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'eieio)

(load (concat "raylib.el" module-file-suffix))

;; Colors
(defconst rl-lightgray [200 200 200 255])
(defconst rl-gray [130 130 130 255])
(defconst rl-darkgray [80 80 80 255])
(defconst rl-yellow [253 249 0 255])
(defconst rl-gold [255 203 0 255])
(defconst rl-orange [255 161 0 255])
(defconst rl-pink [255 109 194 255])
(defconst rl-red [230 41 55 255])
(defconst rl-maroon [190 33 55 255])
(defconst rl-green [0 228 48 255])
(defconst rl-lime [0 158 47 255])
(defconst rl-darkgreen [0 117 44 255])
(defconst rl-skyblue [102 191 255 255])
(defconst rl-blue [0 121 241 255])
(defconst rl-darkblue [0 82 172 255])
(defconst rl-purple [200 122 255 255])
(defconst rl-violet [135 60 190 255])
(defconst rl-darkpurple [112 31 126 255])
(defconst rl-beige [211 176 131 255])
(defconst rl-brown [127 106 79 255])
(defconst rl-darkbrown [76 63 47 255])

(defconst rl-white [255 255 255 255])
(defconst rl-black [0 0 0 255])
(defconst rl-blank [0 0 0 0])
(defconst rl-magenta [255 0 255 255])
(defconst rl-raywhite [245 245 245 255])

;; Keys, Mouse buttons etc
(require 'rl-const)

;; non-raylib stuff
(defvar rl-dt (/ 1.0 60))

(defvar rl-mainloop-function nil)

(declare-function rl-close-window  "raylib")
(declare-function rl-window-should-close  "raylib")
(declare-function rl-get-frame-time  "raylib")

(defun rl-run-mainloop (function &optional fps)
  (when fps
    (setq rl-dt (/ 1.0 fps)))

  (setq rl-mainloop-function
        (lambda ()
          (let ((t0 (float-time)))
            (if (rl-window-should-close)
                (rl-close-window)
              (condition-case err (funcall function rl-dt)
                (error (message "raylib caught error: %s" err)))

              ;; NOTE: emacs can only call us while it's idle, and if we use
              ;; (rl-get-frame-time) here animations become very jittery.
              ;; So we pretend that we run with fixed timestep here.
              (when rl-mainloop-function
                (let ((time (max 0 (- rl-dt (- (float-time) t0)))))
                  (run-at-time time nil rl-mainloop-function)))))))

  (funcall rl-mainloop-function))

(defun rl-stop-mainloop ()
  (setq rl-mainloop-function nil))

(defun rl-vector2 (x y)
  "Return a Vector2"
  (vector x y))

(defun rl-color (r g b a)
  "Return a Color."
  (vector r g b a))

(defun rl-rectangle (x y with height)
  "Return a Rectangle."
  (vector x y with height))

;; TODO: use (define-inline)
(defmacro rl-x (v)
  `(aref ,v 0))

(defmacro rl-y (v)
  `(aref ,v 1))

(defmacro rl-width (v)
  `(aref ,v 2))

(defmacro rl-height (v)
  `(aref ,v 3))

(defclass rl-camera-2d ()
  ((offset :initarg :offset
           :initform '[0 0])
   (target :initarg :target
           :initform '[0 0])
   (rotation :initarg :rotation
             :initform 0)
   (zoom :initarg :zoom
         :initform 1)))

(defmacro rl-with-camera-2d (camera &rest body)
  (declare (indent 1))
  `(progn
     (rl-begin-mode-2d ,camera)
     ,@body
     (rl-end-mode-2d)))

(defun rl--debug-reload-module ()
  (let* ((module (concat "raylib.el" module-file-suffix))
        (tmpfile (make-temp-file (file-name-nondirectory module) nil module-file-suffix)))
    (message "Reloading raylib from temp file %s" tmpfile)
    (copy-file module tmpfile t)
    (module-load tmpfile)))

(provide 'raylib)
