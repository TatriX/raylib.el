(add-to-list 'load-path (file-name-directory (or load-file-name (buffer-file-name))))
(toggle-debug-on-error)
(find-file "examples/core/core-2d-camera-mouse-zoom.el")
(eval-buffer)
