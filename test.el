(add-to-list 'load-path (file-name-directory (or load-file-name (buffer-file-name))))
(find-file "examples/test.el")
(eval-buffer)
